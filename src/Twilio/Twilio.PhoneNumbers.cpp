/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include "../Twilio.h"
#include "../WebConnector.h"

#include <nlohmann/json.hpp>

namespace Siren::Twilio {

std::string PhoneNumber::normalize(std::string_view phone_number) {
	// Normalize a number to (something like) E.164 format
	// We bias ourselves towards US numbers

	std::string normalized;

	// 1. Erase all characters which are not digits or a leading '+'
	for (std::size_t i = 0; i < phone_number.size(); i++) {
		switch (phone_number[i]) {
			case '+':
				if (i == 0) normalized += '+';
				break;
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				normalized += phone_number[i];
				break;
			default:
				// Ignore all other characters
				break;
		}
	}

	// 2. If the number starts with a '+', we assume it's already in E.164 format
	if (!normalized.empty() && normalized[0] == '+') return normalized;

	// 3. BUT, under some circumstances we can prepend '+1' or '+' if this looks like a US number
	// I.e.:
	//  Case 1:
	//   The number starts with a '1' and is 11 digits long, we assume it's a US number and prepend '+'
	//  Case 2:
	//   The number is 10 digits long, we assume it's a US number and prepend '+1'

	if (normalized.size() == 11 && normalized[0] == '1') {
		return "+" + normalized;
	} else if (normalized.size() == 10) {
		return "+1" + normalized;
	}

	// 4. If we don't recognize the number as a US number, we just return it as-is
	return normalized;
}

bool Twilio::fromNumberIsValid() {
	// Query the Twilio API: List all IncomingPhoneNumbers for this account,
	// and verify that the from_number is in the list of numbers returned.
	WebResult response = getWebConnector().sendGETRequest(
		"https://api.twilio.com/2010-04-01/Accounts/" + account_sid + "/IncomingPhoneNumbers.json",
		account_sid,
		auth_token
	);
	if (!response.success()) return false;
	try {
		nlohmann::json data = nlohmann::json::parse(response.getResponseBody());
		if (!data.contains("incoming_phone_numbers")) return false;
		for (const auto& number : data["incoming_phone_numbers"]) {
			if (!number.contains("phone_number") || number["phone_number"].is_null() || !number["phone_number"].is_string()) continue;
			PhoneNumber api_number(number["phone_number"].get<std::string>());
			if (api_number == from_number) return true;
		}
	} catch (const nlohmann::json::exception&) {
		return false;
	}
	return false;
}

std::set<PhoneNumber> Twilio::getUnsubscribedNumbers() {
	// Query api.twilio.com/2010-04-01/Accounts/{AccountSID}/Messages.json?Status=failed&PageSize={arbitrary size}
	// Then, repeatedly follow the "next_page_uri" field until we've read all there is
	//
	// If the error code associated with a message is:
	//  - 21610 (Unsubscribed)
	//  - 30006 (Absent Subscriber)
	//  - 21211 (Invalid 'To' Phone Number)
	// Then we'll count it and return it in this set
	//
	// Note: this function should be run ASYNCHRONOUSLY, as it may take a while to complete if there are many messages to process.
	//
	// Unfortunately, Twilio does not provide a direct API to get the list of unsubscribed numbers,
	// so we have to infer it from the failed messages.
	// It would be MUCH faster if we could just get the list directly. I have no idea why Twilio doesn't provide this.
	std::set<PhoneNumber> numbers;
	std::string next_page_uri = "/2010-04-01/Accounts/" + account_sid + "/Messages.json?Status=failed&PageSize=5000";
	while (!next_page_uri.empty()) {
		WebResult response = getWebConnector().sendGETRequest(
			"https://api.twilio.com" + next_page_uri,
			account_sid,
			auth_token
		);
		if (!response.success()) break;
		try {
			nlohmann::json data = nlohmann::json::parse(response.getResponseBody());
			if (!data.contains("messages")) break;

			for (auto& number : data["messages"]) {
				if (!number.contains("error_code") || number["error_code"].is_null() || !number["error_code"].is_number_integer()) continue;
				int error_code = number["error_code"].get<int>();
				if (error_code == 21610 || error_code == 30006 || error_code == 21211) {
					if (number.contains("to") && !number["to"].is_null() && number["to"].is_string()) {
						PhoneNumber phone_number(number["to"].get<std::string>());
						numbers.insert(phone_number);
					}
				}
			}

			next_page_uri = data.value("next_page_uri", "");
		} catch (const nlohmann::json::exception&) {
			break;
		}
	}

	return numbers;
}

} // namespace Siren::Twilio
