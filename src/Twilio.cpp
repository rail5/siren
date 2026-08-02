/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include "Twilio.h"
#include "WebConnector.h"

#include <nlohmann/json.hpp>
#include <curl/curl.h>

#include <algorithm>
#include <fstream>

namespace Siren::Twilio {

std::string Twilio::normalizePhoneNumber(const std::string& phone_number) {
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

std::vector<std::u8string> Twilio::extractUTF8Characters(const std::u8string& message_body) {
	std::vector<std::u8string> utf8_characters;
	for (std::size_t i = 0; i < message_body.size(); ) {
		unsigned char c = message_body[i];
		std::size_t char_length = 0;
		if (c <= 0x7F) {
			char_length = 1;
		} else if ((c & 0xE0) == 0xC0) {
			char_length = 2;
		} else if ((c & 0xF0) == 0xE0) {
			char_length = 3;
		} else if ((c & 0xF8) == 0xF0) {
			char_length = 4;
		} else {
			// Invalid UTF-8 character, skip it
			i++;
			continue;
		}
		if (i + char_length > message_body.size()) {
			// Invalid UTF-8 character, skip it
			break;
		}
		utf8_characters.push_back(message_body.substr(i, char_length));
		i += char_length;
	}

	return utf8_characters;
}

std::u8string Twilio::normalizeMessageBody(const std::u8string& message_body) {
	// Remove all non-GSM characters from the message body
	// See: https://en.wikipedia.org/wiki/GSM_03.38
	// The presence of non-GSM characters can multiply message cost by 3 or more
	//
	// Where possible, we will replace non-GSM characters with their GSM equivalents,
	// but if no equivalent exists, we will remove the character entirely.

	std::u8string normalized;

	// 1. Collect UTF-8 characters from the message body
	const auto utf8_characters = extractUTF8Characters(message_body);

	// 2. For each UTF-8 character, check if it's a GSM character or has a GSM equivalent
	for (const auto& utf8_char : utf8_characters) {
		if (std::ranges::contains(valid_gsm_characters, utf8_char)) {
			normalized += utf8_char;
		} else {
			const auto* it = std::ranges::find_if(nongsm_to_gsm_equivalent, [&utf8_char](const auto& pair) {
				return pair.first == utf8_char;
			});
			if (it != nongsm_to_gsm_equivalent.end()) normalized += it->second;
			// If no GSM equivalent exists, we simply skip the character
		}
	}

	return normalized;
}

TenThousandthOfADollar Twilio::getMessageCost(const std::u8string& message_body) {
	// Calculate the cost of sending a message based on its length
	// We assume that the message body has already been normalized to GSM characters

	// The rule:
	// 1. If the message body is 160 characters or less, it costs $0.0083 per recipient
	// 2. If the message body is more than 160 characters, it is split into segments of 153 characters each,
	//    and each segment costs $0.0083 per recipient

	const auto utf8_characters = extractUTF8Characters(message_body);

	// BY LAW, we MUST append unsubscribe instructions to the end of every message, so we must account for that in the cost calculation
	// Siren adds a line break and the text "(stop=quit)" to the end of every message

	const std::u8string unsubscribe_instructions = u8"\n(stop=quit)";
	
	const auto full_message_length = utf8_characters.size() + unsubscribe_instructions.size();

	if (full_message_length <= 160) return 83;

	const std::size_t number_of_segments = (full_message_length + 152) / 153; // Round up to the nearest segment
	return static_cast<TenThousandthOfADollar>(number_of_segments * 83);
}

bool Twilio::canConnect() {
	return getWebConnector().sendGETRequest("https://api.twilio.com/2010-04-01/").successfullyConnected();
}

bool Twilio::canAuthenticate() {
	// Try to get the account balance with the provided credentials. If the HTTP status code is 200, then we are authenticated.
	WebResult response = getWebConnector().sendGETRequest(
		"https://api.twilio.com/2010-04-01/Accounts/" + account_sid + "/Balance.json",
		account_sid,
		auth_token
	);
	return response.success();
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
			if (normalizePhoneNumber(number["phone_number"].get<std::string>()) == normalizePhoneNumber(from_number)) return true;
		}
	} catch (const nlohmann::json::exception&) {
		return false;
	}
	return false;
}

std::string Twilio::getAccountBalance() {
	WebResult response = getWebConnector().sendGETRequest(
		"https://api.twilio.com/2010-04-01/Accounts/" + account_sid + "/Balance.json",
		account_sid,
		auth_token
	);
	if (!response.success()) return "0";
	try {
		nlohmann::json data = nlohmann::json::parse(response.getResponseBody());
		if (!data.contains("balance") || data["balance"].is_null()) return "0";
		if (data["balance"].is_string()) return data["balance"].get<std::string>();
		if (data["balance"].is_number_float()) return std::to_string(data["balance"].get<double>());
		if (data["balance"].is_number_integer()) return std::to_string(data["balance"].get<std::int64_t>());
		return "0";
	} catch (const nlohmann::json::exception&) {
		return "0";
	}
}

std::set<std::string> Twilio::getUnsubscribedNumbers() {
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
	std::set<std::string> numbers;
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
						numbers.insert(normalizePhoneNumber(number["to"].get<std::string>()));
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

TwilioResult Twilio::sendMessage(
	const std::string& to_number,
	const std::u8string& message_body,
	const std::string& picture_url
) {
	// Send a message via the Twilio API
	// Returns a TwilioResponse object which contains the HTTP status code and any error messages

	// Normalize the phone number and message body
	std::string normalized_to_number = normalizePhoneNumber(to_number);
	std::u8string normalized_message_body = normalizeMessageBody(message_body);

	{
		const auto utf8_characters = extractUTF8Characters(normalized_message_body);
		if (utf8_characters.size() > 1600) {
			TwilioResult response;
			response.setError("Message body must have 1600 or fewer characters. Cannot send message with " + std::to_string(utf8_characters.size()) + " characters.");
			return response;
		}
	}

	// Escape the message body for URL encoding
	CURL* curl = curl_easy_init();
	if (!curl) {
		TwilioResult response;
		response.setError("Failed to initialize CURL for message sending.");
		return response;
	}
	char* escaped_message_body = curl_easy_escape(curl, reinterpret_cast<const char*>(normalized_message_body.c_str()), 0);
	if (!escaped_message_body) {
		curl_easy_cleanup(curl);
		TwilioResult response;
		response.setError("Failed to escape message body for URL encoding.");
		return response;
	}
	curl_easy_cleanup(curl);

	std::string post_data = "To=" + normalized_to_number + "&From=" + from_number + "&Body=" + escaped_message_body;
	if (!picture_url.empty()) post_data += "&MediaUrl=" + picture_url;

	auto response = getWebConnector().sendPOSTRequest(
		"https://api.twilio.com/2010-04-01/Accounts/" + account_sid + "/Messages.json",
		post_data,
		account_sid,
		auth_token
	);

	curl_free(escaped_message_body);

	return response;
}


bool Twilio::parseConfigFile(const std::filesystem::path& config_file_path) {
	// Parse the JSON config file and set the account_sid, auth_token, and from_number
	try {
		std::ifstream config_file(config_file_path);
		if (!config_file.is_open()) return false;
		nlohmann::json config_json;
		config_file >> config_json;
		if (!config_json.contains("account_sid") || !config_json.contains("auth_token") || !config_json.contains("from_number")) return false;
		account_sid = config_json["account_sid"].get<std::string>();
		auth_token = config_json["auth_token"].get<std::string>();
		from_number = normalizePhoneNumber(config_json["from_number"].get<std::string>());
		return true;
	} catch (const std::exception&) {
		return false;
	}
}

void Twilio::saveConfigFile(const std::filesystem::path& config_file_path) const {
	// Save the account_sid, auth_token, and from_number to a JSON config file
	nlohmann::json config_json;
	config_json["account_sid"] = account_sid;
	config_json["auth_token"] = auth_token;
	config_json["from_number"] = from_number;
	std::ofstream config_file(config_file_path);
	config_file << config_json.dump(4);
}

bool Twilio::parseOldStyleConfigFile(const std::filesystem::path& config_file_path) {
	/*
	Example of old-style XML config file format:

<?xml version="1.0" encoding="utf-8"?>
<CONFIG>
  <TApplication>
    <Form1 TWAccountID="{account sid}" TWAuthToken="{account auth token}" TWFromNumber="{from number}"/>
  </TApplication>
</CONFIG>
	*/

	// We won't use a full XML parser.
	// We're just going to search for TWAccountID, TWAuthToken, and TWFromNumber in the file and extract their values.
	try {
		std::ifstream config_file(config_file_path);
		if (!config_file.is_open()) return false;
		std::string line;

		std::string retrieved_id, retrieved_token, retrieved_number;
		while (std::getline(config_file, line)) {
			auto account_id_pos = line.find("TWAccountID=\"");
			if (account_id_pos != std::string::npos) {
				account_id_pos += 13; // Move past TWAccountID="
				auto account_id_end = line.find('\"', account_id_pos);
				if (account_id_end != std::string::npos) {
					retrieved_id = line.substr(account_id_pos, account_id_end - account_id_pos);
				}
			}
			auto auth_token_pos = line.find("TWAuthToken=\"");
			if (auth_token_pos != std::string::npos) {
				auth_token_pos += 13; // Move past TWAuthToken="
				auto auth_token_end = line.find('\"', auth_token_pos);
				if (auth_token_end != std::string::npos) {
					retrieved_token = line.substr(auth_token_pos, auth_token_end - auth_token_pos);
				}
			}
			auto from_number_pos = line.find("TWFromNumber=\"");
			if (from_number_pos != std::string::npos) {
				from_number_pos += 14; // Move past TWFromNumber="
				auto from_number_end = line.find('\"', from_number_pos);
				if (from_number_end != std::string::npos) {
					retrieved_number = line.substr(from_number_pos, from_number_end - from_number_pos);
				}
			}
		}

		if (retrieved_id.empty() || retrieved_token.empty() || retrieved_number.empty()) return false;

		account_sid = retrieved_id;
		auth_token = retrieved_token;
		from_number = retrieved_number;

		// Save to a new-style JSON config file for future use
		std::filesystem::path new_config_file_path = config_file_path.parent_path() / ".siren-config";
		saveConfigFile(new_config_file_path);

		return true;
	} catch (const std::exception&) {
		return false;
	}
}

/**
 * @brief Load settings from a config file. The config file can be either JSON or XML format.
 * 
 * @param config_file_path The path to the config file. If the extension is .xml, we assume it's an old-style XML config file. Otherwise, we assume it's a JSON config file.
 * @return true if the settings were successfully loaded, false otherwise.
 */
bool Twilio::loadSettingsFromConfigFile(const std::filesystem::path& config_file_path) {
	// If the extension is .xml, we assume it's an old-style config file and parse it accordingly.
	if (config_file_path.extension() == ".xml") {
		if (!parseOldStyleConfigFile(config_file_path)) return false;
	} else {
		// Otherwise, we assume it's a JSON config file.
		if (!parseConfigFile(config_file_path)) return false;
	}

	return true;
}

} // namespace Siren::Twilio
