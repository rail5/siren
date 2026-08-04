/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include "../Twilio.h"
#include "../WebConnector.h"

#include <curl/curl.h>

#include <algorithm>
#include <thread>
#include <chrono>

namespace Siren::Twilio {
	
std::pair<std::u8string, std::size_t> TextMessage::normalizeMessageBodyAndCountCharacters(std::u8string_view message_body) {
	// Remove all non-GSM characters from the message body
	// See: https://en.wikipedia.org/wiki/GSM_03.38
	// The presence of non-GSM characters can multiply message cost by 3 or more
	//
	// Where possible, we will replace non-GSM characters with their GSM equivalents,
	// but if no equivalent exists, we will remove the character entirely.

	std::pair<std::u8string, std::size_t> result;

	// 1. Collect UTF-8 characters from the message body
	const auto utf8_characters = extractUTF8Characters(message_body);
	result.second = utf8_characters.size();

	// 2. For each UTF-8 character, check if it's a GSM character or has a GSM equivalent
	for (const auto& utf8_char : utf8_characters) {
		if (std::ranges::contains(valid_gsm_characters, utf8_char)) {
			result.first += utf8_char;
		} else {
			const auto* it = std::ranges::find_if(nongsm_to_gsm_equivalent, [&utf8_char](const auto& pair) {
				return pair.first == utf8_char;
			});
			if (it != nongsm_to_gsm_equivalent.end()) result.first += it->second;
			// If no GSM equivalent exists, we simply skip the character
		}
	}

	return result;
}

std::vector<std::u8string_view> TextMessage::extractUTF8Characters(std::u8string_view message_body) {
	std::vector<std::u8string_view> utf8_characters;
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

TenThousandthOfADollar TextMessage::getCostPerRecipient() const {
	// Calculate the cost of sending a message based on its length
	// We assume that the message body has already been normalized to GSM characters

	// The rule:
	// 1. If the message body is 160 characters or less, it costs Twilio::costPerSegment per recipient
	// 2. If the message body is more than 160 characters, it is split into segments of 153 characters each,
	//    and each segment costs Twilio::costPerSegment per recipient

	const auto utf8_characters = extractUTF8Characters(message_body);

	// BY LAW, we MUST append unsubscribe instructions to the end of every message, so we must account for that in the cost calculation
	// Siren adds a line break and the text "(stop=quit)" to the end of every message

	const std::u8string unsubscribe_instructions = u8"\n(stop=quit)";

	const auto full_message_length = utf8_characters.size() + unsubscribe_instructions.size();

	if (full_message_length <= 160) return Twilio::costPerSegment;

	const std::size_t number_of_segments = (full_message_length + 152) / 153; // Round up to the nearest segment
	return static_cast<TenThousandthOfADollar>(number_of_segments * Twilio::costPerSegment);
}

TwilioResult Twilio::sendMessage(
	const PhoneNumber& to_number,
	const TextMessage& message_body,
	const ListOfPhoneNumbers auto&& unsubscribed_numbers
) {
	if (unsubscribed_numbers.contains(to_number)) {
		TwilioResult response;
		response.setError("The recipient has unsubscribed from receiving messages.");
		return response;
	}

	// Append "\n(stop=quit)" to the message body, as required by law
	std::u8string full_message_body = std::u8string(message_body.getMessageBody()) + u8"\n(stop=quit)";

	// Replace all occurrences of `$name` in full_message_body with to_number.getName()
	const std::u8string name(reinterpret_cast<const char8_t*>(to_number.getName().data()), to_number.getName().size());
	while (true) {
		const auto pos = full_message_body.find(u8"$name");
		if (pos == std::u8string::npos) break;
		full_message_body.replace(pos, 5, name);
	}

	// Verify that the full (expanded) message doesn't exceed 1600 characters (Twilio's maximum message length)
	const auto expanded_length = TextMessage::extractUTF8Characters(full_message_body).size();
	if (expanded_length > 1600) {
		TwilioResult response;
		response.setError("The message body exceeds the maximum length of 1600 characters after expansion.");
		return response;
	}

	// Escape the message body for URL encoding
	CURL* curl = curl_easy_init();
	if (!curl) {
		TwilioResult response;
		response.setError("Failed to initialize CURL for message sending.");
		return response;
	}
	char* escaped_message_body = curl_easy_escape(
		curl,
		reinterpret_cast<const char*>(full_message_body.data()),
		static_cast<int>(full_message_body.size())
	);
	if (!escaped_message_body) {
		curl_easy_cleanup(curl);
		TwilioResult response;
		response.setError("Failed to escape message body for URL encoding.");
		return response;
	}

	char* escaped_picture_url = nullptr;
	if (!message_body.getPictureURL().empty()) {
		escaped_picture_url = curl_easy_escape(
			curl,
			message_body.getPictureURL().data(),
			static_cast<int>(message_body.getPictureURL().size())
		);
		if (!escaped_picture_url) {
			curl_easy_cleanup(curl);
			curl_free(escaped_message_body);
			TwilioResult response;
			response.setError("Failed to escape picture URL for URL encoding.");
			return response;
		}
	}

	curl_easy_cleanup(curl);

	std::string post_data = "To=" + std::string(to_number.getNumber())
		+ "&From=" + std::string(from_number.getNumber())
		+ "&Body=" + escaped_message_body;

	if (escaped_picture_url != nullptr) post_data += "&MediaUrl=" + std::string(escaped_picture_url);

	auto response = getWebConnector().sendPOSTRequest(
		"https://api.twilio.com/2010-04-01/Accounts/" + account_sid + "/Messages.json",
		post_data,
		account_sid,
		auth_token
	);

	// If we receive a 429 Too Many Requests response, we will employ an "exponential backoff" strategy
	// Starting with a 1 second delay, we will double the delay each time we receive a 429 response, up to a maximum of 16 seconds
	// After 5 attempts, we will give up and return the last response received
	std::uint8_t attempt = 1;
	while (response.getHttpStatusCode() == 429 && attempt <= 5) {
		auto delay = 1u << (attempt - 1); // 1, 2, 4, 8, 16 seconds
		std::this_thread::sleep_for(std::chrono::seconds(delay));
		response = getWebConnector().sendPOSTRequest(
			"https://api.twilio.com/2010-04-01/Accounts/" + account_sid + "/Messages.json",
			post_data,
			account_sid,
			auth_token
		);
		attempt++;
	}

	curl_free(escaped_message_body);
	if (escaped_picture_url != nullptr) curl_free(escaped_picture_url);

	// "Slicing object from WebResult to OperationResult discards xx bytes of state"
	// Yes, but those bytes (the derived class's members) are not needed by the caller.
	return response; // NOLINT(cppcoreguidelines-slicing)
}

TwilioResult Twilio::sendMassMessage(
	const ListOfPhoneNumbers auto&& to_numbers,
	const TextMessage& message_body,
	const ListOfPhoneNumbers auto&& unsubscribed_numbers
) {
	TwilioResult final_result;
	std::string concatenated_error_message;
	for (const auto& to_number : to_numbers) {
		auto result = sendMessage(to_number, message_body, unsubscribed_numbers);
		if (!result.success()) concatenated_error_message += std::string(to_number.getNumber()) + ": " + result.getError() + "\n";
	}

	final_result.setError(concatenated_error_message);
	return final_result;
}

} // namespace Siren::Twilio
