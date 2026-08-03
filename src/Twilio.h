/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#pragma once

#include <string>
#include <set>
#include <array>
#include <vector>
#include <string_view>
#include <utility>
#include <filesystem>

#include "Siren.h"
#include "OperationResult.h"

namespace Siren::Twilio {

class PhoneNumber final {
	private:
		std::string phone_number;
		std::string friendly_name;
	public:
		static std::string normalize(std::string_view phone_number);

		PhoneNumber() = default;
		PhoneNumber(std::string_view number, std::string_view name = "") : phone_number(normalize(number)), friendly_name(name) {}

		~PhoneNumber() = default;
		PhoneNumber(const PhoneNumber&) = default;
		PhoneNumber& operator=(const PhoneNumber&) = default;
		PhoneNumber(PhoneNumber&&) = default;
		PhoneNumber& operator=(PhoneNumber&&) = default;

		void setNumber(std::string_view number) { phone_number = normalize(number); }
		std::string_view getNumber() const { return phone_number; }
		void setName(std::string_view name) { friendly_name = name; }
		std::string_view getName() const { return friendly_name; }

		friend bool operator<(const PhoneNumber& lhs, const PhoneNumber& rhs) {
			return lhs.phone_number < rhs.phone_number;
		}
		friend bool operator==(const PhoneNumber& lhs, const PhoneNumber& rhs) {
			return lhs.phone_number == rhs.phone_number;
		}
		friend bool operator!=(const PhoneNumber& lhs, const PhoneNumber& rhs) {
			return !(lhs == rhs);
		}
		friend bool operator>(const PhoneNumber& lhs, const PhoneNumber& rhs) {
			return rhs < lhs;
		}
		friend bool operator<=(const PhoneNumber& lhs, const PhoneNumber& rhs) {
			return !(rhs < lhs);
		}
		friend bool operator>=(const PhoneNumber& lhs, const PhoneNumber& rhs) {
			return !(lhs < rhs);
		}

		operator std::string_view() const { return phone_number; }
		operator std::string() const { return phone_number; }

		// Ensure that PhoneNumber's hash function does NOT depend on the friendly name, only the phone number itself.
		struct Hash {
			std::size_t operator()(const PhoneNumber& number) const {
				return std::hash<std::string>()(number.phone_number);
			}
		};
};

using TwilioResult = OperationResult;

/**
 * @brief A class for interacting with the Twilio API.
 * 
 */
class Twilio final {
	private:
		std::string account_sid;
		std::string auth_token;
		PhoneNumber from_number;

		// JSON config file format:

		/**
		 * @brief Parses a JSON configuration file to load Twilio settings.
		 * 
		 * @param config_file_path The path to the JSON configuration file.
		 * @return true if the settings were successfully loaded, false otherwise.
		 */
		bool parseConfigFile(const std::filesystem::path& config_file_path);

		// XML config file format (was used by the old Pascal version of Siren, but is no longer used)
		// Nevertheless, we still support it for backwards compatibility with old config files,
		// so that when users upgrade they don't need to re-enter their Twilio credentials.
		// But we'll immediately transition them to the new format after loading.

		/**
		 * @brief Parses an old-style XML configuration file to load Twilio settings.
		 * This format was used by the old Pascal version of Siren.
		 * 
		 * @param config_file_path The path to the XML configuration file.
		 * @return true if the settings were successfully loaded, false otherwise.
		 */
		bool parseOldStyleConfigFile(const std::filesystem::path& config_file_path);
	public:

		static constexpr TenThousandthOfADollar costPerSegment = 83; // Twilio charges $0.0083 per segment (as of 2026-08-03)

		Twilio() = default;
		~Twilio() = default;

		Twilio(const Twilio&) = delete;
		Twilio& operator=(const Twilio&) = delete;
		Twilio(Twilio&&) = delete;
		Twilio& operator=(Twilio&&) = delete;

		/**
		 * @brief Loads Twilio settings from a config file. The config file can be either JSON or XML format.
		 * 
		 * @param config_file_path The path to the config file. If the extension is .xml, we assume it's an old-style XML config file. Otherwise, we assume it's a JSON config file.
		 * @return true if the settings were successfully loaded, false otherwise.
		 */
		bool loadSettingsFromConfigFile(const std::filesystem::path& config_file_path);

		/**
		 * @brief Saves the current Twilio settings to a configuration file in JSON format.
		 * If the file already exists, it will be overwritten.
		 * 
		 * @param config_file_path The path to the JSON configuration file.
		 */
		void saveConfigFile(const std::filesystem::path& config_file_path) const;

		void setAccountSID(const std::string& account_sid_in) { account_sid = account_sid_in; }
		void setAuthToken(const std::string& auth_token_in) { auth_token = auth_token_in; }
		void setFromNumber(const PhoneNumber& from_number_in) { from_number = from_number_in; }
		const std::string& getAccountSID() const { return account_sid; }
		const std::string& getAuthToken() const { return auth_token; }
		const PhoneNumber& getFromNumber() const { return from_number; }

		/**
		 * @brief Sends a message using the Twilio API.
		 * This can be either an SMS or MMS message, depending on whether a picture URL is provided.
		 * 
		 * @param to_number The phone number to send the message to.
		 * @param message_body The body of the message.
		 * @param picture_url The URL of the picture to include in the message (optional).
		 * @return TwilioResult The result of the operation.
		 */
		TwilioResult sendMessage(
			const PhoneNumber& to_number,
			const std::u8string& message_body,
			const std::string& picture_url = ""
		);

		/**
		 * @brief Get the current account balance from Twilio. This is the amount of money available for sending messages.
		 * 
		 * @return std::string The account balance as a string, e.g. "10.00". Copied directly from Twilio's API response.
		 */
		std::string getAccountBalance();

		/**
		 * @brief Get the list of unsubscribed phone numbers from Twilio.
		 * These are numbers that have opted out of receiving messages from your Twilio account,
		 * or which are not valid for sending messages.
		 * This list is used to filter out invalid recipients before sending messages.
		 * This function should only ever be called from a background thread, as it may take a long time to complete.
		 * 
		 * @return std::set<PhoneNumber> The set of unsubscribed phone numbers.
		 */
		std::set<PhoneNumber> getUnsubscribedNumbers();

		/**
		 * @brief Check if the current settings contain valid credentials for authenticating with the Twilio API.
		 * 
		 * @return true if authentication is possible, false otherwise.
		 */
		bool canAuthenticate();

		/**
		 * @brief Check if the configured "from" phone number is owned by the Twilio account and is valid for sending messages.
		 * 
		 * @return true if the "from" number is valid, false otherwise.
		 */
		bool fromNumberIsValid();

		/**
		 * @brief Check if the Twilio API is reachable.
		 * 
		 * @return true if the API is reachable, false otherwise.
		 */
		static bool canConnect();

		/**
		 * @brief Approximate the cost of sending a message based on its length and Twilio's pricing rules.
		 * 
		 * @param message_body The body of the message to be sent.
		 * @return TenThousandthOfADollar The cost of sending the message in ten-thousandths of a dollar (e.g. 83 = $0.0083).
		 */
		static TenThousandthOfADollar getMessageCost(std::u8string_view message_body);

		/**
		 * @brief Normalize a message body to ensure it only contains valid GSM characters, replacing or removing invalid characters as necessary.
		 * 
		 * @param message_body The message body to normalize.
		 * @return std::u8string The normalized message body containing only valid GSM characters.
		 */
		static std::u8string normalizeMessageBody(std::u8string_view message_body);

		/**
		 * @brief Extract individual UTF-8 characters from a message body, returning them as a vector of strings.
		 * Each UTF-8 character is represented as a separate string in the vector.
		 * 
		 * @param message_body The message body from which to extract UTF-8 characters.
		 * @return std::vector<std::u8string> A vector containing each UTF-8 character as a separate string.
		 */
		static std::vector<std::u8string_view> extractUTF8Characters(std::u8string_view message_body);

		// Valid characters in the GSM 03.38 character set, which is used for SMS messages.
		// Presence of characters outside of this set can multiply message cost by 3 or more.
		static constexpr std::array<std::u8string_view, 127> valid_gsm_characters = {
			u8"@", u8"£", u8"$", u8"¥",  u8"è", u8"é", u8"ù", u8"ì", u8"ò", u8"Ç",
			u8"\n",u8"Ø", u8"ø", u8"\r", u8"Å", u8"å", u8"Δ", u8"_", u8"Φ", u8"Γ",
			u8"Λ", u8"Ω", u8"Π", u8"Ψ",  u8"Σ", u8"Θ", u8"Ξ",
			u8"Æ", u8"æ", u8"ß", u8"É",  u8" ", u8"!", u8"\"",
			u8"#", u8"¤", u8"%", u8"&",  u8"'", u8"(", u8")", u8"*", u8"+", u8",",
			u8"-", u8".", u8"/", u8"0",  u8"1", u8"2", u8"3", u8"4", u8"5", u8"6", u8"7", u8"8", u8"9",
			u8":", u8";", u8"<", u8"=",  u8">", u8"?", u8"¡", u8"A", u8"B", u8"C", u8"D", u8"E", u8"F",
			u8"G", u8"H", u8"I", u8"J",  u8"K", u8"L", u8"M", u8"N", u8"O", u8"P", u8"Q", u8"R", u8"S",
			u8"T", u8"U", u8"V", u8"W",  u8"X", u8"Y", u8"Z", u8"Ä", u8"Ö", u8"Ñ", u8"Ü", u8"§",
			u8"¿", u8"a", u8"b", u8"c",  u8"d", u8"e", u8"f", u8"g", u8"h", u8"i", u8"j", u8"k", u8"l",
			u8"m", u8"n", u8"o", u8"p",  u8"q", u8"r", u8"s", u8"t", u8"u", u8"v", u8"w", u8"x", u8"y",
			u8"z", u8"ä", u8"ö", u8"ñ",  u8"ü", u8"à"
		};

		// Non-GSM characters which have GSM equivalents, and their replacements.
		// If a non-GSM character is found in a message body, it will be replaced with its GSM equivalent if one exists, or removed entirely if not.
		// See: https://en.wikipedia.org/wiki/GSM_03.38 for more information on the GSM character set
		static constexpr std::array<std::pair<std::u8string_view, std::u8string_view>, 22> nongsm_to_gsm_equivalent = {
			std::make_pair(u8"“", u8"\""),
			std::make_pair(u8"”", u8"\""),
			std::make_pair(u8"‘", u8"'"),
			std::make_pair(u8"’", u8"'"),
			std::make_pair(u8"–", u8"-"),
			std::make_pair(u8"—", u8"-"),
			std::make_pair(u8"…", u8"..."),
			std::make_pair(u8"€", u8"E"),
			std::make_pair(u8"•", u8"*"),
			std::make_pair(u8"~", u8"-"),
			std::make_pair(u8"¬", u8"-"),
			std::make_pair(u8"©", u8"(c)"),
			std::make_pair(u8"®", u8"(R)"),
			std::make_pair(u8"™", u8"(TM)"),
			std::make_pair(u8"°", u8"o"),
			std::make_pair(u8"²", u8"2"),
			std::make_pair(u8"³", u8"3"),
			std::make_pair(u8"¼", u8"1/4"),
			std::make_pair(u8"½", u8"1/2"),
			std::make_pair(u8"¾", u8"3/4"),
			std::make_pair(u8"×", u8"x"),
			std::make_pair(u8"÷", u8"/")
		};
};

} // namespace Siren::Twilio
