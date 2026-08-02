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

#include "Siren.h"
#include "OperationResult.h"

namespace Siren::Twilio {

using TwilioResult = OperationResult;

class Twilio final {
	private:
		std::string account_sid;
		std::string auth_token;
		std::string from_number;

	public:
		Twilio(const std::string& account_sid, const std::string& auth_token, const std::string& from_number)
			: account_sid(account_sid), auth_token(auth_token), from_number(normalizePhoneNumber(from_number)) {}
		Twilio() = delete;
		~Twilio() = default;

		Twilio(const Twilio&) = delete;
		Twilio& operator=(const Twilio&) = delete;
		Twilio(Twilio&&) = delete;
		Twilio& operator=(Twilio&&) = delete;

		TwilioResult sendMessage(
			const std::string& to_number,
			const std::u8string& message_body,
			const std::string& picture_url = ""
		);

		std::string getAccountBalance();
		std::set<std::string> getUnsubscribedNumbers();
		bool canAuthenticate();
		bool fromNumberIsValid();
		bool canConnect();

		static TenThousandthOfADollar getMessageCost(const std::u8string& message_body);
		static std::string normalizePhoneNumber(const std::string& phone_number);
		static std::u8string normalizeMessageBody(const std::u8string& message_body);
		static std::vector<std::u8string> extractUTF8Characters(const std::u8string& message_body);

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
