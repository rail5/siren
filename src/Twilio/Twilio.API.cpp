/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include "../Twilio.h"
#include "../WebConnector.h"

#include <nlohmann/json.hpp>

namespace Siren::Twilio {

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

} // namespace Siren::Twilio
