/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include "../Twilio.h"

#include <nlohmann/json.hpp>

#include <filesystem>
#include <fstream>

namespace Siren::Twilio {

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
