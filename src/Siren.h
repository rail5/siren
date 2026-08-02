/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#pragma once

#include <cstdint>
#include <string>
#include <filesystem>

#ifdef _WIN32
#include <windows.h>
#include <shlobj.h>
#else
#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>
#include <vector>
#endif

namespace Siren {

using TenThousandthOfADollar = std::uint64_t;

/**
 * @brief Formats a TenThousandthOfADollar value into a human-readable string.
 * 
 * @param cost The cost in ten-thousandths of a dollar.
 * @return std::string The formatted cost as a string in the format "D.CCtt", where D is dollars, CC is cents, and tt is ten-thousandths of a dollar.
 */
inline std::string formatCost(TenThousandthOfADollar cost) {
	std::uint64_t dollars = cost / 10000;
	std::uint64_t cents = (cost % 10000) / 100;
	std::uint64_t tenThousandths = cost % 100;

	std::string result = std::to_string(dollars) + ".";

	if (cents < 10) result += "0";
	result += std::to_string(cents);

	if (tenThousandths < 10) result += "0";
	result += std::to_string(tenThousandths);

	return result;
}

/**
 * @brief Get the home directory of the current user in a cross-platform manner.
 * 
 * @return std::filesystem::path The path to the home directory.
 * @throws std::runtime_error If the home directory cannot be determined.
 */
inline std::filesystem::path getHomeDirectory() {
	#ifdef _WIN32
		PWSTR path = nullptr;
		HRESULT hr = SHGetKnownFolderPath(FOLDERID_Profile, 0, nullptr, &path);
		if (FAILED(hr)) {
			throw std::runtime_error("Failed to get home directory on Windows.");
		}
		std::filesystem::path homeDir(path);
		CoTaskMemFree(path);
		return homeDir;
	#else
		// Unix-like systems (Linux, macOS, etc.)

		// The "thread-unsafe" warning regarding std::getenv is a false positive
		// (It's only "thread-unsafe" if we're *modifying* the environment variable. This is just a read.)
		//NOLINTNEXTLINE(concurrency-mt-unsafe)
		if (const char* home = std::getenv("HOME")) {
			return {home};
		}
		std::vector<char> buffer(4097);
		passwd pwd = {};
		passwd* result = nullptr;
		if (getpwuid_r(getuid(), &pwd, buffer.data(), buffer.size(), &result) == 0 && result) {
			return {pwd.pw_dir};
		}

		throw std::runtime_error("Failed to get home directory on Unix-like system.");
	#endif

}

} // namespace Siren
