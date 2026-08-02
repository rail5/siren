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
