/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#pragma once

#include <cstdint>
#include <string>

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

} // namespace Siren
