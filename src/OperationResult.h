/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#pragma once

#include <optional>
#include <string>

class OperationResult {
	private:
		std::optional<std::string> error_message = std::nullopt;
	public:
		void setError(const std::string& message = "Unknown error") { error_message = message; }

		bool error() const { return error_message.has_value(); }
		bool success() const { return !error_message.has_value(); }
		std::string getError() const { return error_message.value_or("No error"); }

		virtual ~OperationResult() = default;
		OperationResult() = default;
		OperationResult(const OperationResult&) = default;
		OperationResult& operator=(const OperationResult&) = default;
		OperationResult(OperationResult&&) = default;
		OperationResult& operator=(OperationResult&&) = default;
};
