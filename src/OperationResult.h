/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#pragma once

#include <optional>
#include <string>

/**
 * @brief The result of an operation which may fail, containing an optional error message.
 *
 * The operation is considered to have been successful if no error message is set.
 */
class OperationResult {
	private:
		std::optional<std::string> error_message = std::nullopt;
	public:
		void setError(const std::string& message = "Unknown error") { error_message = message; }

		/**
		 * @brief Checks if the operation failed.
		 * 
		 * @return true if the operation failed, false otherwise.
		 */
		bool error() const { return error_message.has_value(); }

		/**
		 * @brief Checks if the operation was successful.
		 * 
		 * @return true if the operation was successful, false otherwise.
		 */
		bool success() const { return !error_message.has_value(); }

		/**
		 * @brief Gets the error message associated with the operation result.
		 * 
		 * @return std::string The error message if the operation failed, or "No error" if it was successful.
		 */
		std::string getError() const { return error_message.value_or("No error"); }

		virtual ~OperationResult() = default;
		OperationResult() = default;
		OperationResult(const OperationResult&) = default;
		OperationResult& operator=(const OperationResult&) = default;
		OperationResult(OperationResult&&) = default;
		OperationResult& operator=(OperationResult&&) = default;
};
