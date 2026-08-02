/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#pragma once


#include <string>
#include <optional>
#include <cstdint>

#include "OperationResult.h"

namespace Siren::Twilio {

class WebConnector;

WebConnector& getWebConnector();

class WebResult : public OperationResult {
	private:
		std::optional<std::string> response_body = std::nullopt;
		std::uint32_t http_status_code = 0;
		bool connected = false;
	public:
		void setResponseBody(const std::string& body) { response_body = body; }
		void setHttpStatusCode(std::uint32_t code) { http_status_code = code; }
		void setConnected(bool conn) { connected = conn; }
		std::string getResponseBody() const { return response_body.value_or(""); }
		bool hasResponseBody() const { return response_body.has_value(); }
		std::uint32_t getHttpStatusCode() const { return http_status_code; }
		bool successfullyConnected() const { return connected; }
};

class WebConnector final {
	private:
		friend WebConnector& getWebConnector();

		WebConnector();
		~WebConnector();
	public:
		WebConnector(const WebConnector&) = delete;
		WebConnector& operator=(const WebConnector&) = delete;
		WebConnector(WebConnector&&) = delete;
		WebConnector& operator=(WebConnector&&) = delete;

		WebResult sendGETRequest(
			const std::string& url,
			const std::optional<std::string>& username = std::nullopt,
			const std::optional<std::string>& password = std::nullopt
		);

		WebResult sendPOSTRequest(
			const std::string& url,
			const std::string& post_data,
			const std::optional<std::string>& username = std::nullopt,
			const std::optional<std::string>& password = std::nullopt
		);
};

} // namespace Siren::Twilio
