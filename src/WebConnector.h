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

/**
 * @brief The result of a web request, containing the response body, HTTP status code, and connection status.
 * 
 */
class WebResult : public OperationResult {
	private:
		std::optional<std::string> response_body = std::nullopt;
		std::uint32_t http_status_code = 0;
		bool connected = false;
	public:
		void setResponseBody(const std::string& body) { response_body = body; }
		void setHttpStatusCode(std::uint32_t code) { http_status_code = code; }
		void setConnected(bool conn) { connected = conn; }

		/**
		 * @brief Get the response body of the web request.
		 * 
		 * @return std::string The response body as a string. If no response body was returned, an empty string is returned.
		 */
		std::string getResponseBody() const { return response_body.value_or(""); }

		/**
		 * @brief Whether the server returned a response body for the web request.
		 * 
		 * @return true if the server returned a response body, false otherwise.
		 */
		bool hasResponseBody() const { return response_body.has_value(); }

		/**
		 * @brief Gets the HTTP status code of the web request.
		 * 
		 * @return std::uint32_t The HTTP status code (e.g., 200 for OK, 404 for Not Found).
		 */
		std::uint32_t getHttpStatusCode() const { return http_status_code; }

		/**
		 * @brief Whether the web request successfully connected to the server.
		 * 
		 * @return true if the connection was successful, false otherwise.
		 */
		bool successfullyConnected() const { return connected; }
};

/**
 * @brief A singleton class for sending HTTP GET and POST requests, optionally with basic authentication.
 * 
 */
class WebConnector final {
	private:
		friend WebConnector& getWebConnector();

		/**
		 * @brief Construct the singleton WebConnector instance. This constructor is private to enforce the singleton pattern.
		 * When the constructor is called, it initializes the underlying CURL library for making HTTP requests.
		 * This global CURL initialization is the reason that the WebConnector class is a singleton, as CURL should only be initialized once per application.
		 */
		WebConnector();

		/**
		 * @brief Destroy the singleton WebConnector instance. This destructor is private to enforce the singleton pattern.
		 * When the destructor is called, it cleans up the underlying CURL library, releasing any resources allocated during initialization.
		 */
		~WebConnector();
	public:
		WebConnector(const WebConnector&) = delete;
		WebConnector& operator=(const WebConnector&) = delete;
		WebConnector(WebConnector&&) = delete;
		WebConnector& operator=(WebConnector&&) = delete;

		/**
		 * @brief Sends an HTTP GET request to the specified URL, optionally with basic authentication.
		 * 
		 * @param url The URL to send the GET request to.
		 * @param username The optional username for basic authentication. If not provided, no authentication is used.
		 * @param password The optional password for basic authentication. If not provided, no authentication is used.
		 * @return WebResult The result of the web request, containing the response body, HTTP status code, and connection status.
		 */
		WebResult sendGETRequest(
			const std::string& url,
			const std::optional<std::string>& username = std::nullopt,
			const std::optional<std::string>& password = std::nullopt
		);

		/**
		 * @brief Sends an HTTP POST request to the specified URL with the provided data, optionally with basic authentication.
		 * 
		 * @param url The URL to send the POST request to.
		 * @param post_data The data to include in the POST request body.
		 * @param username The optional username for basic authentication. If not provided, no authentication is used.
		 * @param password The optional password for basic authentication. If not provided, no authentication is used.
		 * @return WebResult The result of the web request, containing the response body, HTTP status code, and connection status.
		 */
		WebResult sendPOSTRequest(
			const std::string& url,
			const std::string& post_data,
			const std::optional<std::string>& username = std::nullopt,
			const std::optional<std::string>& password = std::nullopt
		);
};

} // namespace Siren::Twilio
