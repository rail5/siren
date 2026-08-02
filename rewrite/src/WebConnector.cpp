/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include "WebConnector.h"

#include <curl/curl.h>

#include <sstream>

namespace Siren::Twilio {

WebConnector& getWebConnector() {
	static WebConnector connector;
	return connector;
}

WebConnector::WebConnector() {
	curl_global_init(CURL_GLOBAL_ALL);
}

WebConnector::~WebConnector() {
	curl_global_cleanup();
}

namespace {

// Helper function to deal with curl's C-style callbacks

// Write curl response to a stringstream
std::size_t _stream_write(char* ptr, std::size_t size, std::size_t nmemb, void* userdata) {
	std::size_t response_size = size * nmemb;
	auto* ss = static_cast<std::stringstream*>(userdata);
	auto write_size = static_cast<std::streamsize>(response_size);
	ss->write(ptr, write_size);
	return response_size;
}

} // anonymous namespace

WebResult WebConnector::sendGETRequest(
	const std::string& url,
	const std::optional<std::string>& username,
	const std::optional<std::string>& password
) {
	WebResult response;

	CURL* curl = curl_easy_init();

	if (!curl) {
		response.setError("Failed to initialize CURL for GET request.");
		return response;
	}

	std::stringstream response_stream;

	curl_easy_setopt(curl, CURLOPT_HTTPGET, 1);
	curl_easy_setopt(curl, CURLOPT_URL, url.c_str());

	if (username.has_value() && password.has_value()) {
		curl_easy_setopt(curl, CURLOPT_USERNAME, username.value().c_str());
		curl_easy_setopt(curl, CURLOPT_PASSWORD, password.value().c_str());
	}

	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, _stream_write);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_stream);

	// timeout: 30s
	curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);

	CURLcode res = curl_easy_perform(curl);

	std::uint32_t http_code = 0;
	curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);
	response.setHttpStatusCode(http_code);

	curl_easy_cleanup(curl);

	if (res == CURLE_OK) response.setConnected(true);

	if (res != CURLE_OK) {
		response.setError(curl_easy_strerror(res));
	} else if (http_code < 200 || http_code >= 300) { // 2xx is success, anything else is an error
		response.setError("HTTP error code: " + std::to_string(http_code));
	}

	response.setResponseBody(response_stream.str());
	return response;
}

WebResult WebConnector::sendPOSTRequest(
	const std::string& url,
	const std::string& post_data,
	const std::optional<std::string>& username,
	const std::optional<std::string>& password
) {
	WebResult response;

	CURL* curl = curl_easy_init();

	if (!curl) {
		response.setError("Failed to initialize CURL for POST request.");
		return response;
	}

	std::stringstream response_stream;

	curl_easy_setopt(curl, CURLOPT_POST, 1);
	curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
	curl_easy_setopt(curl, CURLOPT_POSTFIELDS, post_data.c_str());

	if (username.has_value() && password.has_value()) {
		curl_easy_setopt(curl, CURLOPT_USERNAME, username.value().c_str());
		curl_easy_setopt(curl, CURLOPT_PASSWORD, password.value().c_str());
	}

	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, _stream_write);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_stream);

	// timeout: 30s
	curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);

	CURLcode res = curl_easy_perform(curl);

	std::uint32_t http_code = 0;
	curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);
	response.setHttpStatusCode(http_code);

	curl_easy_cleanup(curl);

	if (res == CURLE_OK) response.setConnected(true);

	if (res != CURLE_OK) {
		response.setError(curl_easy_strerror(res));
	} else if (http_code != 200 && http_code != 201) {
		response.setError("HTTP error code: " + std::to_string(http_code));
	}

	response.setResponseBody(response_stream.str());
	return response;
}

} // namespace Siren::Twilio
