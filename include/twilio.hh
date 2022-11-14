#pragma once

#include <string>
#include "type_conversion.hh"

namespace twilio {

class Twilio {
public:
	Twilio(std::string const& account_sid_in, 
		std::string const& auth_token_in,
		std::string const& from_number_in)
		: account_sid(account_sid_in)
		, auth_token(auth_token_in)
		, from_number(from_number_in)
	{}
	// Nothing in destructor
	~Twilio() = default;

	bool send_message(
		std::string const& to_number,
		std::string const& message_body,
		std::string& response,
		std::string const& picture_url = "",
		bool verbose = false
	);

	std::string account_balance();

	bool can_authenticate();
	
	bool can_connect();

private:
	// Account SID and Auth Token come from the Twilio console.
	// See: https://twilio.com/console for more.

	// Used for the username of the auth header
	std::string const account_sid;
	// Used for the password of the auth header
	std::string const auth_token;
	// From number
	std::string const from_number;

	// Portably ignore curl response
	static size_t _null_write(char *, size_t, size_t, void *);
	// Write curl response to a stringstream
	static size_t _stream_write(char *, size_t, size_t, void *);
};

} // end namespace twilio
