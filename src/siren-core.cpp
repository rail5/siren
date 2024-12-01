#include <iostream>
#include <string>
#include <memory>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "twilio.hh"

using namespace std;

int main(int argc, char * argv[])
{
	int cmd;
	string account_sid = "";
	string auth_token = "";
	string message = "";
	string from_number = "";
	string to_number = "";
	string picture_url = "";
	bool verbose = false;
	bool checkbalance = false;
	bool checkauth = false;
	bool checkconnection = false;
	bool getunsubscribednumbers = false;

	opterr = 0;

	while ((cmd = getopt(argc, argv, "a:s:m:f:t:p:vhbcui?")) != -1) {
		switch (cmd) {
			case '?':
			case 'h':
				printf("Siren (core) 0.1\n");
				printf("Copyright (C) 2022 rail5\n");
				printf("This program comes with ABSOLUTELY NO WARRANTY.\n");
				printf("This is free software (GNU GPL V3), and you are welcome to redistribute it under certain conditions.\n");
				printf("-a: Account\t\t"
				       "(ex: -a \"ACXXXXX\")\n");
				printf("-s: Auth Token\t\t"
				       "(ex: -s \"your_token\")\n");
				printf("-f: From Number\t\t"
				       "(ex: -f \"+18005551212\")\n");
				printf("-t: To Number\t\t"
				       "(ex: -t \"+18005551212\")\n");
				printf("-m: Message to send\t"
				       "(ex: -m \"Hello, Twilio!\")\n");
				printf("-p: (Opt.) URL to Image\t"
				       "(ex: -p \"Hello, Twilio!\")\n");
				printf("-v: Verbose Mode\n");
				printf("-b: Print account balance and exit\n");
				printf("-u: Print unsubscribed/invalid numbers and exit\n");
				printf("-c: Check Twilio credentials and exit (prints '1' if authenticated, '0' if not)\n");
				printf("-i: Check if we can connect to the Twilio server (prints '1' if can connect, '0' if cannot)\n");
				printf("-h: This help dialog\n");
				return 0;
			case 'a':
				account_sid = optarg;
				break;
			case 's':
				auth_token = optarg;
				break;
			case 'm':
				message = optarg;
				break;
			case 'f':
				from_number = optarg;
				break;
			case 't':
				to_number = optarg;
				break;
			case 'p':
				picture_url = optarg;
				break;
			case 'v':
				verbose = true;
				break;
			case 'b':
				checkbalance = true;
				break;
			case 'c':
				checkauth = true;
				break;
			case 'i':
				checkconnection = true;
				break;
			case 'u':
				getunsubscribednumbers = true;
				break;
			default:
				abort();
		}
	}

	if ((account_sid.empty() || auth_token.empty()) && !checkconnection) {
		cout<< "You didn't include all necessary inputs!" << endl << "Call using -h for help." << endl;
		return -1;
	}

	// Instantiate a twilio object and call send_message
	string response;
	auto twilio = make_shared<twilio::Twilio>(
	    account_sid, 
	    auth_token,
	    from_number
	);
	
	// Are we just checking connection?
	if (checkconnection) {
		cout << twilio->can_connect();
		return 0;
	}

	// Are we just checking balance?
	if (checkbalance) {
		cout << twilio->account_balance();
		return 0; // Don't send any message, just exit
	}

	// Are we just checking authentication?
	if (checkauth) {
		cout << twilio->can_authenticate();
		return 0;
	}

	// Are we just checking unsubscribed numbers?
	if (getunsubscribednumbers) {
		auto numbers = twilio->unsubscribed_numbers();

		for (auto& number : numbers) {
			cout << number << endl;
		}
		return 0;
	}

	if (from_number.empty() || to_number.empty() || message.empty()) {
		cout << "You didn't include all necessary inputs! " << endl << "Call using -h for help" << endl;
		return -1;
	}

	bool message_success = twilio->send_message(
		to_number, 
		message,
		response,
		picture_url,
		verbose
	);

	// Report success or failure
	if (!message_success) {
		if (verbose) {
			cout << "Message send failed." << endl;
			if (!response.empty()) {
				cout << "Response:" << endl 
					<< response << endl;
			}
		}
		return -1;
	} else if (verbose) {
		cout << "SMS sent successfully!" << endl;
		cout << "Response:" << endl << response
			<< endl;
	}

	return 0;
}
