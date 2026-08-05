/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

// Unfortunately, wxWidgets relies on manual 'new'
// So we'll disable the clang-tidy warning for this file
//NOLINTBEGIN(cppcoreguidelines-owning-memory)

#include "../SirenGUI.h"

#ifndef _WIN32
	#include "../res/siren.xpm"
#endif

#include <wx/msgdlg.h>
#include <wx/utils.h>
#include <wx/weakref.h>

#include <memory>
#include <thread>

namespace Siren::GUI {

MainWindow::MainWindow(
	wxWindow* parent,
	wxWindowID id,
	const wxString& title,
	const wxPoint& pos,
	const wxSize& size,
	std::int64_t style
) : wxFrame(
	parent, id, title, pos, size, style
) {
	this->SetSizeHints(wxSize(581,437), wxDefaultSize);

	#ifdef _WIN32
		// For some reason, wxWidgets on windows defaults to an ugly dark-grey background,
		// overriding the operating system's color scheme.
		// By setting the background color to wxNullColour, we allow the operating system to choose the background color.
		this->SetBackgroundColour(wxNullColour);
		// On Windows, the icon is embedded in the executable via the .rc resource file, so no need to set it here.
	#else
		// On non-Windows platforms, set the icon from the XPM data.
		wxIcon icon(siren_xpm.data());
		this->SetIcon(icon);
	#endif

	//NOLINTBEGIN(cppcoreguidelines-prefer-member-initializer)
	MainMenuBar = new wxMenuBar(0);
	SettingsMenu = new wxMenu();
	auto* TwilioAccountMenuButton = new wxMenuItem(
		SettingsMenu,
		wxID_ANY,
		wxString(_("Twilio Account")) ,
		wxEmptyString,
		wxITEM_NORMAL);
	SettingsMenu->Append(TwilioAccountMenuButton);

	Bind(wxEVT_MENU, [this](wxCommandEvent& /*event*/) {
		auto dialog = std::make_unique<TwilioAccountSettingsWindow>(this);
		dialog->ShowModal();
	}, TwilioAccountMenuButton->GetId());

	SettingsMenu->AppendSeparator();

	auto* QuitButton = new wxMenuItem(
		SettingsMenu,
		wxID_ANY,
		wxString(_("Quit")),
		wxEmptyString,
		wxITEM_NORMAL);
	SettingsMenu->Append(QuitButton);

	Bind(wxEVT_MENU, [this](wxCommandEvent& /*event*/) {
		Close();
	}, QuitButton->GetId());

	MainMenuBar->Append(SettingsMenu, _("Settings"));

	HelpMenu = new wxMenu();
	//NOLINTEND(cppcoreguidelines-prefer-member-initializer)
	auto* RechargeButton = new wxMenuItem(
		HelpMenu,
		wxID_ANY,
		wxString(_("Recharge Account")),
		wxEmptyString,
		wxITEM_NORMAL);
	HelpMenu->Append(RechargeButton);

	// On-click action for recharge button: open a web browser to the Twilio billing page
	Bind(wxEVT_MENU, [](wxCommandEvent& /*event*/) {
		wxLaunchDefaultBrowser("https://console.twilio.com/us1/billing/manage-billing/billing-overview");
	}, RechargeButton->GetId());

	auto* UnsubscribedNumbersButton = new wxMenuItem(
		HelpMenu,
		wxID_ANY,
		wxString(_("View Unsubscribed Numbers")),
		wxEmptyString,
		wxITEM_NORMAL);
	HelpMenu->Append(UnsubscribedNumbersButton);

	Bind(wxEVT_MENU, [this](wxCommandEvent& /*event*/) {
		auto dialog = std::make_unique<UnsubscribedNumbersWindow>(this);
		dialog->ShowModal();
	}, UnsubscribedNumbersButton->GetId());

	HelpMenu->AppendSeparator();

	auto* AboutButton = new wxMenuItem(
		HelpMenu,
		wxID_ANY,
		wxString(_("About")) ,
		wxEmptyString,
		wxITEM_NORMAL);
	HelpMenu->Append(AboutButton);


	Bind(wxEVT_MENU, [this](wxCommandEvent& /*event*/) {
		wxMessageBox(
			_("Copyright (C) 2026 Andrew S. Rightenburg\r\n\r\n"
			"This program is free software: you can redistribute it and/or modify it\r\n"
			"under the terms of the GNU General Public License as published by the\r\n"
			"Free Software Foundation, either version 3 of the License, or (at your\r\n"
			"option) any later version.\r\n\r\n"
			"This program is distributed in the hope that it will be useful, but\r\n"
			"WITHOUT ANY WARRANTY; without even the implied warranty of\r\n"
			"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\r\n"
			"See the GNU General Public License for more details.\r\n\r\n"
			"You should have received a copy of the GNU General Public License along\r\n"
			"with this program. If not, see <https://www.gnu.org/licenses/>."),
			_("About Siren"), wxOK | wxICON_INFORMATION, this);
	}, AboutButton->GetId());


	MainMenuBar->Append(HelpMenu, _("Help"));

	this->SetMenuBar(MainMenuBar);

	auto* VerticalSizer = new wxBoxSizer(wxVERTICAL);

	auto* MetaDataSizer = new wxBoxSizer(wxVERTICAL);

	SignedInLabel = new wxStaticText(
		this,
		wxID_ANY,
		_("You are not signed in"),
		wxDefaultPosition,
		wxDefaultSize,
		wxALIGN_CENTER_HORIZONTAL);
	SignedInLabel->Wrap(-1);
	MetaDataSizer->Add(SignedInLabel, 0, wxALIGN_CENTER, 5);

	AccountBalanceLabel = new wxStaticText(
		this,
		wxID_ANY,
		_("Balance: $0"),
		wxDefaultPosition,
		wxDefaultSize,
		wxALIGN_CENTER_HORIZONTAL);
	AccountBalanceLabel->Wrap(-1);
	MetaDataSizer->Add(AccountBalanceLabel, 0, wxALIGN_CENTER, 5);

	VerticalSizer->Add(MetaDataSizer, 1, wxEXPAND, 5);

	auto* MainContentSizer = new wxBoxSizer(wxHORIZONTAL);

	auto* MessageSizer = new wxBoxSizer(wxVERTICAL);

	MessageboxLabel = new wxStaticText(this, wxID_ANY, _("Message:"), wxDefaultPosition, wxDefaultSize, 0);
	MessageboxLabel->Wrap(-1);
	MessageSizer->Add(MessageboxLabel, 0, wxALIGN_LEFT|wxLEFT, 5);

	MessageBox = new wxTextCtrl(
		this,
		wxID_ANY,
		_("Hello $name!"),
		wxDefaultPosition,
		wxDefaultSize,
		wxTE_MULTILINE);
	MessageBox->SetMinSize(wxSize(350,250));

	MessageSizer->Add(MessageBox, 0, wxALL|wxEXPAND, 5);

	// Add callback for when the message box text changes:
	// 1. Normalize the message body to remove non-GSM characters (Twilio::normalizeMessageBody)
	// 2. Update the displayed cost per recipient and total cost in the GUI (MainWindow::updateDisplayedCost)
	MessageBox->Bind(wxEVT_TEXT, [this](wxCommandEvent& /*event*/) {
		const wxString& current_value = MessageBox->GetValue();
		Twilio::TextMessage message(reinterpret_cast<const char8_t*>(current_value.ToUTF8().data()));
		const wxString normalized_value = wxString::FromUTF8(reinterpret_cast<const char*>(message.getMessageBody().data()), message.getMessageBody().size());
		if (normalized_value != current_value) {
			long selection_start = 0;
			long selection_end = 0;
			MessageBox->GetSelection(&selection_start, &selection_end);
			const auto insertion_point = MessageBox->GetInsertionPoint();

			// Defer the rewrite so GTK can finish processing the paste event first.
			MessageBox->CallAfter([this, normalized_value, selection_start, selection_end, insertion_point, current_value]() {
					// Map original selection/insertion indices through normalization so
					// multi-character replacements (e.g. "½" -> "1/2") keep the cursor
					// at the expected logical position.
					auto new_sel_start = selection_start;
					auto new_sel_end = selection_end;
					auto new_insertion = static_cast<std::int64_t>(insertion_point);

					if (selection_start != selection_end) {
						// Compute normalized prefix lengths for both selection boundaries
						wxString prefix_start = current_value.Left(selection_start);
						Twilio::TextMessage tprefix_start(reinterpret_cast<const char8_t*>(prefix_start.ToUTF8().data()));
						const wxString normalized_prefix_start = wxString::FromUTF8(
							reinterpret_cast<const char*>(tprefix_start.getMessageBody().data()),
							static_cast<std::size_t>(tprefix_start.getMessageBody().size())
						);
						new_sel_start = static_cast<std::int64_t>(normalized_prefix_start.Length());

						wxString prefix_end = current_value.Left(selection_end);
						Twilio::TextMessage tprefix_end(reinterpret_cast<const char8_t*>(prefix_end.ToUTF8().data()));
						const wxString normalized_prefix_end = wxString::FromUTF8(
							reinterpret_cast<const char*>(tprefix_end.getMessageBody().data()),
							static_cast<std::size_t>(tprefix_end.getMessageBody().size())
						);
						new_sel_end = static_cast<std::int64_t>(normalized_prefix_end.Length());
					} else {
						// Compute normalized prefix length for caret position
						wxString prefix = current_value.Left(insertion_point);
						Twilio::TextMessage tprefix(reinterpret_cast<const char8_t*>(prefix.ToUTF8().data()));
						const wxString normalized_prefix = wxString::FromUTF8(
							reinterpret_cast<const char*>(tprefix.getMessageBody().data()),
							static_cast<std::size_t>(tprefix.getMessageBody().size())
						);
						new_insertion = static_cast<std::int64_t>(normalized_prefix.Length());
					}

					MessageBox->ChangeValue(normalized_value);
					if (selection_start != selection_end) {
						MessageBox->SetSelection(new_sel_start, new_sel_end);
					} else {
						MessageBox->SetInsertionPoint(new_insertion);
					}
				});
		}

		updateDisplayedCost();
	});

	CostPerMessageLabel = new wxStaticText(
		this,
		wxID_ANY,
		_("Cost per recipient: $0.0000 (approx)"),
		wxDefaultPosition,
		wxDefaultSize,
		0);
	CostPerMessageLabel->Wrap(-1);
	MessageSizer->Add(CostPerMessageLabel, 0, wxALIGN_LEFT|wxLEFT, 5);

	MainContentSizer->Add(MessageSizer, 1, wxEXPAND, 5);

	auto* PhoneNumbersSizer = new wxBoxSizer(wxVERTICAL);

	PhoneNumbersLabel = new wxStaticText(
		this,
		wxID_ANY,
		_("Phone numbers (one per line):"),
		wxDefaultPosition,
		wxDefaultSize,
		0);
	PhoneNumbersLabel->Wrap(-1);
	PhoneNumbersSizer->Add(PhoneNumbersLabel, 0, wxALIGN_RIGHT|wxRIGHT, 5);

	PhoneNumbersInputBox = new wxTextCtrl(
		this,
		wxID_ANY,
		_("123-456-7890\n555-555-5555, Brian\n0123456789\n0001112222, Sarah"),
		wxDefaultPosition,
		wxDefaultSize,
		wxTE_MULTILINE);
	PhoneNumbersInputBox->SetMinSize(wxSize(200,250));

	// Add callback for when the phone numbers input box text changes:
	// 1. Count the number of lines in the input box (each line is a recipient)
	// 2. Update the displayed total cost (MainWindow::updateDisplayedCost)
	PhoneNumbersInputBox->Bind(wxEVT_TEXT, [this](wxCommandEvent& /*event*/) {
		updateDisplayedCost();
	});

	PhoneNumbersSizer->Add(PhoneNumbersInputBox, 0, wxALL|wxEXPAND, 5);

	TotalCostLabel = new wxStaticText(
		this,
		wxID_ANY,
		_("Total cost: $0.0000 (approx)"),
		wxDefaultPosition,
		wxDefaultSize,
		0);
	TotalCostLabel->Wrap(-1);
	PhoneNumbersSizer->Add(TotalCostLabel, 0, wxALIGN_RIGHT|wxRIGHT, 5);

	updateDisplayedCost(); // Update the displayed cost based on the initial message and phone numbers

	MainContentSizer->Add(PhoneNumbersSizer, 1, wxEXPAND, 5);


	VerticalSizer->Add(MainContentSizer, 1, wxEXPAND, 5);

	auto* SendButtonSizer = new wxBoxSizer(wxHORIZONTAL);

	SendButton = new wxButton(this, wxID_ANY, _("Send messages"), wxDefaultPosition, wxDefaultSize, 0);
	SendButtonSizer->AddStretchSpacer(1);
	SendButtonSizer->Add(SendButton, 0, wxALL, 5);
	SendButtonSizer->AddStretchSpacer(1);


	VerticalSizer->Add(SendButtonSizer, 1, wxEXPAND, 5);

	// SendButton callback: when clicked, open a MessageSendingProgressWindow and send the messages in a background thread
	SendButton->Bind(wxEVT_BUTTON, [this](wxCommandEvent& /*event*/) {
		auto cancel_flag = std::make_shared<std::atomic<bool>>(false);
		auto* progress_window = new MessageSendingProgressWindow(this, cancel_flag);
		const Twilio::TextMessage message(reinterpret_cast<const char8_t*>(MessageBox->GetValue().ToUTF8().data()));
		const auto recipients = getRecipientsFromInputBox();
		std::thread([this, progress_window, cancel_flag, message, recipients]() {
			if (!unsubscribedNumbersList.waitUntilReadyOrCancelled(cancel_flag)) {
				if (progress_window) progress_window->CallAfter([progress_window]() {
					progress_window->notifyCancelled();
				});
				return;
			}
			const auto unsubscribed_numbers = getUnsubscribedNumbers();
			const auto result = twilioClient.sendMassMessage(
				recipients,
				message,
				unsubscribed_numbers,
				cancel_flag,
				[progress_window](std::uint8_t percentage) {
					if (progress_window) progress_window->CallAfter([progress_window, percentage]() {
						progress_window->updateProgress(percentage);
					});
				},
				[progress_window]() {
					if (progress_window) progress_window->CallAfter([progress_window]() {
						progress_window->notifyCancelled();
					});
				}
			);
		}).detach();
		progress_window->ShowModal();
		progress_window->Destroy();
	});


	this->SetSizer(VerticalSizer);
	this->Layout();

	this->Centre(wxBOTH);

	SignedInLabel->SetLabel(_("Signing in..."));
	AccountBalanceLabel->SetLabel(_("Balance: loading..."));
	SignedInLabel->Wrap(-1);
	AccountBalanceLabel->Wrap(-1);
	Layout();

	// Attempt to load Twilio settings from the config file in the background.
	auto config_file_path = Siren::getHomeDirectory() / ".siren-config";
	// Check if the config file exists before attempting to load it.
	// If not, try to load from the old-style XML config file (~/.siren-config.xml).
	if (!std::filesystem::exists(config_file_path)) config_file_path = Siren::getHomeDirectory() / "siren-config.xml";
	loadTwilioSettingsAsync(config_file_path);
}

void MainWindow::loadTwilioSettingsAsync(std::filesystem::path config_file_path) {
	wxWeakRef<MainWindow> weak_self(this);
	std::thread([weak_self, config_file_path]() mutable {
		if (!weak_self) return;
		if (!weak_self->twilioClient.loadSettingsFromConfigFile(config_file_path)) {
			weak_self->CallAfter([weak_self]() {
				if (!weak_self) return;
				weak_self->SignedInLabel->SetLabel(_("You are not signed in"));
				weak_self->AccountBalanceLabel->SetLabel(_("Balance: $0"));
				weak_self->SignedInLabel->Wrap(-1);
				weak_self->AccountBalanceLabel->Wrap(-1);
				weak_self->Layout();
			});
			return;
		}

		if (weak_self) weak_self->CallAfter([weak_self]() {
			if (!weak_self) return;
			weak_self->SignedInLabel->SetLabel(_("Signing in..."));
			weak_self->AccountBalanceLabel->SetLabel(_("Balance: loading..."));
			weak_self->SignedInLabel->Wrap(-1);
			weak_self->AccountBalanceLabel->Wrap(-1);
			weak_self->Layout();
		});

		const std::string& account_sid = weak_self->twilioClient.getAccountSID();
		const std::string& auth_token = weak_self->twilioClient.getAuthToken();

		const bool can_authenticate = weak_self->twilioClient.canAuthenticate();
		const bool from_number_is_valid = can_authenticate && weak_self->twilioClient.fromNumberIsValid();
		const std::string account_balance = (can_authenticate && from_number_is_valid)
			? weak_self->twilioClient.getAccountBalance()
			: "0";

		if (weak_self) weak_self->CallAfter([
			weak_self,
			account_sid,
			auth_token,
			can_authenticate,
			from_number_is_valid,
			account_balance
		]() {
			if (!weak_self) return;

			if (!can_authenticate) {
				wxMessageBox(
					_("Could not authenticate with the Twilio API. Please check your Account ID and Auth Token."),
					_("Authentication Error"),
					wxOK | wxICON_ERROR,
					weak_self);
				weak_self->SignedInLabel->SetLabel(_("You are not signed in"));
				weak_self->AccountBalanceLabel->SetLabel(_("Balance: $0"));
				weak_self->SignedInLabel->Wrap(-1);
				weak_self->AccountBalanceLabel->Wrap(-1);
				weak_self->Layout();
				return;
			}

			if (!from_number_is_valid) {
				wxMessageBox(
					_("The provided \"From\" phone number is not valid for this Twilio account. Please check the number and ensure it is associated with your Twilio account."),
					_("Invalid From Number"),
					wxOK | wxICON_ERROR,
					weak_self);
				weak_self->SignedInLabel->SetLabel(_("You are not signed in"));
				weak_self->AccountBalanceLabel->SetLabel(_("Balance: $0"));
				weak_self->SignedInLabel->Wrap(-1);
				weak_self->AccountBalanceLabel->Wrap(-1);
				weak_self->Layout();
				return;
			}

			weak_self->SignedInLabel->SetLabel(_("Signed in to Twilio"));
			weak_self->AccountBalanceLabel->SetLabel(_("Balance: $") + account_balance);
			weak_self->SignedInLabel->Wrap(-1);
			weak_self->AccountBalanceLabel->Wrap(-1);
			weak_self->Layout();

			// Once we're signed in, we can also load the unsubscribed numbers in the background.
			weak_self->loadUnsubscribedNumbersAsync();
		});
	}).detach();
}

void MainWindow::loadUnsubscribedNumbersAsync() {
	wxWeakRef<MainWindow> weak_self(this);
	const std::string account_sid = twilioClient.getAccountSID();
	const std::string auth_token = twilioClient.getAuthToken();
	std::thread([weak_self, account_sid, auth_token]() mutable {
		if (!weak_self) return;

		std::set<Twilio::PhoneNumber> unsubscribed_numbers;
		try {
			Twilio::Twilio twilio;
			twilio.setAccountSID(account_sid);
			twilio.setAuthToken(auth_token);
			unsubscribed_numbers = twilio.getUnsubscribedNumbers();
		} catch (const std::exception&) {
			unsubscribed_numbers.clear();
		}
		if (weak_self) weak_self->CallAfter([weak_self, unsubscribed_numbers]() {
			if (!weak_self) return;
			weak_self->unsubscribedNumbersList.setNumbers(unsubscribed_numbers);
		});
	}).detach();
}

std::set<Twilio::PhoneNumber> MainWindow::getRecipientsFromInputBox() const {
	std::set<Twilio::PhoneNumber> recipients;
	const wxString& input_text = PhoneNumbersInputBox->GetValue();
	// Split the input text into lines
	std::vector<std::string> lines;
	std::string current_line;
	for (const auto& ch : input_text) {
		if (ch == '\n') {
			lines.push_back(current_line);
			current_line.clear();
		} else {
			current_line += static_cast<char>(ch);
		}
	}
	lines.push_back(current_line);

	// Split each line into `number{, name}`
	for (const auto& line : lines) {
		Twilio::PhoneNumber recipient;
		const auto comma_pos = line.find(',');
		if (comma_pos != std::string::npos) {
			//recipient.setNumber(line.substr(0, comma_pos));
			const auto number = line.substr(0, comma_pos);
			// If the 'number' is blank or contains only whitespace, skip this line
			if (number.find_first_not_of(" \t") == std::string::npos) continue;
			recipient.setNumber(number);
			// Skip any whitespace after the comma for the name
			const auto name_start = line.find_first_not_of(" \t", comma_pos + 1);
			if (name_start != std::string::npos) recipient.setName(line.substr(name_start));
		} else {
			if (line.find_first_not_of(" \t") == std::string::npos) continue; // Skip blank lines
			recipient.setNumber(line);
		}

		recipients.insert(recipient); // Implicitly skips duplicates due to being a std::set
	}

	return recipients;
}

} // namespace Siren::GUI

//NOLINTEND(cppcoreguidelines-owning-memory)
