/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

// Unfortunately, wxWidgets relies on manual 'new'
// So we'll disable the clang-tidy warning for this file
//NOLINTBEGIN(cppcoreguidelines-owning-memory)

#include "../SirenGUI.h"

#include <wx/msgdlg.h>
#include <wx/utils.h>
#include <wx/weakref.h>

#include <filesystem>

namespace Siren::GUI {

TwilioAccountSettingsWindow::TwilioAccountSettingsWindow(
	wxWindow* parent,
	wxWindowID id,
	const wxString& title,
	const wxPoint& pos,
	const wxSize& size,
	std::int64_t style
) : wxDialog(
	parent, id, title, pos, size, style
) {
	this->SetSizeHints(wxDefaultSize, wxDefaultSize);

	auto* SettingsStackSizer = new wxBoxSizer(wxVERTICAL);

	AccountIDLabel = new wxStaticText(
		this,
		wxID_ANY,
		_("Twilio Account ID:"),
		wxDefaultPosition,
		wxDefaultSize,
		0);
	AccountIDLabel->Wrap(-1);
	SettingsStackSizer->Add(AccountIDLabel, 0, wxALIGN_CENTER, 5);

	AccountIDInputBox = new wxTextCtrl(
		this,
		wxID_ANY,
		wxEmptyString,
		wxDefaultPosition,
		wxDefaultSize,
		0);
	SettingsStackSizer->Add(AccountIDInputBox, 0, wxALL|wxEXPAND, 5);

	AuthTokenLabel = new wxStaticText(
		this,
		wxID_ANY,
		_("Twilio Auth Token:"),
		wxDefaultPosition,
		wxDefaultSize,
		0);
	AuthTokenLabel->Wrap(-1);
	SettingsStackSizer->Add(AuthTokenLabel, 0, wxALIGN_CENTER, 5);

	AuthTokenInputBox = new wxTextCtrl(
		this,
		wxID_ANY,
		wxEmptyString,
		wxDefaultPosition,
		wxDefaultSize,
		0);
	SettingsStackSizer->Add(AuthTokenInputBox, 0, wxALL|wxEXPAND, 5);

	FromNumberLabel = new wxStaticText(
		this,
		wxID_ANY,
		_("Twilio \"From\" Phone Number:"),
		wxDefaultPosition,
		wxDefaultSize,
		0);
	FromNumberLabel->Wrap(-1);
	SettingsStackSizer->Add(FromNumberLabel, 0, wxALIGN_CENTER, 5);

	FromNumberInputBox = new wxTextCtrl(
		this,
		wxID_ANY,
		wxEmptyString,
		wxDefaultPosition,
		wxDefaultSize,
		0);
	SettingsStackSizer->Add(FromNumberInputBox, 0, wxALL|wxEXPAND, 5);

	// Default-fill the input boxes with the current Twilio settings from the parent window
	auto* parent_window = dynamic_cast<MainWindow*>(GetParent());
	if (parent_window) {
		Twilio::Twilio& twilio = parent_window->getTwilioClient();
		AccountIDInputBox->SetValue(twilio.getAccountSID());
		AuthTokenInputBox->SetValue(twilio.getAuthToken());
		FromNumberInputBox->SetValue(wxString::FromUTF8(std::string(twilio.getFromNumber()).c_str()));
	}

	CheckSettingsButton = new wxButton(this, wxID_ANY, _("Check settings"), wxDefaultPosition, wxDefaultSize, 0);

	// Callback for when the "Check settings" button is clicked:
	// 1. Get the account ID, auth token, and from number from the input
	// 2. Create a Twilio object with those settings
	// 3. Call Twilio::canAuthenticate() to check if the settings are valid
	// 4. Call Twilio::fromNumberIsValid() to check if the from number is valid
	// 5. Display a message box with the results
	CheckSettingsButton->Bind(wxEVT_BUTTON, [this](wxCommandEvent& /*event*/) {
		// The parent window should hold the Twilio object, so we can just use that instead of creating a new one
		auto* parent_window = dynamic_cast<MainWindow*>(GetParent());
		if (!parent_window) {
			wxMessageBox(
				_("Could not access parent window."),
				_("Error"),
				wxOK | wxICON_ERROR,
				this);
			return;
		}
		Twilio::Twilio& twilio = parent_window->getTwilioClient();
		twilio.setAccountSID(AccountIDInputBox->GetValue().ToStdString());
		twilio.setAuthToken(AuthTokenInputBox->GetValue().ToStdString());
		twilio.setFromNumber(Twilio::PhoneNumber(FromNumberInputBox->GetValue().ToStdString()));
		// First: can we even connect to the Twilio API?
		if (!Twilio::Twilio::canConnect()) {
			wxMessageBox(
				_("Could not connect to the Twilio API. Please check your internet connection."),
				_("Connection Error"),
				wxOK | wxICON_ERROR,
				this);
			return;
		}
		// Second: can we authenticate with the provided credentials?
		if (!twilio.canAuthenticate()) {
			wxMessageBox(
				_("Could not authenticate with the Twilio API. Please check your Account ID and Auth Token."),
				_("Authentication Error"),
				wxOK | wxICON_ERROR,
				this);
			return;
		}
		// Third: is the from number valid for this account?
		if (!twilio.fromNumberIsValid()) {
			wxMessageBox(
				_("The provided \"From\" phone number is not valid for this Twilio account. Please check the number and ensure it is associated with your Twilio account."),
				_("Invalid From Number"),
				wxOK | wxICON_ERROR,
				this);
			return;
		}
		// If we got here, everything is valid
		wxMessageBox(
			_("The provided Twilio settings are valid!"),
			_("Settings Valid"),
			wxOK | wxICON_INFORMATION,
			this);
	});
	SettingsStackSizer->Add(CheckSettingsButton, 0, wxALL|wxEXPAND, 5);

	SaveSettingsButton = new wxButton(this, wxID_ANY, _("Save settings"), wxDefaultPosition, wxDefaultSize, 0);
	SettingsStackSizer->Add(SaveSettingsButton, 0, wxALL|wxEXPAND, 5);

	// Callback for when the "Save settings" button is clicked:
	// 1. Get the account ID, auth token, and from number from the input
	// 2. Create a Twilio object with those settings
	// 3. Call Twilio::saveConfigFile() to save the settings to ~/.siren-config
	SaveSettingsButton->Bind(wxEVT_BUTTON, [this](wxCommandEvent& /*event*/) {
		auto* parent_window = dynamic_cast<MainWindow*>(GetParent());
		if (!parent_window) {
			wxMessageBox(
				_("Could not access parent window."),
				_("Error"),
				wxOK | wxICON_ERROR,
				this);
			return;
		}
		Twilio::Twilio& twilio = parent_window->getTwilioClient();
		twilio.setAccountSID(AccountIDInputBox->GetValue().ToStdString());
		twilio.setAuthToken(AuthTokenInputBox->GetValue().ToStdString());
		twilio.setFromNumber(Twilio::PhoneNumber(FromNumberInputBox->GetValue().ToStdString()));
		std::filesystem::path config_file_path = Siren::getHomeDirectory() / ".siren-config";
		twilio.saveConfigFile(config_file_path);

		parent_window->loadTwilioSettingsAsync(config_file_path);

		// Close the dialog after saving settings
		this->Close();
	});

	this->SetSizer(SettingsStackSizer);
	this->Layout();

	this->Centre(wxBOTH);
}

} // namespace Siren::GUI
//NOLINTEND(cppcoreguidelines-owning-memory)
