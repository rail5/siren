/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

// Unfortunately, wxWidgets relies on manual 'new'
// So we'll disable the clang-tidy warning for this file
//NOLINTBEGIN(cppcoreguidelines-owning-memory)

#include "SirenGUI.h"

#include <wx/msgdlg.h>

#include <memory>

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

	m_textCtrl2 = new wxTextCtrl(
		this,
		wxID_ANY,
		_("Hello {name}!"),
		wxDefaultPosition,
		wxDefaultSize,
		wxTE_MULTILINE);
	m_textCtrl2->SetMinSize(wxSize(350,250));

	MessageSizer->Add(m_textCtrl2, 0, wxALL|wxEXPAND, 5);

	CostPerMessageLabel = new wxStaticText(
		this,
		wxID_ANY,
		_("Cost per recipient: $0.0079 (approx)"),
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

	PhoneNumbersSizer->Add(PhoneNumbersInputBox, 0, wxALL|wxEXPAND, 5);

	TotalCostLabel = new wxStaticText(
		this,
		wxID_ANY,
		_("Total cost: $0.0316 (approx)"),
		wxDefaultPosition,
		wxDefaultSize,
		0);
	TotalCostLabel->Wrap(-1);
	PhoneNumbersSizer->Add(TotalCostLabel, 0, wxALIGN_RIGHT|wxRIGHT, 5);


	MainContentSizer->Add(PhoneNumbersSizer, 1, wxEXPAND, 5);


	VerticalSizer->Add(MainContentSizer, 1, wxEXPAND, 5);

	auto* SendButtonSizer = new wxBoxSizer(wxVERTICAL);

	SendButton = new wxButton(this, wxID_ANY, _("Send messages"), wxDefaultPosition, wxDefaultSize, 0);
	SendButtonSizer->Add(SendButton, 0, wxALL, 5);


	VerticalSizer->Add(SendButtonSizer, 1, wxEXPAND, 5);


	this->SetSizer(VerticalSizer);
	this->Layout();

	this->Centre(wxBOTH);
}

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

	CheckSettingsButton = new wxButton(this, wxID_ANY, _("Check settings"), wxDefaultPosition, wxDefaultSize, 0);
	SettingsStackSizer->Add(CheckSettingsButton, 0, wxALL|wxEXPAND, 5);

	SaveSettingsButton = new wxButton(this, wxID_ANY, _("Save settings"), wxDefaultPosition, wxDefaultSize, 0);
	SettingsStackSizer->Add(SaveSettingsButton, 0, wxALL|wxEXPAND, 5);


	this->SetSizer(SettingsStackSizer);
	this->Layout();

	this->Centre(wxBOTH);
}

UnsubscribedNumbersWindow::UnsubscribedNumbersWindow(
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

	auto* Container = new wxBoxSizer(wxVERTICAL);

	UnsubscribedNumbersTextbox = new wxTextCtrl(
		this,
		wxID_ANY,
		wxEmptyString,
		wxDefaultPosition,
		wxDefaultSize,
		wxTE_DONTWRAP|wxTE_MULTILINE|wxTE_READONLY);
	UnsubscribedNumbersTextbox->SetMinSize(wxSize(-1,300));

	Container->Add(UnsubscribedNumbersTextbox, 0, wxALL|wxEXPAND, 5);


	this->SetSizer(Container);
	this->Layout();

	this->Centre(wxBOTH);
}

} // namespace Siren::GUI

//NOLINTEND(cppcoreguidelines-owning-memory)
