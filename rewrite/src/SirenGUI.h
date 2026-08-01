/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#pragma once

#include <wx/artprov.h>
#include <wx/xrc/xmlres.h>
#include <wx/intl.h>
#include <wx/string.h>
#include <wx/bitmap.h>
#include <wx/image.h>
#include <wx/icon.h>
#include <wx/menu.h>
#include <wx/gdicmn.h>
#include <wx/font.h>
#include <wx/colour.h>
#include <wx/settings.h>
#include <wx/stattext.h>
#include <wx/sizer.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/frame.h>
#include <wx/dialog.h>

#include <cstdint>

namespace Siren::GUI {

using TenThousandthOfADollar = std::uint64_t;

inline std::string formatCost(TenThousandthOfADollar cost) {
	std::uint64_t dollars = cost / 10000;
	std::uint64_t cents = (cost % 10000) / 100;
	std::uint64_t tenThousandths = cost % 100;
	
	std::string result = std::to_string(dollars) + ".";

	if (cents < 10) result += "0";
	result += std::to_string(cents);

	if (tenThousandths < 10) result += "0";
	result += std::to_string(tenThousandths);

	return result;
}

class MainWindow : public wxFrame {
	private:
		TenThousandthOfADollar costPerRecipient = 79; // $0.0079 per recipient (initially, longer messages cost more)
		std::uint64_t totalRecipients = 4; // 4 recipients (initially, in the example text box)

		TenThousandthOfADollar calculateTotalCost() const { return costPerRecipient * totalRecipients; }
	protected:
		wxMenuBar* MainMenuBar;
		wxMenu* SettingsMenu;
		wxMenu* HelpMenu;
		wxStaticText* SignedInLabel;
		wxStaticText* AccountBalanceLabel;
		wxStaticText* MessageboxLabel;
		wxTextCtrl* m_textCtrl2;
		wxStaticText* CostPerMessageLabel;
		wxStaticText* PhoneNumbersLabel;
		wxTextCtrl* PhoneNumbersInputBox;
		wxStaticText* TotalCostLabel;
		wxButton* SendButton;

	public:
		MainWindow(wxWindow* parent,
			wxWindowID id = wxID_ANY,
			const wxString& title = _("Siren"),
			const wxPoint& pos = wxDefaultPosition,
			const wxSize& size = wxSize(581, 437),
			std::int64_t style = wxDEFAULT_FRAME_STYLE|wxTAB_TRAVERSAL);

		~MainWindow() override = default;

		MainWindow(const MainWindow&) = delete;
		MainWindow& operator=(const MainWindow&) = delete;
		MainWindow(MainWindow&&) = delete;
		MainWindow& operator=(MainWindow&&) = delete;

		void updateDisplayedCost(TenThousandthOfADollar newCostPerRecipient, std::uint64_t newTotalRecipients) {
			costPerRecipient = newCostPerRecipient;
			totalRecipients = newTotalRecipients;

			CostPerMessageLabel->SetLabel(_("Cost per recipient: $") + formatCost(costPerRecipient) + _(" (approx)"));
			TotalCostLabel->SetLabel(_("Total cost: $") + formatCost(calculateTotalCost()) + _(" (approx)"));
		}
};

class TwilioAccountSettingsWindow : public wxDialog {
	protected:
		wxStaticText* AccountIDLabel;
		wxTextCtrl* AccountIDInputBox;
		wxStaticText* AuthTokenLabel;
		wxTextCtrl* AuthTokenInputBox;
		wxStaticText* FromNumberLabel;
		wxTextCtrl* FromNumberInputBox;
		wxButton* CheckSettingsButton;
		wxButton* SaveSettingsButton;

	public:
		TwilioAccountSettingsWindow(wxWindow* parent,
			wxWindowID id = wxID_ANY,
			const wxString& title = ("Siren - Twilio Account"),
			const wxPoint& pos = wxDefaultPosition,
			const wxSize& size = wxSize(345, 309),
			std::int64_t style = wxDEFAULT_DIALOG_STYLE|wxTAB_TRAVERSAL);

		~TwilioAccountSettingsWindow() override = default;

		TwilioAccountSettingsWindow(const TwilioAccountSettingsWindow&) = delete;
		TwilioAccountSettingsWindow& operator=(const TwilioAccountSettingsWindow&) = delete;
		TwilioAccountSettingsWindow(TwilioAccountSettingsWindow&&) = delete;
		TwilioAccountSettingsWindow& operator=(TwilioAccountSettingsWindow&&) = delete;
};

class UnsubscribedNumbersWindow : public wxDialog {
	protected:
		wxTextCtrl* UnsubscribedNumbersTextbox;

	public:
		UnsubscribedNumbersWindow(wxWindow* parent,
			wxWindowID id = wxID_ANY,
			const wxString& title = ("Siren - Unsubscribed / Invalid Numbers"),
			const wxPoint& pos = wxDefaultPosition,
			const wxSize& size = wxSize(500, 332),
			std::int64_t style = wxDEFAULT_DIALOG_STYLE|wxTAB_TRAVERSAL);

		~UnsubscribedNumbersWindow() override = default;

		UnsubscribedNumbersWindow(const UnsubscribedNumbersWindow&) = delete;
		UnsubscribedNumbersWindow& operator=(const UnsubscribedNumbersWindow&) = delete;
		UnsubscribedNumbersWindow(UnsubscribedNumbersWindow&&) = delete;
		UnsubscribedNumbersWindow& operator=(UnsubscribedNumbersWindow&&) = delete;
};

} // namespace Siren::GUI

