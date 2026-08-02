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

#include "Siren.h"
#include "Twilio.h"

#include <cstdint>
#include <condition_variable>
#include <set>
#include <string>
#include <atomic>
#include <mutex>

namespace Siren::GUI {

/**
 * @brief The main window of the application, providing the user interface for sending messages via Twilio.
 * 
 */
class MainWindow : public wxFrame {
	private:
		TenThousandthOfADollar costPerRecipient = 83; // $0.0083 per recipient (initially, longer messages cost more)
		std::uint64_t totalRecipients = 4; // 4 recipients (initially, in the example text box)

		TenThousandthOfADollar calculateTotalCost() const { return costPerRecipient * totalRecipients; }

		Twilio::Twilio twilioClient;

		/**
		 * @brief A list of unsubscribed phone numbers.
		 *
		 * The list is thread-safe and can be accessed from multiple threads.
		 * It uses a condition variable to allow threads to wait until the list is ready.
		 * It is populated asynchronously when the application starts, and can be accessed via the getUnsubscribedNumbers() method.
		 */
		class UnsubscribedNumbersList {
			private:
				std::set<std::string> numbers;
				std::atomic<bool> ready{false};
				std::mutex mutex;
				std::condition_variable condition_variable;
			public:
				/**
				 * @brief Sets the list of unsubscribed numbers and notifies any waiting threads that the list is ready.
				 * 
				 * @param new_numbers The new set of unsubscribed numbers.
				 */
				void setNumbers(const std::set<std::string>& new_numbers) {
					std::lock_guard<std::mutex> lock(mutex);
					numbers = new_numbers;
					ready = true;
					condition_variable.notify_all();
				}

				/**
				 * @brief Get the list of unsubscribed numbers. This method is thread-safe.
				 * 
				 * @return std::set<std::string> The set of unsubscribed numbers.
				 */
				std::set<std::string> getNumbers() {
					std::lock_guard<std::mutex> lock(mutex);
					return numbers;
				}

				/**
				 * @brief Waits until the list of unsubscribed numbers is ready.
				 * This method blocks the calling thread until the list is ready.
				 */
				void waitUntilReady() {
					std::unique_lock<std::mutex> lock(mutex);
					condition_variable.wait(lock, [this]() {
						return ready.load();
					});
				}

				/**
				 * @brief Checks if the list of unsubscribed numbers is ready to be read.
				 * 
				 * @return true if the list is ready, false otherwise.
				 */
				bool isReady() const {
					return ready.load();
				}
		} unsubscribedNumbersList;
	protected:
		wxMenuBar* MainMenuBar;
		wxMenu* SettingsMenu;
		wxMenu* HelpMenu;
		wxStaticText* SignedInLabel;
		wxStaticText* AccountBalanceLabel;
		wxStaticText* MessageboxLabel;
		wxTextCtrl* MessageBox;
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

		/**
		 * @brief Updates the displayed cost per recipient and total cost in the GUI.
		 * 
		 * @param newCostPerRecipient The new cost per recipient in ten-thousandths of a dollar (determined by message length)
		 * @param newTotalRecipients The new total number of recipients (determined by the number of lines in the phone numbers input box)
		 */
		void updateDisplayedCost(TenThousandthOfADollar newCostPerRecipient, std::uint64_t newTotalRecipients) {
			costPerRecipient = newCostPerRecipient;
			totalRecipients = newTotalRecipients;

			CostPerMessageLabel->SetLabel(_("Cost per recipient: $") + formatCost(costPerRecipient) + _(" (approx)"));
			TotalCostLabel->SetLabel(_("Total cost: $") + formatCost(calculateTotalCost()) + _(" (approx)"));
			CostPerMessageLabel->Wrap(-1);
			TotalCostLabel->Wrap(-1);
			Layout();
		}

		Twilio::Twilio& getTwilioClient() { return twilioClient; }

		/**
		 * @brief Loads Twilio settings from the specified configuration file asynchronously.
		 * 
		 * @param config_file_path The path to the configuration file containing Twilio settings.
		 */
		void loadTwilioSettingsAsync(std::filesystem::path config_file_path);

		/**
		 * @brief Loads the list of unsubscribed phone numbers asynchronously.
		 */
		void loadUnsubscribedNumbersAsync();

		bool unsubscribedNumbersReady() const { return unsubscribedNumbersList.isReady(); }
		void waitForUnsubscribedNumbers() { unsubscribedNumbersList.waitUntilReady(); }
		std::set<std::string> getUnsubscribedNumbers() { return unsubscribedNumbersList.getNumbers(); }
};

/**
 * @brief A dialog for managing Twilio account settings.
 * 
 */
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
			const wxString& title = _("Siren - Twilio Account"),
			const wxPoint& pos = wxDefaultPosition,
			const wxSize& size = wxSize(345, 309),
			std::int64_t style = wxDEFAULT_DIALOG_STYLE|wxTAB_TRAVERSAL);

		~TwilioAccountSettingsWindow() override = default;

		TwilioAccountSettingsWindow(const TwilioAccountSettingsWindow&) = delete;
		TwilioAccountSettingsWindow& operator=(const TwilioAccountSettingsWindow&) = delete;
		TwilioAccountSettingsWindow(TwilioAccountSettingsWindow&&) = delete;
		TwilioAccountSettingsWindow& operator=(TwilioAccountSettingsWindow&&) = delete;
};

/**
 * @brief A dialog for displaying unsubscribed or invalid phone numbers.
 * 
 */
class UnsubscribedNumbersWindow : public wxDialog {
	protected:
		wxTextCtrl* UnsubscribedNumbersTextbox;

	public:
		UnsubscribedNumbersWindow(wxWindow* parent,
			wxWindowID id = wxID_ANY,
			const wxString& title = _("Siren - Unsubscribed / Invalid Numbers"),
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

