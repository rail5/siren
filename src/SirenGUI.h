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
#include <wx/gauge.h>

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
		TenThousandthOfADollar costPerRecipient = Twilio::Twilio::costPerSegment; // $0.0083 per recipient (initially, longer messages cost more)
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
				std::set<Twilio::PhoneNumber> numbers;
				std::atomic<bool> ready{false};
				std::mutex mutex;
				std::condition_variable condition_variable;
			public:
				/**
				 * @brief Sets the list of unsubscribed numbers and notifies any waiting threads that the list is ready.
				 * 
				 * @param new_numbers The new set of unsubscribed numbers.
				 */
				void setNumbers(const std::set<Twilio::PhoneNumber>& new_numbers) {
					std::lock_guard<std::mutex> lock(mutex);
					numbers = new_numbers;
					ready = true;
					condition_variable.notify_all();
				}

				/**
				 * @brief Get the list of unsubscribed numbers. This method is thread-safe.
				 * 
				 * @return std::set<Twilio::PhoneNumber> The set of unsubscribed numbers.
				 */
				std::set<Twilio::PhoneNumber> getNumbers() {
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
		 * @brief Updates the displayed cost per recipient and total cost in the GUI, based on the length of the message and the number of recipients.
		 */
		void updateDisplayedCost() {
			// 1. Calculate the cost per recipient based on the length of the message (MainWindow::calculateCostPerRecipient)
			Twilio::TextMessage message(reinterpret_cast<const char8_t*>(MessageBox->GetValue().ToUTF8().data()));
			costPerRecipient = message.getCostPerRecipient();

			// 2. Count the number of lines in the phone numbers input box (each line is a recipient)
			std::string phone_numbers_text = PhoneNumbersInputBox->GetValue().ToStdString();
			totalRecipients = std::count(phone_numbers_text.begin(), phone_numbers_text.end(), '\n') + 1;

			// 3. Update the displayed cost per recipient and total cost in the GUI
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

		void waitForUnsubscribedNumbers() { unsubscribedNumbersList.waitUntilReady(); }
		std::set<Twilio::PhoneNumber> getUnsubscribedNumbers() { return unsubscribedNumbersList.getNumbers(); }
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

class MessageSendingProgressWindow : public wxDialog {
	protected:
		wxStaticText* ProgressLabel;
		wxGauge* ProgressBar;
		wxButton* CancelButton;

	public:
		MessageSendingProgressWindow(wxWindow* parent,
			wxWindowID id = wxID_ANY,
			const wxString& title = _("Siren - Sending Messages"),
			const wxPoint& pos = wxDefaultPosition,
			const wxSize& size = wxSize(400, 150),
			std::int64_t style = wxCAPTION|wxSYSTEM_MENU|wxTAB_TRAVERSAL); // No "close" button

		~MessageSendingProgressWindow() override = default;

		MessageSendingProgressWindow(const MessageSendingProgressWindow&) = delete;
		MessageSendingProgressWindow& operator=(const MessageSendingProgressWindow&) = delete;
		MessageSendingProgressWindow(MessageSendingProgressWindow&&) = delete;
		MessageSendingProgressWindow& operator=(MessageSendingProgressWindow&&) = delete;

		void updateProgress(std::size_t current, std::size_t total);
};

} // namespace Siren::GUI

