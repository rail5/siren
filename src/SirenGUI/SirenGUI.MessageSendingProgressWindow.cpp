/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

// Unfortunately, wxWidgets relies on manual 'new'
// So we'll disable the clang-tidy warning for this file
//NOLINTBEGIN(cppcoreguidelines-owning-memory)

#include "../SirenGUI.h"

#include <wx/event.h>
#include <wx/msgdlg.h>

namespace Siren::GUI {

MessageSendingProgressWindow::MessageSendingProgressWindow(
	wxWindow* parent,
	std::shared_ptr<std::atomic<bool>> cancel_flag,
	wxWindowID id,
	const wxString& title,
	const wxPoint& pos,
	const wxSize& size,
	std::int64_t style
) : wxDialog(
	parent, id, title, pos, size, style), cancelFlag(cancel_flag)
{
	this->SetSizeHints(wxDefaultSize, wxDefaultSize);

	auto* Container = new wxBoxSizer(wxVERTICAL);

	ProgressLabel = new wxStaticText(
		this,
		wxID_ANY,
		_("Sending messages..."),
		wxDefaultPosition,
		wxDefaultSize,
		wxALIGN_CENTER_HORIZONTAL);
	ProgressLabel->Wrap(-1);
	Container->Add(ProgressLabel, 0, wxALL|wxALIGN_CENTER_HORIZONTAL, 5);

	ProgressBar = new wxGauge(
		this,
		wxID_ANY,
		100,
		wxDefaultPosition,
		wxDefaultSize,
		wxGA_HORIZONTAL);
	Container->Add(ProgressBar, 0, wxALL|wxEXPAND, 5);

	CancelButton = new wxButton(
		this,
		wxID_ANY,
		_("Cancel"),
		wxDefaultPosition,
		wxDefaultSize,
		0);
	Container->Add(CancelButton, 0, wxTOP|wxALIGN_CENTER_HORIZONTAL, 5);

	// Callback for when the user clicks the "Cancel" button
	CancelButton->Bind(wxEVT_BUTTON, [this](wxCommandEvent& /*event*/) {
		// Trip the cancel flag to tell the message sender to quit
		if (cancelFlag) cancelFlag->store(true);
		// Wait for full cancel:
		// When the user presses the cancel button, we follow this procedure:
		// 1. Request a cancellation (set cancelFlag to true)
		// 2. The background thread will notice the cancelFlag and stop sending messages
		// 3. The background thread will call MessageSendingProgressWindow::notifyCancelled() to say "OK, I've cancelled!"
		// 4. The notifyCancelled() function will show a message box and close the progress window
		CancelButton->Disable();
	});

	this->SetSizerAndFit(Container);
	this->Layout();
	this->Centre(wxBOTH);
}

void MessageSendingProgressWindow::updateProgress(std::uint8_t percentage) {
	if (percentage > 100) percentage = 100;
	ProgressBar->SetValue(percentage);

	// If we've reached 100%, show a dialog box that says "Done!" and then close the progress window
	if (percentage == 100) {
		wxMessageBox(
			_("Done!"),
			_("Done!"),
			wxOK | wxICON_INFORMATION,
			this);
		this->EndModal(wxID_OK);
	}
}

void MessageSendingProgressWindow::notifyCancelled() {
	wxMessageBox(
		_("Message sending cancelled."),
		_("Cancelled"),
		wxOK | wxICON_INFORMATION,
		this);
	this->EndModal(wxID_CANCEL);
}

} // namespace Siren::GUI
//NOLINTEND(cppcoreguidelines-owning-memory)
