/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

// Unfortunately, wxWidgets relies on manual 'new'
// So we'll disable the clang-tidy warning for this file
//NOLINTBEGIN(cppcoreguidelines-owning-memory)

#include "../SirenGUI.h"

namespace Siren::GUI {

MessageSendingProgressWindow::MessageSendingProgressWindow(
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
		// Close the dialog
		//TODO(@rail5): Cancel the sending of messages in the background thread
		this->EndModal(wxID_CANCEL);
	});

	this->SetSizerAndFit(Container);
	this->Layout();
	this->Centre(wxBOTH);
}

} // namespace Siren::GUI
//NOLINTEND(cppcoreguidelines-owning-memory)
