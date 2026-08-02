/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

// Unfortunately, wxWidgets relies on manual 'new'
// So we'll disable the clang-tidy warning for this file
//NOLINTBEGIN(cppcoreguidelines-owning-memory)

#include "../SirenGUI.h"

#include <wx/utils.h>
#include <wx/weakref.h>

#include <thread>

namespace Siren::GUI {

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

	// Wait for the unsubscribed numbers to be ready, then populate the textbox
	// Until then, at least inform the user that the list is loading
	UnsubscribedNumbersTextbox->SetValue(_("Loading unsubscribed numbers..."));
	wxWeakRef<UnsubscribedNumbersWindow> weak_self(this);
	std::thread([this, weak_self]() {
		if (!weak_self) return;
		auto* parent_window = dynamic_cast<MainWindow*>(weak_self->GetParent());
		if (!parent_window) return;

		parent_window->waitForUnsubscribedNumbers();
		if (weak_self) weak_self->CallAfter([this, weak_self, parent_window]() {
			if (!weak_self) return;
			const auto unsubscribed_numbers = parent_window->getUnsubscribedNumbers();
			if (unsubscribed_numbers.empty()) {
				UnsubscribedNumbersTextbox->SetValue(_("No unsubscribed numbers found."));
			} else {
				std::string unsubscribed_numbers_text;
				for (const auto& number : unsubscribed_numbers) {
					unsubscribed_numbers_text += number + "\n";
				}
				UnsubscribedNumbersTextbox->SetValue(unsubscribed_numbers_text);
			}
		});
	}).detach();

	this->SetSizer(Container);
	this->Layout();

	this->Centre(wxBOTH);
}

} // namespace Siren::GUI
//NOLINTEND(cppcoreguidelines-owning-memory)
