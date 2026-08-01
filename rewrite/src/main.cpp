/**
 * Copyright (C) 2026 Andrew S. Rightenburg
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include "SirenGUI.h"
#include <wx/wx.h>
#include <wx/app.h>

class SirenApp : public wxApp {
public:
	bool OnInit() override {
		auto* mainWindow = new Siren::GUI::MainWindow(nullptr);
		mainWindow->Show(true);
		return true;
	}
};

wxIMPLEMENT_APP(SirenApp);
