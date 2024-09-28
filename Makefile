# Run 'make' on GNU/Linux or MacOS to build native binaries to the bin/ folder (see 'Build Requirements' in README)

# Run 'make windows' from GNU/Linux with MXE & Lazarus configured to build Win64 binaries to the bin/ folder

# Run 'make macos' from MacOS to build native binaries to the bin/ & create a .App bundle in the root folder

# And of course 'make install' from GNU/Linux if desired

# MACHINETYPE: Output of 'uname -s' (Tells us whether we're working on *nix or MacOS)
MACHINETYPE=$(shell uname)

# CROSS: The MXE (cross-compilation) prefix (for cross-compiling to windows) [keep default for Win64 builds]
CROSS=x86_64-w64-mingw32.static

# MXEDIRECTORY: Root directory of your MXE installation (for cross-compiling to windows)
MXEDIRECTORY=/opt/mxe/

# LAZDIR: Root directory of your Lazarus installation (for building the GUI)
LAZDIR=/usr/lib/lazarus/2.2.6

# MACLAZDIR: Root directory of your Lazarus installation IF you're on MacOS
MACLAZDIR=/Applications/Lazarus

# Intentionally blank unless we detect MacOS
MACFLAGS=

# XLAZTARGET: Lazarus target for cross-compilation (only used by 'windows' make target)
XLAZTARGET=win64


XCC=$(CROSS)-g++
XPKG_CONFIG=$(CROSS)-pkg-config
XINCLUDE= -Iinclude -I$(MXEDIRECTORY)/usr/$(CROSS)/include
XLDFLAGS= -L$(MXEDIRECTORY)/usr/$(CROSS)/lib
XCURLCONFIG=`$(MXEDIRECTORY)/usr/bin/$(CROSS)-curl-config --libs`
XLDLIBS= -DCURL_STATICLIB $(XCURLCONFIG)

CXX = g++
CFLAGS = -Wall -std=c++17 -O2
INCLUDE = -Iinclude -I/usr/local/include -I/usr/include
LDFLAGS = -L/usr/local/lib -I/usr/lib
LDLIBS = -lcurl

SOURCES = src/siren-core.cpp src/twilio.cc
OUT = bin/siren


all: core frontend

core: $(SOURCES)
	$(CXX) -o $(OUT) $(INCLUDE) $(LDFLAGS) $(CFLAGS) $(SOURCES) $(LDLIBS)

frontend:
# Change LAZDIR and set widgetset to cocoa if we're on MacOS
ifeq ($(MACHINETYPE),Darwin)
	$(eval LAZDIR := $(MACLAZDIR))
	$(eval MACFLAGS := --widgetset=cocoa)
endif
	$(LAZDIR)/lazbuild --lazarusdir=$(LAZDIR) $(MACFLAGS) --build-mode=Release src/gui/siren-gui.lpr
	mv src/gui/siren-gui bin/siren-gui

windows: wincore winfrontend wininstaller

wincore: $(SOURCES)
	$(XCC) -o $(OUT).exe $(XINCLUDE) $(XLDFLAGS) $(CFLAGS) $(SOURCES) $(XLDLIBS)

winfrontend:
	lazbuild --lazarusdir=$(LAZDIR) --build-mode=Release --operating-system=$(XLAZTARGET) src/gui/siren-gui.lpr
	mv src/gui/siren-gui.exe bin/siren-gui.exe

wininstaller:
	wine ~/.wine/drive_c/Program\ Files\ \(x86\)/Inno\ Setup\ 6/ISCC.exe ./siren-installer.iss

macos: core frontend macpkg macdmg

macpkg:
	mkdir -p bin/macos/Siren.app/Contents/MacOS
	mkdir -p bin/macos/Siren.app/Contents/Resources
	echo "APPL????" > bin/macos/Siren.app/Contents/PkgInfo
	cp Info.plist bin/macos/Siren.app/Contents/
	cp bin/siren bin/macos/Siren.app/Contents/MacOS/
	cp bin/siren-gui bin/macos/Siren.app/Contents/MacOS/

macdmg:
	ln -s /Applications "bin/macos/Drag Siren here"
	hdiutil create bin/Install-Siren.dmg -ov -volname "Install Siren" -fs HFS+ -srcfolder bin/macos/
	hdiutil convert bin/Install-Siren.dmg -format UDZO -o bin/Siren.dmg
	rm -f bin/Install-Siren.dmg

install:
	echo placeholder

clean:
	rm -rf bin/siren bin/siren.exe bin/siren-gui bin/siren-gui.exe bin/Siren-Installer.exe src/gui/lib bin/macos bin/Siren.dmg bin/Install-Siren.dmg
