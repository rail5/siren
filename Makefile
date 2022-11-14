# Run 'make' on Linux/MacOS to build native binaries (see 'Build Requirements' in README)
# Run 'make windows' from Linux with MXE & Lazarus configured to build Win64 binaries

# CROSS: The MXE (cross-compilation) prefix [keep default for Win64 builds]
CROSS=x86_64-w64-mingw32.static

# MXEDIRECTORY: Root directory of your MXE installation
MXEDIRECTORY=/opt/mxe/

# LAZDIR: Root directory of your Lazarus installation (for building the GUI)
LAZDIR=/usr/lib/lazarus/2.0.10

# XLAZTARGET: Lazarus target for cross-compilation
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
	lazbuild --lazarusdir=$(LAZDIR) --build-mode=Release src/gui/siren-gui.lpr
	mv src/gui/siren-gui bin/siren-gui

windows: wincore winfrontend

wincore: $(SOURCES)
	$(XCC) -o $(OUT).exe $(XINCLUDE) $(XLDFLAGS) $(CFLAGS) $(SOURCES) $(XLDLIBS)

winfrontend:
	lazbuild --lazarusdir=$(LAZDIR) --build-mode=Release --operating-system=$(XLAZTARGET) src/gui/siren-gui.lpr
	mv src/gui/siren-gui.exe bin/siren-gui.exe

install:
	echo placeholder

clean:
	rm -rf bin/siren bin/siren.exe bin/siren-gui bin/siren-gui.exe src/gui/lib
