CXX         := g++
CXXFLAGS    := -std=gnu++23 -Wall -Wextra -Wpedantic -O2 -MMD -MP -g
LDFLAGS     :=

HOST_OS     := $(shell uname)

BUILDDIR    := bin
OBJDIR      := $(BUILDDIR)/obj
SRCDIR      := src
ICON_SRC    := $(CURDIR)/siren.ico
RC          ?= windres
RCFLAGS     ?= --use-temp-file -J rc -O coff

SRCS        := $(shell find $(SRCDIR) -name '*.cpp')
OBJS        := $(patsubst $(SRCDIR)/%.cpp,$(OBJDIR)/%.o,$(SRCS))

# If the HOST_OS strings contains "NT", we're on Windows
# check if the string contains "NT" (case-insensitive) to determine if we're on Windows
ifeq ($(findstring NT,$(HOST_OS)),NT)
# Static link everything
CXXFLAGS += $(shell pkg-config --static --cflags libcurl)
CXXFLAGS += $(shell wx-config --static --cxxflags)
LDFLAGS  += $(shell wx-config --static --libs)
LDFLAGS  += $(shell pkg-config --static --libs libcurl)
LDFLAGS  += -static-libgcc -static-libstdc++ -static -mwindows
ifneq ($(strip $(shell command -v $(RC) 2>/dev/null)),)
RES_SRC  := siren.rc
RES_OBJ  := $(OBJDIR)/siren.res.o
OBJS     += $(RES_OBJ)
else
$(warning windres not found; skipping embedded Windows icon)
endif
 else
# Standard dynamic linking for GNU/Linux: assume a package manager will handle dependencies
CXXFLAGS += $(shell pkg-config --cflags libcurl)
CXXFLAGS += $(shell wx-config --cxxflags)
LDFLAGS  += $(shell wx-config --libs)
LDFLAGS  += $(shell pkg-config --libs libcurl)
endif

TARGET      := $(BUILDDIR)/siren

$(TARGET): $(OBJS)
	@mkdir -p $(BUILDDIR)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJDIR)/%.o: $(SRCDIR)/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -c -o $@ $<

# Windows resource compilation
$(RES_OBJ): $(RES_SRC) $(ICON_SRC)
	@mkdir -p $(dir $@)
	$(RC) $(RCFLAGS) -i $< -o $@

.PHONY: macpkg macdmg clean
macpkg: $(TARGET)
	mkdir -p bin/macos/Siren.app/Contents/MacOS
	mkdir -p bin/macos/Siren.app/Contents/Resources
	echo "APPL????" > bin/macos/Siren.app/Contents/PkgInfo
	cp Info.plist bin/macos/Siren.app/Contents/
	cp bin/siren bin/macos/Siren.app/Contents/MacOS/
	cp $(ICON_SRC) bin/macos/Siren.app/Contents/Resources/siren.ico

macdmg: macpkg
	ln -s /Applications "bin/macos/Drag Siren here"
	hdiutil create bin/Install-Siren.dmg -ov -volname "Install Siren" -fs HFS+ -srcfolder bin/macos/
	hdiutil convert bin/Install-Siren.dmg -format UDZO -o bin/Siren.dmg
	rm -f bin/Install-Siren.dmg

clean:
	@rm -rf $(BUILDDIR)

-include $(OBJS:.o=.d)
