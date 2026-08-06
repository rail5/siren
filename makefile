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
	mkdir -p bin/macos/Siren.app/Contents/Frameworks
	echo "APPL????" > bin/macos/Siren.app/Contents/PkgInfo
	cp Info.plist bin/macos/Siren.app/Contents/
	cp bin/siren bin/macos/Siren.app/Contents/MacOS/
	cp $(ICON_SRC) bin/macos/Siren.app/Contents/Resources/siren.ico
	@set -e; \
	APP="bin/macos/Siren.app"; \
	APP_BIN="$$APP/Contents/MacOS/siren"; \
	FW_DIR="$$APP/Contents/Frameworks"; \
	resolve_dep_path() { \
		dep="$$1"; \
		case "$$dep" in \
			/System/Library/*|/usr/lib/*|/usr/libexec/*|/Library/Frameworks/*|/System/Library/Frameworks/*) \
				return 0; \
				;; \
			@rpath/*|@loader_path/*|@executable_path/*) \
				dep_name="$$dep"; \
				case "$$dep_name" in \
					@rpath/*) dep_name="$${dep_name#@rpath/}" ;; \
					@loader_path/*) dep_name="$${dep_name#@loader_path/}" ;; \
					@executable_path/*) dep_name="$${dep_name#@executable_path/}" ;; \
				esac; \
				dep_name="$$(basename "$$dep_name")"; \
				;; \
			/*) \
				printf '%s\n' "$$dep"; \
				return 0; \
				;; \
			*) \
				dep_name="$$(basename "$$dep")"; \
				;; \
			esac; \
		for prefix in /opt/homebrew /usr/local /opt/local; do \
			if [ -d "$$prefix" ]; then \
				found="$$(find -L "$$prefix" \( -type f -o -type l \) -name "$$dep_name" 2>/dev/null | head -n 1)"; \
				if [ -n "$$found" ]; then \
					printf '%s\n' "$$found"; \
					return 0; \
				fi; \
				stem="$${dep_name%.dylib}"; \
				if [ "$$stem" != "$$dep_name" ]; then \
					found="$$(find -L "$$prefix" \( -type f -o -type l \) -name "$$stem*.dylib" 2>/dev/null | head -n 1)"; \
					if [ -n "$$found" ]; then \
						printf '%s\n' "$$found"; \
						return 0; \
					fi; \
				fi; \
			fi; \
		done; \
	}; \
	get_non_system_deps() { \
		target="$$1"; \
		otool -L "$$target" | awk 'NR > 1 {print $$1}' | while IFS= read -r dep; do \
			case "$$dep" in \
				/System/Library/*|/usr/lib/*|/usr/libexec/*|/Library/Frameworks/*|/System/Library/Frameworks/*) \
					continue; \
					;; \
				@rpath/*|@loader_path/*|@executable_path/*) \
					dep_name="$$dep"; \
					case "$$dep_name" in \
						@rpath/*) dep_name="$${dep_name#@rpath/}" ;; \
						@loader_path/*) dep_name="$${dep_name#@loader_path/}" ;; \
						@executable_path/*) dep_name="$${dep_name#@executable_path/}" ;; \
					esac; \
					dep_name="$$(basename "$$dep_name")"; \
					;; \
				*) \
					dep_name="$$(basename "$$dep")"; \
					;; \
				esac; \
			resolved="$$(resolve_dep_path "$$dep")"; \
			if [ -n "$$resolved" ]; then \
				printf '%s\n' "$$dep"; \
			fi; \
		done; \
	}; \
	pending="$$APP_BIN"; \
	seen=""; \
	while [ -n "$$pending" ]; do \
		set -- $$pending; \
		pending=""; \
		for target in "$$@"; do \
			target_base="$$(basename "$$target")"; \
			case " $$seen " in \
				*" $$target_base "*) continue ;; \
			esac; \
			seen="$$seen $$target_base"; \
			for dep in $$(get_non_system_deps "$$target"); do \
				dep_name="$$(basename "$$dep")"; \
				case "$$dep_name" in \
					@rpath/*) dep_name="$${dep_name#@rpath/}" ;; \
					@loader_path/*) dep_name="$${dep_name#@loader_path/}" ;; \
					@executable_path/*) dep_name="$${dep_name#@executable_path/}" ;; \
					esac; \
				resolved="$$(resolve_dep_path "$$dep")"; \
				if [ -n "$$resolved" ]; then \
					if [ ! -f "$$FW_DIR/$$dep_name" ]; then \
						cp -f "$$resolved" "$$FW_DIR/$$dep_name"; \
						chmod u+w "$$FW_DIR/$$dep_name"; \
					fi; \
					pending="$$pending $$FW_DIR/$$dep_name"; \
				fi; \
			done; \
		done; \
	done; \
	install_name_tool -add_rpath "@executable_path/../Frameworks" "$$APP_BIN" 2>/dev/null || true; \
	for dylib in "$$FW_DIR"/*.dylib; do \
		[ -e "$$dylib" ] || continue; \
		base="$$(basename "$$dylib")"; \
		install_name_tool -add_rpath "@loader_path" "$$dylib" 2>/dev/null || true; \
		install_name_tool -id "@rpath/$$base" "$$dylib"; \
	done; \
	for target in "$$APP_BIN" "$$FW_DIR"/*.dylib; do \
		[ -e "$$target" ] || continue; \
		for dep in $$(get_non_system_deps "$$target"); do \
			dep_name="$$(basename "$$dep")"; \
			case "$$dep_name" in \
				@rpath/*) dep_name="$${dep_name#@rpath/}" ;; \
				@loader_path/*) dep_name="$${dep_name#@loader_path/}" ;; \
				@executable_path/*) dep_name="$${dep_name#@executable_path/}" ;; \
				esac; \
			if [ -f "$$FW_DIR/$$dep_name" ]; then \
				install_name_tool -change "$$dep" "@rpath/$$dep_name" "$$target"; \
			fi; \
		done; \
	done

macdmg: macpkg
	ln -s /Applications "bin/macos/Drag Siren here"
	hdiutil create bin/Install-Siren.dmg -ov -volname "Install Siren" -fs HFS+ -srcfolder bin/macos/
	hdiutil convert bin/Install-Siren.dmg -format UDZO -o bin/Siren.dmg
	rm -f bin/Install-Siren.dmg

clean:
	@rm -rf $(BUILDDIR)

-include $(OBJS:.o=.d)
