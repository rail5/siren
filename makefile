CXX         := g++
CXXFLAGS    := -std=gnu++23 -Wall -Wextra -Wpedantic -O2 -MMD -MP -g
LDFLAGS     :=

HOST_OS     := $(shell uname)

BUILDDIR    := bin
OBJDIR      := $(BUILDDIR)/obj
SRCDIR      := src

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
RES_SRC  := siren.rc
RES_OBJ  := $(OBJDIR)/siren.res.o
OBJS     += $(RES_OBJ)
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
$(RES_OBJ): $(RES_SRC)
	@mkdir -p $(dir $@)
	windres -i $< -o $@

.PHONY: clean
clean:
	@rm -rf $(BUILDDIR)

-include $(OBJS:.o=.d)
