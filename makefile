CXX         := g++
CXXFLAGS    := -std=gnu++23 -Wall -Wextra -Wpedantic -O2 -MMD -MP -g
WX_CXXFLAGS := $(shell wx-config --cxxflags)
WX_LDFLAGS  := $(shell wx-config --libs)
LDFLAGS     := -lcurl -lssl -lcrypto

BUILDDIR    := bin
OBJDIR      := $(BUILDDIR)/obj
SRCDIR      := src

SRCS        := $(shell find $(SRCDIR) -name '*.cpp')
OBJS        := $(patsubst $(SRCDIR)/%.cpp,$(OBJDIR)/%.o,$(SRCS))
TARGET      := $(BUILDDIR)/siren

$(TARGET): $(OBJS)
	@mkdir -p $(BUILDDIR)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(WX_LDFLAGS) $(LDFLAGS)

$(OBJDIR)/%.o: $(SRCDIR)/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) $(WX_CXXFLAGS) -c -o $@ $<

.PHONY: clean
clean:
	@rm -rf $(BUILDDIR)

-include $(OBJS:.o=.d)
