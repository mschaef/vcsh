# config.mk --
#
# Build configuration shared by all the makefiles. vcsh builds with
# clang (gcc also works) on 64-bit macOS and Linux, arm64 or x86-64.
#
# Every setting below can be overridden on the command line:
#
#   make                          debug build with clang
#   make BUILD=release            optimized build
#   make BUILD=checked            -O0, plus internal consistency checks
#   make SANITIZE=address,undefined
#   make COVERAGE=yes             clang source-based coverage
#   make CC=gcc
#
# or persistently in an untracked local.mk next to this file, using the
# same syntax (e.g. 'BUILD = release').
#
# Changing a setting changes the compiler flags, and every object file
# depends on a stamp of those flags (.build-flags), so the next build
# recompiles what it needs to. No 'make clean' is needed when switching.
#
# (C) Copyright 2001-2026 East Coast Toolworks Inc.
# (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
#
# See the file "LICENSE" for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL
# WARRANTIES.

TOP := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

-include $(TOP)/local.mk

#### Settings

# debug | release | checked
BUILD ?= debug

# Comma-separated clang sanitizers, passed to -fsanitize=. Empty for none.
SANITIZE ?=

# yes: instrument for clang source-based coverage (see 'make coverage').
COVERAGE ?= no

# make predefines CC as 'cc', so ?= wouldn't take effect.
ifeq ($(origin CC),default)
  CC = clang
endif

LLVM_PROFDATA ?= llvm-profdata
LLVM_COV ?= llvm-cov

#### Flags

CPPFLAGS += -I. -D_POSIX_C_SOURCE=200809L -D_DEFAULT_SOURCE
CFLAGS += -std=c11 -Wall -funsigned-char -g
LDFLAGS += -g
LDLIBS += -lm

ifeq ($(BUILD),debug)
  CPPFLAGS += -D_DEBUG
  CFLAGS += -O1
else ifeq ($(BUILD),checked)
  CPPFLAGS += -D_DEBUG -DCHECKED
  CFLAGS += -O0
else ifeq ($(BUILD),release)
  CFLAGS += -O3
else
  $(error Unknown BUILD '$(BUILD)': use debug, release or checked)
endif

ifneq ($(SANITIZE),)
  # Stop at the first problem, rather than reporting it and continuing,
  # so that 'make SANITIZE=... tested' fails on any finding.
  SANITIZE_FLAGS := -fsanitize=$(SANITIZE) -fno-sanitize-recover=all \
                    -fno-omit-frame-pointer
  # The garbage collector scans the C stack conservatively. ASan's
  # 'fake stack' for detecting use-after-return moves locals off the
  # real stack, where the scan can't see them.
  ifneq ($(findstring address,$(SANITIZE)),)
    ifneq ($(findstring clang,$(shell $(CC) --version 2>/dev/null)),)
      SANITIZE_FLAGS += -fsanitize-address-use-after-return=never
    else
      SANITIZE_FLAGS += --param=asan-use-after-return=0
    endif
  endif
  CFLAGS += $(SANITIZE_FLAGS)
  LDFLAGS += $(SANITIZE_FLAGS)
endif

ifeq ($(COVERAGE),yes)
  CFLAGS += -fprofile-instr-generate -fcoverage-mapping
  LDFLAGS += -fprofile-instr-generate
else ifneq ($(COVERAGE),no)
  $(error Unknown COVERAGE '$(COVERAGE)': use yes or no)
endif

#### Flag stamp
#
# Rewritten only when the flags change, so that objects depending on it
# are rebuilt then and only then.

BUILD_FLAGS_STAMP := $(TOP)/.build-flags
BUILD_FLAGS := $(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(LDLIBS)

# (Quoted, because the flags can contain commas.)
ifneq "$(BUILD_FLAGS)" "$(shell cat $(BUILD_FLAGS_STAMP) 2>/dev/null)"
  $(shell echo '$(BUILD_FLAGS)' > $(BUILD_FLAGS_STAMP))
endif

#### Standard targets

.PHONY: all todo clean clean-local

all: $(TARGETS)

todo:
	ack --noheading 'TODO|REVISIT|XXX'

clean: clean-local
	rm -f *.o *.a *.scf *.profraw *~ TAGS $(TARGETS)

# etags isn't always installed (macOS no longer ships it), so TAGS
# is skipped rather than failing the build when it's missing.
TAGS:
	@if command -v etags >/dev/null 2>&1; then \
	     etags *.[ch]; \
	else \
	     echo "etags not found; skipping TAGS"; \
	fi
