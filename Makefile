
# Makefile --
#
# The master makefile for the project.
#
# Build settings (BUILD, CC, SANITIZE, COVERAGE) are described in
# config.mk.
#
# (C) Copyright 2001-2026 East Coast Toolworks Inc.
# (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
#
# See the file "LICENSE" for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL
# WARRANTIES.

include config.mk

.PHONY: tested vcsh-tested vm-tested vcsh vm indented coverage bootstrap-check

all: vcsh

tested: vcsh-tested

vcsh-tested: vm-tested vcsh
	$(MAKE) -r -C scheme-core tested

vm-tested: vm
	$(MAKE) -r -C vm tested

vcsh: vm
	$(MAKE) -r -C scheme-core

vm:
	$(MAKE) -r -C vm --jobs=2

# Checks that the image reproduces itself; see scheme-core/Makefile.
bootstrap-check: vcsh
	$(MAKE) -r -C scheme-core bootstrap-check

indented:
	$(MAKE) -r -C vm indented
	$(MAKE) -r -C scheme-core indented

# Runs the tests in a coverage-instrumented build and prints a
# per-file summary. 'make coverage COVERAGE_HTML=yes' also writes an
# HTML report to coverage/html. On macOS, set
# LLVM_PROFDATA='xcrun llvm-profdata' LLVM_COV='xcrun llvm-cov'.
coverage: export LLVM_PROFILE_FILE = $(TOP)/coverage/%p-%m.profraw
coverage:
	rm -rf coverage
	$(MAKE) COVERAGE=yes tested
	$(LLVM_PROFDATA) merge -sparse coverage/*.profraw -o coverage/vcsh.profdata
	$(LLVM_COV) report scheme-core/vcsh -object vm/scansh0 \
	     -instr-profile=coverage/vcsh.profdata
ifeq ($(COVERAGE_HTML),yes)
	$(LLVM_COV) show scheme-core/vcsh -object vm/scansh0 \
	     -instr-profile=coverage/vcsh.profdata -format=html \
	     -output-dir=coverage/html
endif

clean-local:
	rm -rf coverage .build-flags
	$(MAKE) -r -C vm clean
	$(MAKE) -r -C scheme-core clean
	$(MAKE) -r -C scheme-core clean-scheme
