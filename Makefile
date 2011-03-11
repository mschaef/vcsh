
# Makefile --
#
# The master makefile for the project.
#
# (C) Copyright 2001-2011 East Coast Toolworks Inc.
# (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
#
# See the file "license.terms" for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL
# WARRANTIES.

include build-settings
include config.${PLATFORM}

.PHONY: tested vcsh vm clean indented

all: vcsh

tested: vcsh
	$(MAKE) -r -C scheme-core tested

vcsh: vm
	$(MAKE) -r -C scheme-core

vm:
	$(MAKE) -r -C vm --jobs=2

indented:
	$(MAKE) -r -C vm indented
	$(MAKE) -r -C scheme-core indented

clean-local:
	rm -f *.scf *~
	$(MAKE) -r -C vm clean
	$(MAKE) -r -C scheme-core clean
	$(MAKE) -r -C scheme-core clean-scheme
