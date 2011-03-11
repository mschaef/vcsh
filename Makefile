
include build-settings

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
