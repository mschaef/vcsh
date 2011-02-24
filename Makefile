
.PHONY: tested vcsh vm util clean indented

all: vcsh

tested: vcsh
	$(MAKE) -r -C scheme-core tested

vcsh: vm
	$(MAKE) -r -C scheme-core

vm: util
	$(MAKE) -r -C vm --jobs=2

util:
	$(MAKE) -r -C util

indented:
	$(MAKE) -r -C vm indented
	$(MAKE) -r -C util indented
	$(MAKE) -r -C scheme-core indented

clean:
	$(MAKE) -r -C vm clean
	$(MAKE) -r -C util clean
	$(MAKE) -r -C scheme-core clean
	$(MAKE) -r -C scheme-core clean-scheme
