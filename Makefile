
.PHONY: vm util

all: vcsh

tested: vcsh
	$(MAKE) -r -C scheme-core tested

vcsh: vm
	$(MAKE) -r -C scheme-core

vm: util
	$(MAKE) -r -C vm

util:
	$(MAKE) -r -C util

clean:
	$(MAKE) -r -C vm clean
	$(MAKE) -r -C util clean
	$(MAKE) -r -C scheme-core clean
	$(MAKE) -r -C scheme-core clean-scheme
