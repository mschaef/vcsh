
.PHONY: vcalc ectworks tested vcsh vm util clean

ifeq (${BUILD_VCALC}, YES)
all: vcalc
else
all: vcsh
endif

vcalc: vcsh ectworks
	$(MAKE) -r -C vcalc

ectworks:
	$(MAKE) -r -C ectworks

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
	$(MAKE) -r -C vcalc clean
	$(MAKE) -r -C ectworks clean
