
# Makefile --
#
# The sub-Makefile for the Scheme image, vcsh0, and vcsh.
#
# (C) Copyright 2001-2015 East Coast Toolworks Inc.
# (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

.PHONY: indented tested tested-aot

TARGETS=scanlib.a vcsh

include ../config.mk

SCC0=../vm/scansh0 -Xinit-load=../scc0/scheme.scf -Xinit-load=../scc0/compiler-run.scf

# Generated C sources (images converted by to-c-source), their objects,
# and the standard library registration file all go here. Removed by
# 'make clean'.
GEN_DIR=build

tested-aot: tested scheme-tests.scf
	 ./vcsh scheme-tests.scf

tested: all
	./vcsh scheme-tests.scm

scheme-tests.scf: vcsh scheme-tests.scm
	./vcsh -c scheme-tests.scm

SCHEME_MODULES=csv.scm autoload.scm unit-test.scm unit-test-utils.scm

SCHEME_MODULES_C:=$(addprefix ${GEN_DIR}/,$(SCHEME_MODULES:.scm=.c))

SCHEME_MODULES_O:=$(SCHEME_MODULES_C:.c=.o)

vcsh: vcsh0 vcsh.o ${SCHEME_MODULES_O}
	$(CC) $(LDFLAGS) -o $@ vcsh.o ${SCHEME_MODULES_O} scanlib.a $(LDLIBS)

${GEN_DIR}/vcsh-standard-lib-registration.i: gen-register-calls.scm vcsh0 ${SCHEME_MODULES_C}
	./vcsh0 gen-register-calls.scm ${SCHEME_MODULES_C} > $@

vcsh0: scanlib.a vcsh0.o
	$(CC) $(LDFLAGS) -o $@ vcsh0.o scanlib.a $(LDLIBS)

scanlib.a: scanlib.o ${GEN_DIR}/scheme.o ${GEN_DIR}/compiler-run.o ../vm/scan-vm.a
	cp ../vm/scan-vm.a scanlib.a
	$(AR) rs scanlib.a scanlib.o ${GEN_DIR}/scheme.o ${GEN_DIR}/compiler-run.o

# scheme.c and compiler-run.c have explicit rules because the generic
# ${GEN_DIR}/%.c rule below depends on vcsh0, which is built from them.
${GEN_DIR}/scheme.c: scheme.scf ../vm/to-c-source | ${GEN_DIR}
	echo \#include \"../../vm/scan-internal-file.h\" > $@
	../vm/to-c-source scheme.scf >> $@

scheme.scf:
	$(SCC0) --load-file:setup-image-compile.scm scheme.scm

${GEN_DIR}/compiler-run.c: compiler-run.scf ../vm/to-c-source | ${GEN_DIR}
	echo \#include \"../../vm/scan-internal-file.h\" > $@
	../vm/to-c-source compiler-run.scf >> $@

compiler-run.scf: compiler-run.scm
	$(SCC0)  --initial-package:scheme compiler-run.scm

${GEN_DIR}:
	mkdir -p ${GEN_DIR}

clean-local:
	rm -rf ${GEN_DIR} vcsh0

clean-scheme:
	rm -rf ${GEN_DIR}
	rm -f scheme.scf compiler-run.scf scanlib.a

%.o: %.c Makefile $(BUILD_FLAGS_STAMP)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

%.scf: %.scm Makefile vcsh0
	./vcsh0 --compile --output=$@ $<

# to-c-source is run from this directory so the internal file name
# embedded in the output stays a bare name (e.g. "csv.scf").
${GEN_DIR}/%.c: %.scf Makefile vcsh0 ../vm/to-c-source | ${GEN_DIR}
	echo \#include \"../../vm/scan-internal-file.h\" > $@
	../vm/to-c-source $< >> $@

depend: vcsh grovel-dependancies.scm scheme.scm
	./vcsh grovel-dependancies.scm scheme.scm 1> .depend

indented:
	gnuindent vcsh.c scanlib.c

vcsh.o:	vcsh.c ${GEN_DIR}/vcsh-standard-lib-registration.i

ifneq ($(wildcard .depend),)
include .depend
endif
