
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

TARGETS=scanlib${LIB_EXT} vcsh${EXE_EXT}

include ../build-settings
include ../config.${PLATFORM}

SCC0=../vm/scansh0 -Xinit-load=../scc0/scheme.scf -Xinit-load=../scc0/compiler-run.scf

# Generated C sources (images converted by to-c-source), their objects,
# and the standard library registration file all go here. Removed by
# 'make clean'.
GEN_DIR=build

tested-aot: tested scheme-tests.scf
	 ./vcsh scheme-tests.scf

tested: all
	./vcsh scheme-tests.scm

scheme-tests.scf: vcsh${EXE_EXT} scheme-tests.scm
	./vcsh -c scheme-tests.scm

SCHEME_MODULES=csv.scm autoload.scm unit-test.scm unit-test-utils.scm

SCHEME_MODULES_C:=$(addprefix ${GEN_DIR}/,$(SCHEME_MODULES:.scm=.c))

SCHEME_MODULES_O:=$(SCHEME_MODULES_C:.c=${OBJ_EXT})

vcsh${EXE_EXT}: vcsh0${EXE_EXT} vcsh${OBJ_EXT} ${SCHEME_MODULES_O}
	${CC} ${NAMOBJFL}vcsh${EXE_EXT} vcsh${OBJ_EXT} ${SCHEME_MODULES_O} scanlib${LIB_EXT} ${LDFLAGS} 

${GEN_DIR}/vcsh-standard-lib-registration.i: gen-register-calls.scm vcsh0${EXE_EXT} ${SCHEME_MODULES_C}
	./vcsh0 gen-register-calls.scm ${SCHEME_MODULES_C} > $@

vcsh0${EXE_EXT}: scanlib${LIB_EXT} vcsh0${OBJ_EXT}
	${CC} ${NAMOBJFL}vcsh0${EXE_EXT} vcsh0${OBJ_EXT} scanlib${LIB_EXT} ${LDFLAGS} 
scanlib${LIB_EXT}: scanlib${OBJ_EXT} ${GEN_DIR}/scheme${OBJ_EXT} ${GEN_DIR}/compiler-run${OBJ_EXT} ../vm/scan-vm${LIB_EXT}
	cp ../vm/scan-vm${LIB_EXT} scanlib${LIB_EXT}
	${AR_RS} ${AROUTFL_RS}scanlib${LIB_EXT} scanlib${OBJ_EXT} ${GEN_DIR}/scheme${OBJ_EXT} ${GEN_DIR}/compiler-run${OBJ_EXT}
ifdef ${RANLIB}
	${RANLIB} scanlib${LIB_EXT}
endif

# scheme.c and compiler-run.c have explicit rules because the generic
# ${GEN_DIR}/%.c rule below depends on vcsh0, which is built from them.
${GEN_DIR}/scheme.c: scheme.scf ../vm/to-c-source${EXE_EXT} | ${GEN_DIR}
	echo \#include \"../../vm/scan-internal-file.h\" > $@
	../vm/to-c-source scheme.scf >> $@

scheme.scf:
	$(SCC0) --load-file:setup-image-compile.scm scheme.scm

${GEN_DIR}/compiler-run.c: compiler-run.scf ../vm/to-c-source${EXE_EXT} | ${GEN_DIR}
	echo \#include \"../../vm/scan-internal-file.h\" > $@
	../vm/to-c-source compiler-run.scf >> $@

compiler-run.scf: compiler-run.scm
	$(SCC0)  --initial-package:scheme compiler-run.scm

${GEN_DIR}:
	mkdir -p ${GEN_DIR}

clean-local:
	${RM_F} -r ${GEN_DIR}

clean-scheme:
	${RM_F} -r ${GEN_DIR}
	${RM_F} scheme.scf compiler-run.scf
	${RM_F} scanlib.a scanlib.lib

%${OBJ_EXT}: %.c Makefile
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}$@ $<

%.scf: %.scm Makefile vcsh0${EXE_EXT}
	./vcsh0${EXE_EXT} --compile --output=$@ $<

# to-c-source is run from this directory so the internal file name
# embedded in the output stays a bare name (e.g. "csv.scf").
${GEN_DIR}/%.c: %.scf Makefile vcsh0${EXE_EXT} ../vm/to-c-source${EXE_EXT} | ${GEN_DIR}
	echo \#include \"../../vm/scan-internal-file.h\" > $@
	../vm/to-c-source $< >> $@

depend: vcsh${EXE_EXT} grovel-dependancies.scm scheme.scm
	./vcsh grovel-dependancies.scm scheme.scm 1> .depend

indented:
	gnuindent vcsh.c scanlib.c

vcsh${OBJ_EXT}:	vcsh.c ${GEN_DIR}/vcsh-standard-lib-registration.i

ifneq ($(wildcard .depend),)
include .depend
endif
