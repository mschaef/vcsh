
# Makefile --
#
# The sub-Makefile for the Scheme image, vcsh0, and vcsh.
#
# (C) Copyright 2001-2011 East Coast Toolworks Inc.
# (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
#
# See the file "license.terms" for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL
# WARRANTIES.

.PHONY: indented tested

TARGETS=scanlib${LIB_EXT} vcsh${EXE_EXT}

include ../build-settings
include ../config.${PLATFORM}

SCC0=../vm/scansh0 -Xinit-load=../scc0/scheme.scf -Xinit-load=../scc0/compiler-run.scf

tested: all scheme-tests.scf
	 ./vcsh scheme-tests.scf

scheme-tests.scf: vcsh${EXE_EXT} scheme-tests.scm
	./vcsh -c scheme-tests.scm

SCHEME_MODULES=csv.scm autoload.scm unit-test.scm unit-test-utils.scm

SCHEME_MODULES_C:=$(SCHEME_MODULES:.scm=.c)

SCHEME_MODULES_O:=$(SCHEME_MODULES:.scm=${OBJ_EXT})

vcsh${EXE_EXT}: vcsh0${EXE_EXT} vcsh${OBJ_EXT} ${SCHEME_MODULES_O}
	${CC} ${NAMOBJFL}vcsh${EXE_EXT} vcsh${OBJ_EXT} ${SCHEME_MODULES_O} scanlib${LIB_EXT} ${LDFLAGS} 

vcsh-standard-lib-registration.i: gen-register-calls.scm vcsh0${EXE_EXT} ${SCHEME_MODULES_C}
	./vcsh0 gen-register-calls.scm ${SCHEME_MODULES_C} > vcsh-standard-lib-registration.i

vcsh0${EXE_EXT}: scanlib${LIB_EXT} vcsh0${OBJ_EXT}
	${CC} ${NAMOBJFL}vcsh0${EXE_EXT} vcsh0${OBJ_EXT} scanlib${LIB_EXT} ${LDFLAGS} 
scanlib${LIB_EXT}: scanlib${OBJ_EXT} scheme${OBJ_EXT} compiler-run${OBJ_EXT} ../vm/scan-vm${LIB_EXT}
	cp ../vm/scan-vm${LIB_EXT} scanlib${LIB_EXT}
	${AR_RS} ${AROUTFL_RS}scanlib${LIB_EXT} scanlib${OBJ_EXT} scheme${OBJ_EXT} compiler-run${OBJ_EXT}
ifdef ${RANLIB}
	${RANLIB} scanlib${LIB_EXT}
endif

scheme${OBJ_EXT}: scheme.scf
	echo \#include \"../vm/scan-internal-file.h\" > scheme.c
	../vm/to-c-source scheme.scf >> scheme.c
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}scheme${OBJ_EXT} scheme.c

scheme.scf:
	$(SCC0) --load-file:setup-image-compile.scm scheme.scm

compiler-run${OBJ_EXT}: compiler-run.scf
	echo \#include \"../vm/scan-internal-file.h\" > compiler-run.c
	../vm/to-c-source compiler-run.scf >> compiler-run.c
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}compiler-run${OBJ_EXT} compiler-run.c

compiler-run.scf: compiler-run.scm
	$(SCC0)  --initial-package:scheme compiler-run.scm

clean-scheme:
	${RM_F} scheme${OBJ_EXT} compiler-run${OBJ_EXT}
	${RM_F} scheme.c scheme.scf
	${RM_F} compiler-run.c compiler-run.scf
	${RM_F} scanlib.a scanlib.lib
	${RM_F} vcsh-standard-lib-registration.i

%${OBJ_EXT}: %.c Makefile
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}$@ $<

%.scf: %.scm Makefile vcsh0${EXE_EXT}
	./vcsh0${EXE_EXT} --compile --output=$@ $<

%.c: %.scf Makefile vcsh0${EXE_EXT} ../vm/to-c-source${EXE_EXT}
	echo \#include \"../vm/scan-internal-file.h\" > $@
	../vm/to-c-source $< >> $@

depend: vcsh${EXE_EXT} grovel-dependancies.scm scheme.scm
	./vcsh grovel-dependancies.scm scheme.scm 1> .depend

indented:
	gnuindent vcsh.c scanlib.c

vcsh${OBJ_EXT}:	vcsh.c vcsh-standard-lib-registration.i

ifneq ($(wildcard .depend),)
include .depend
endif
