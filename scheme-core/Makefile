# Makefile for Scan VM

.PHONY: indented tested ctested

TARGETS=scanlib${LIB_EXT} vcsh${EXE_EXT}

include ../build-settings

SCC0=../vm/scansh0 -Xinit-load=../scc0/scheme.scf -Xinit-load=../scc0/compiler-run.scf

tested: all scheme-tests.scf
	./vcsh unit-test.scm tests/run-tests.scm

ctested: all scheme-tests.scf
	 ./vcsh scheme-tests.scf

scheme-tests.scf: vcsh${EXE_EXT} scheme-tests.scm
	./vcsh -c scheme-tests.scm

VCSH_OBJS=vcsh${OBJ_EXT}

VCSH_SRCS:= $(VCSH_OBJS:${OBJ_EXT}=.cpp)

LIB_OBJS=scanlib${OBJ_EXT}

LIB_SRCS:= $(LIB_OBJS:${OBJ_EXT}=.cpp)

vcsh${EXE_EXT}: scanlib${LIB_EXT} $(VCSH_OBJS)
	${CC} ${LDFLAGS} ${NAMOBJFL}vcsh${EXE_EXT} vcsh${OBJ_EXT} scanlib${LIB_EXT}

scanlib${LIB_EXT}: $(LIB_OBJS) scheme${OBJ_EXT} compiler-run${OBJ_EXT} ../vm/scan-vm${LIB_EXT}
	cp ../vm/scan-vm${LIB_EXT} scanlib${LIB_EXT}
	${AR_RS} ${AROUTFL_RS}scanlib${LIB_EXT} $(LIB_OBJS) scheme${OBJ_EXT} compiler-run${OBJ_EXT}
ifdef ${RANLIB}
	${RANLIB} scanlib${LIB_EXT}
endif

scheme${OBJ_EXT}: scheme.scf
	echo \#include \"../vm/base-types.h\" > scheme.cpp
	../vm/to-c-source scheme.scf scf_scheme >> scheme.cpp
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}scheme${OBJ_EXT} scheme.cpp

scheme.scf:
	$(SCC0) --load-file:setup-image-compile.scm scheme.scm

compiler-run${OBJ_EXT}: compiler-run.scf
	echo \#include \"../vm/base-types.h\" > compiler-run.cpp
	../vm/to-c-source compiler-run.scf scf_compiler_run >> compiler-run.cpp
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}compiler-run${OBJ_EXT} compiler-run.cpp

compiler-run.scf: compiler-run.scm
	$(SCC0)  --initial-package:scheme compiler-run.scm

clean-scheme:
	${RM_F} scheme${OBJ_EXT} compiler-run${OBJ_EXT}
	${RM_F} scheme.cpp scheme.scf
	${RM_F} compiler-run.cpp compiler-run.scf
	${RM_F} scanlib.a scanlib.lib

%${OBJ_EXT}: %.cpp Makefile
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}$@ $<

depend: vcsh${EXE_EXT} grovel-dependancies.scm scheme.scm
	./vcsh grovel-dependancies.scm scheme.scm 1> .depend

indented:
	gnuindent ${VCSH_SRCS} ${LIB_SRCS}

ifneq ($(wildcard .depend),)
include .depend
endif
