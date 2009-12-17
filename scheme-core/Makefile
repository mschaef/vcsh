
# Makefile for Scan VM

TARGETS=scanlib${LIB_EXT} vcsh${EXE_EXT}

include ../build-settings

SCC0=../vm/scansh0 -Xinit-load=../scc0/scheme.scf -Xinit-load=../scc0/fasl-compiler-run.scf

tested: all
	 ./vcsh unit-test.scm tests/run-tests.scm

tested-individually: all
	for file in tests/test*.scm; do \
	    ./vcsh $$file tests/run-loaded-tests.scm || exit 1; \
	done

VCSH_OBJS=vcsh${OBJ_EXT}

VCSH_SRCS:= $(VCSH_OBJS:${OBJ_EXT}=.cpp)

LIB_OBJS=scanlib${OBJ_EXT}

LIB_SRCS:= $(LIB_OBJS:${OBJ_EXT}=.cpp)

vcsh${EXE_EXT}: scanlib${LIB_EXT} $(VCSH_OBJS)
	${CC} ${LDFLAGS} ${NAMOBJFL}vcsh${EXE_EXT} vcsh${OBJ_EXT} scanlib${LIB_EXT}

scanlib${LIB_EXT}: $(LIB_OBJS) scheme${OBJ_EXT} fasl-compiler-run${OBJ_EXT} ../vm/scan-vm${LIB_EXT}
	cp ../vm/scan-vm${LIB_EXT} scanlib${LIB_EXT}
	${AR_RS} ${AROUTFL_RS}scanlib${LIB_EXT} $(LIB_OBJS) scheme${OBJ_EXT} fasl-compiler-run${OBJ_EXT}
ifdef ${RANLIB}
	${RANLIB} scanlib${LIB_EXT}
endif

scheme${OBJ_EXT}: scheme.scf
	../util/to-c-source scheme.scf scmSCore > scheme.cpp
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}scheme${OBJ_EXT} scheme.cpp

scheme.scf:
	$(SCC0) --cross-compile --initial-package:scheme scheme.scm

fasl-compiler-run${OBJ_EXT}: fasl-compiler-run.scf
	../util/to-c-source fasl-compiler-run.scf scmFaslCompilerRun > fasl-compiler-run.cpp
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}fasl-compiler-run${OBJ_EXT} fasl-compiler-run.cpp

fasl-compiler-run.scf: fasl-compiler-run.scm
	$(SCC0) --cross-compile --initial-package:scheme fasl-compiler-run.scm

clean-scheme:
	${RM_F} scheme${OBJ_EXT} fasl-compiler-run${OBJ_EXT}
	${RM_F} scheme.cpp scheme.scf
	${RM_F} fasl-compiler-run.cpp fasl-compiler-run.scf
	${RM_F} scanlib.a scanlib.lib

%${OBJ_EXT}: %.cpp Makefile
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}$@ $<

depend: vcsh${EXE_EXT} grovel-dependancies.scm scheme.scm
	./vcsh grovel-dependancies.scm scheme.scm 1> .depend

ifneq ($(wildcard .depend),)
include .depend
endif

