
# Makefile for Scan VM

.PHONY: indented

TARGETS=to-c-source${EXE_EXT} show-retval${EXE_EXT} scan-vm${LIB_EXT} scansh0${EXE_EXT} fasl-dump${EXE_EXT} constants.scm TAGS

include ../build-settings

SCC0=scc0/scc0

scansh0${EXE_EXT}: scan-vm${LIB_EXT} scansh0${OBJ_EXT}
	${CC} ${LDFLAGS} ${NAMOBJFL}scansh0${EXE_EXT} scansh0${OBJ_EXT} scan-vm${LIB_EXT}

SH0_OBJS=scansh0${OBJ_EXT}

SH0_SRCS:= $(SH0_OBJS:${OBJ_EXT}=.cpp)

VM_OBJS=$(PLATFORM)-sys${OBJ_EXT} \
       constants${OBJ_EXT} \
       core${OBJ_EXT} \
       debug-printer${OBJ_EXT} \
       debug-tools${OBJ_EXT} \
       error${OBJ_EXT} \
       evaluator${OBJ_EXT} \
       fasl${OBJ_EXT} \
       global-env${OBJ_EXT} \
       hash-table${OBJ_EXT} \
       instance${OBJ_EXT} \
       io${OBJ_EXT} \
       io-blocking-input${OBJ_EXT} \
       io-external-file${OBJ_EXT} \
       io-internal-file${OBJ_EXT} \
       io-string${OBJ_EXT} \
       list${OBJ_EXT} \
       macro${OBJ_EXT} \
       main${OBJ_EXT} \
       malloc${OBJ_EXT} \
       memory${OBJ_EXT} \
       mt19937${OBJ_EXT} \
       number${OBJ_EXT} \
       oblist${OBJ_EXT} \
       structure${OBJ_EXT} \
       system${OBJ_EXT} \
       text-data${OBJ_EXT} \
       vector${OBJ_EXT}

VM_SRCS:= $(VM_OBJS:${OBJ_EXT}=.cpp)

FASL_DUMP_OBJS=fasl-dump${OBJ_EXT} constants${OBJ_EXT} $(PLATFORM)-sys${OBJ_EXT} malloc${OBJ_EXT}

FASL_DUMP_SRCS:= $(FASL_DUMP_OBJS:${OBJ_EXT}=.cpp)

fasl-dump${EXE_EXT}: $(FASL_DUMP_OBJS)
	${CC} ${LDFLAGS} ${NAMOBJFL}fasl-dump${EXE_EXT}  $(FASL_DUMP_OBJS)

scan-vm${LIB_EXT}: $(VM_OBJS)
	${AR_RCS} ${AROUTFL}scan-vm${LIB_EXT} $(VM_OBJS)
ifdef ${RANLIB}
	${RANLIB} scan-vm${LIB_EXT}
endif


TO_C_SOURCE_OBJS=to-c-source${OBJ_EXT}

TO_C_SOURCE_SRCS:=$(TO_C_SOURCE_OBJS:${OBJ_EXT}=.cpp)

to-c-source${EXE_EXT}: $(TO_C_SOURCE_OBJS)
	${CC} ${LDFLAGS} ${NAMOBJFL}to-c-source${EXE_EXT} $(TO_C_SOURCE_OBJS)

SHOW_RETVAL_OBJS=show-retval${OBJ_EXT}

SHOW_RETVAL_SRCS:=$(SHOW_RETVAL_OBJS:${OBJ_EXT}=.cpp)

show-retval${EXE_EXT}: $(SHOW_RETVAL_OBJS)
	${CC} ${LDFLAGS} ${NAMOBJFL}show-retval${EXE_EXT} $(SHOW_RETVAL_OBJS)

constants.scm: constants.i
	cpp -DCONST_SCHEME -P $< $@

depend:
	vcsh grovel-dependancies.scm scheme.scm 1> .depend

indented:
	gnuindent ${VM_SRCS} mt19937.h scan.h sys.h constants.i base-types.h

clean-local:
	rm -f constants.scm

%${OBJ_EXT}: %.cpp Makefile scan.h
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}$@ $<

ifneq ($(wildcard .depend),)
include .depend
endif

