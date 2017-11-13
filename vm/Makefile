# Makefile --
#
# The sub-Makefile for the VM and some support tools for the
# image build.
#
# (C) Copyright 2001-2011 East Coast Toolworks Inc.
# (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
#
# See the file "license.terms" for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL
# WARRANTIES.

.PHONY: all indented coverage tested

TARGETS=to-c-source${EXE_EXT} show-retval${EXE_EXT} scan-vm${LIB_EXT} scansh0${EXE_EXT} \
        fasl-dump${EXE_EXT} scan-constants.scm TAGS

SCAN_HEADERS=scan-base.h scan-constants.h scan-internal-file.h scan-private.h \
             scan-sys.h scan-types.h scan.h scan-constants.i

include ../build-settings
include ../config.${PLATFORM}

tested: all ./scansh0${EXE_EXT}
	./scansh0${EXE_EXT} -Xdebug-flags=test-vm

SCC0=scc0/scc0

VM_OBJS=$(PLATFORM)-sys${OBJ_EXT} \
       character${OBJ_EXT} \
       constants${OBJ_EXT} \
       core${OBJ_EXT} \
       debug-printer${OBJ_EXT} \
       debug-tools${OBJ_EXT} \
       error${OBJ_EXT} \
       evaluator${OBJ_EXT} \
       fasl${OBJ_EXT} \
       global-env${OBJ_EXT} \
       hash-table${OBJ_EXT} \
       io${OBJ_EXT} \
       io-encdec${OBJ_EXT} \
       io-external-file${OBJ_EXT} \
       io-internal-file${OBJ_EXT} \
       io-string${OBJ_EXT} \
       io-text${OBJ_EXT} \
       list${OBJ_EXT} \
       macro${OBJ_EXT} \
       main${OBJ_EXT} \
       memory${OBJ_EXT} \
       mt19937${OBJ_EXT} \
       number${OBJ_EXT} \
       number-format${OBJ_EXT} \
       oblist${OBJ_EXT} \
       string${OBJ_EXT} \
       structure${OBJ_EXT} \
       system${OBJ_EXT} \
       vector${OBJ_EXT}

VM_SRCS:= $(VM_OBJS:${OBJ_EXT}=.c)

VM_GCOV:= $(VM_OBJS:${OBJ_EXT}=.gcov)

FASL_DUMP_OBJS=fasl-dump${OBJ_EXT} \
               io-encdec${OBJ_EXT} \
               constants${OBJ_EXT}

FASL_DUMP_SRCS:= $(FASL_DUMP_OBJS:${OBJ_EXT}=.c)

coverage: $(VM_GCOV)

fasl-dump${EXE_EXT}: $(FASL_DUMP_OBJS)
	${CC} ${LDFLAGS} ${NAMOBJFL}fasl-dump${EXE_EXT}  $(FASL_DUMP_OBJS)

scan-vm${LIB_EXT}: $(VM_OBJS)
	${AR_RCS} ${AROUTFL}scan-vm${LIB_EXT} $(VM_OBJS)
ifdef RANLIB
	${RANLIB} scan-vm${LIB_EXT}
endif

SH0_OBJS=scansh0${OBJ_EXT} \
         vm-tests${OBJ_EXT}

SH0_SRCS:= $(SH0_OBJS:${OBJ_EXT}=.c)

scansh0${EXE_EXT}: scan-vm${LIB_EXT} ${SH0_OBJS}
	${CC} ${NAMOBJFL}scansh0${EXE_EXT} ${SH0_OBJS} scan-vm${LIB_EXT} ${LDFLAGS} 

TO_C_SOURCE_OBJS=to-c-source${OBJ_EXT}

TO_C_SOURCE_SRCS:=$(TO_C_SOURCE_OBJS:${OBJ_EXT}=.c)

to-c-source${EXE_EXT}: $(TO_C_SOURCE_OBJS)
	${CC} ${NAMOBJFL}to-c-source${EXE_EXT} $(TO_C_SOURCE_OBJS) ${LDFLAGS} 

SHOW_RETVAL_OBJS=show-retval${OBJ_EXT}

SHOW_RETVAL_SRCS:=$(SHOW_RETVAL_OBJS:${OBJ_EXT}=.c)

show-retval${EXE_EXT}: $(SHOW_RETVAL_OBJS)
	${CC}  ${NAMOBJFL}show-retval${EXE_EXT} $(SHOW_RETVAL_OBJS) ${LDFLAGS}

scan-constants.scm: scan-constants.i
	cpp -DCONST_SCHEME -P $< $@

depend:
	vcsh grovel-dependancies.scm scheme.scm 1> .depend

indented:
	gnuindent ${VM_SRCS} mt19937.h ${SCAN_HEADERS}

clean-local:
	rm -f scan-constants.scm

%.gcov: %.c %.gcda %.gcno scansh0${EXE_EXT}
	gcov $<

%${OBJ_EXT}: %.c $(SCAN_HEADERS) Makefile
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}$@ $<

ifneq ($(wildcard .depend),)
include .depend
endif

