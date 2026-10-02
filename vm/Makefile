# Makefile --
#
# The sub-Makefile for the VM and some support tools for the
# image build.
#
# (C) Copyright 2001-2022 East Coast Toolworks Inc.
# (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

.PHONY: indented tested

TARGETS=to-c-source scan-vm.a scansh0 fasl-dump scan-constants.scm TAGS

SCAN_HEADERS=scan-base.h scan-constants.h scan-internal-file.h scan-private.h \
             scan-sys.h scan-types.h scan.h scan-constants.i

include ../config.mk

tested: all ./scansh0
	./scansh0 -Xdebug-flags=test-vm

SCC0=scc0/scc0

VM_OBJS=unix-sys.o \
       character.o \
       constants.o \
       core.o \
       debug-printer.o \
       debug-tools.o \
       error.o \
       evaluator.o \
       fasl.o \
       global-env.o \
       hash-table.o \
       io.o \
       io-encdec.o \
       io-external-file.o \
       io-internal-file.o \
       io-string.o \
       io-text.o \
       list.o \
       macro.o \
       main.o \
       memory.o \
       mt19937.o \
       number.o \
       number-format.o \
       oblist.o \
       string.o \
       system.o \
       vector.o

VM_SRCS:= $(VM_OBJS:.o=.c)

FASL_DUMP_OBJS=fasl-dump.o \
               io-encdec.o \
               constants.o

FASL_DUMP_SRCS:= $(FASL_DUMP_OBJS:.o=.c)

fasl-dump: $(FASL_DUMP_OBJS)
	$(CC) $(LDFLAGS) -o $@ $(FASL_DUMP_OBJS) $(LDLIBS)

scan-vm.a: $(VM_OBJS)
	rm -f $@
	$(AR) rcs $@ $(VM_OBJS)

SH0_OBJS=scansh0.o \
         vm-tests.o

SH0_SRCS:= $(SH0_OBJS:.o=.c)

scansh0: scan-vm.a $(SH0_OBJS)
	$(CC) $(LDFLAGS) -o $@ $(SH0_OBJS) scan-vm.a $(LDLIBS)

TO_C_SOURCE_OBJS=to-c-source.o

TO_C_SOURCE_SRCS:=$(TO_C_SOURCE_OBJS:.o=.c)

to-c-source: $(TO_C_SOURCE_OBJS)
	$(CC) $(LDFLAGS) -o $@ $(TO_C_SOURCE_OBJS) $(LDLIBS)

scan-constants.scm: scan-constants.i
	$(CC) -E -P -x c -DCONST_SCHEME $< -o $@

depend:
	vcsh grovel-dependancies.scm scheme.scm 1> .depend

indented:
	gnuindent ${VM_SRCS} mt19937.h ${SCAN_HEADERS}

clean-local:
	rm -f scan-constants.scm

%.o: %.c $(SCAN_HEADERS) Makefile $(BUILD_FLAGS_STAMP)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

ifneq ($(wildcard .depend),)
include .depend
endif

