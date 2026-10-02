
# Makefile --
#
# The sub-Makefile for the Scheme image, vcsh0, and vcsh.
#
# (C) Copyright 2001-2015 East Coast Toolworks Inc.
# (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

.PHONY: indented tested tested-aot bootstrap-check

TARGETS=scanlib.a vcsh

include ../config.mk

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


${GEN_DIR}/compiler-run.c: compiler-run.scf ../vm/to-c-source | ${GEN_DIR}
	echo \#include \"../../vm/scan-internal-file.h\" > $@
	../vm/to-c-source compiler-run.scf >> $@

#### Image bootstrap
#
# The image is compiled in two stages, and stage 2 is what gets linked
# into vcsh0 and vcsh:
#
#   Stage 1: scc0 (an older image and compiler, run on the current VM)
#            compiles the current sources.
#   Stage 2: the stage 1 image compiles them again, so changes to the
#            compiler take effect in a single build.
#
# 'make bootstrap-check' compiles a stage 3 with the stage 2 image and
# checks that it is byte-identical to stage 2. 'make update' in scc0/
# runs it before copying the images.
#
# SOURCE_DATE_EPOCH fixes the build date recorded in the image (see
# scheme.scm), so that compiles of the same sources are identical. It
# defaults to the time of the last commit that touched the image
# sources, so commits elsewhere (notes, the VM, tests) don't change it.

# The image is built from scheme.scm and the files it includes.
IMAGE_SOURCES := scheme.scm setup-image-compile.scm \
                 $(shell sed -n 's/^(include "\(.*\)")/\1/p' scheme.scm)

SOURCE_DATE_EPOCH ?= $(or $(shell git log -1 --format=%ct -- ${IMAGE_SOURCES} 2>/dev/null),$(shell date +%s))
export SOURCE_DATE_EPOCH

# The images depend on SOURCE_DATE_EPOCH through this stamp, rewritten
# only when the value changes. Without it, an image built before a
# commit would keep the old date while a later compile used the new one,
# and bootstrap-check would fail.
EPOCH_STAMP=${GEN_DIR}/source-date-epoch
$(shell mkdir -p ${GEN_DIR}; \
        echo ${SOURCE_DATE_EPOCH} | cmp -s - ${EPOCH_STAMP} || \
        echo ${SOURCE_DATE_EPOCH} > ${EPOCH_STAMP})

STAGE1_DIR=${GEN_DIR}/stage1
STAGE3_DIR=${GEN_DIR}/stage3

SCC0_IMAGES=../scc0/scheme.scf ../scc0/compiler-run.scf
STAGE1_IMAGES=${STAGE1_DIR}/scheme.scf ${STAGE1_DIR}/compiler-run.scf
STAGE2_IMAGES=scheme.scf compiler-run.scf

# Runs the compiler from the given scheme.scf and compiler-run.scf.
COMPILE_WITH=../vm/scansh0 -Xinit-load=$(1)/scheme.scf -Xinit-load=$(1)/compiler-run.scf

${STAGE1_DIR}/scheme.scf: ${IMAGE_SOURCES} ${EPOCH_STAMP} ../vm/scansh0 ${SCC0_IMAGES} | ${STAGE1_DIR}
	$(call COMPILE_WITH,../scc0) --load-file:setup-image-compile.scm --output:$@ scheme.scm

${STAGE1_DIR}/compiler-run.scf: compiler-run.scm ../vm/scansh0 ${SCC0_IMAGES} | ${STAGE1_DIR}
	$(call COMPILE_WITH,../scc0) --output:$@ compiler-run.scm

scheme.scf: ${IMAGE_SOURCES} ${EPOCH_STAMP} ../vm/scansh0 ${STAGE1_IMAGES}
	$(call COMPILE_WITH,${STAGE1_DIR}) --load-file:setup-image-compile.scm --output:$@ scheme.scm

compiler-run.scf: compiler-run.scm ../vm/scansh0 ${STAGE1_IMAGES}
	$(call COMPILE_WITH,${STAGE1_DIR}) --output:$@ compiler-run.scm

${STAGE3_DIR}/scheme.scf: ${IMAGE_SOURCES} ${EPOCH_STAMP} ../vm/scansh0 ${STAGE2_IMAGES} | ${STAGE3_DIR}
	$(call COMPILE_WITH,.) --load-file:setup-image-compile.scm --output:$@ scheme.scm

${STAGE3_DIR}/compiler-run.scf: compiler-run.scm ../vm/scansh0 ${STAGE2_IMAGES} | ${STAGE3_DIR}
	$(call COMPILE_WITH,.) --output:$@ compiler-run.scm

bootstrap-check: ${STAGE3_DIR}/scheme.scf ${STAGE3_DIR}/compiler-run.scf
	cmp scheme.scf ${STAGE3_DIR}/scheme.scf
	cmp compiler-run.scf ${STAGE3_DIR}/compiler-run.scf
	@echo "bootstrap-check: stage 2 and stage 3 images are identical."

${STAGE1_DIR} ${STAGE3_DIR}:
	mkdir -p $@

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
