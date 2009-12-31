TARGETS=vcalc${EXE_EXT}

include ../build-settings

.PHONY: all clean clean-cov tested tested-individually todo

SCC=../vcsh -c 

CCFLAGS+=/DWINVER=0x0500
LDFLAGS=/nologo /subsystem:windows /incremental:no /nodefaultlib:libcmt

all: ${TARGETS} TAGS

tested: all
	$(error No unit tests yet.)

VCALC_OBJS=drawer-window${OBJ_EXT} \
           EnterNotifyEdit${OBJ_EXT} \
           KeyboardTranslation${OBJ_EXT} \
           license_txt${OBJ_EXT} \
           console-window${OBJ_EXT} \
           lisp-window${OBJ_EXT} \
           stdafx${OBJ_EXT} \
           vcalc${OBJ_EXT} \
           image${OBJ_EXT} \
           context${OBJ_EXT} \
           TipWindow${OBJ_EXT} \
           VCChooseBox${OBJ_EXT} \
           VCConfigDialog${OBJ_EXT} \
           VCEditor${OBJ_EXT} \
           VCSystem${OBJ_EXT} \
           VCWindow${OBJ_EXT} \
           Vcinit${OBJ_EXT}


VCALC_LISP_SRCS=vcinit.scm subrs.scm standard-windows.scm utils.scm \
		formatted-text.scm constants.scm vcalc-commands.scm \
		default-keymap.scm keymap.scm colors.scm vcalc-window.scm core.scm

VCALC_SRCS:=$(VCALC_OBJS:${OBJ_EXT}=.cpp)

vcalc${EXE_EXT}: ..\scheme-core\scanlib${LIB_EXT} ..\ectworks\ectworks${LIB_EXT} ${VCALC_OBJS} vcalc.res
	${LINK} ${LDFLAGS} ${LDNAMEXEFL}vcalc${EXE_EXT} ${VCALC_OBJS} vcalc.res ../scheme-core/scanlib${LIB_EXT} ../ectworks/ectworks${LIB_EXT}

%${OBJ_EXT}: %.cpp Makefile ..\vm\scan.h vcalc.h
	$(CC) ${CCFLAGS} $(DEFINES) ${COMPILEFL} ${NAMOBJFL}$@ $<

str_keys.i:  ids_exp.awk resource.h
	gawk -f ids_exp.awk < resource.h > str_keys.i

cmd_keys.i: cmd_exp.awk resource.h
	gawk -f cmd_exp.awk < resource.h > cmd_keys.i

VCSystem.obj: VCSystem.cpp str_keys.i

lisp-window.obj: lisp-window.cpp lisp-window.h cmd_keys.i

Vcinit.cpp: ${VCALC_LISP_SRCS}
	../scheme-core/vcsh -c vcinit.scm -o:vcinit.scf
	../util/to-c-source vcinit.scf vcInit > vcinit.cpp

vcalc.res: vcalc.rc ../ectworks/util-resource.h ../ectworks/util-resource.rc
	rc  /i "../ectworks" /fo vcalc.res vcalc.rc

%${OBJ_EXT}: %.cpp Makefile
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}$@ $<

depend: $(VM_SRCS) $(SHO_SRCS)
	$(CC) -MM $(CCFLAGS) $(DEFINES) $^ 1>.depend

clean-local:
	${RM_F} vcinit.cpp vcinit.scf

ifneq ($(wildcard .depend),)
include .depend
endif
