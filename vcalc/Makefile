
# Makefile for Scan VM

# DEBUG or RELEASE
BUILD_TYPE=DEBUG

# on or off
PROFILE=off

.PHONY: all clean clean-cov tested tested-individually todo


DEFINES=
LIBS=

# -H tells gcc to print the name of each loaded include file.
# CCFLAGS+=-H

# --save-temps tells gcc to save temporary files used during compilation (namely, the
# preprocessed source)
# CCFLAGS+=-save-temps

# Windows Platform Constants
CC=cl
LINK=link
AR_RCS=lib
AR_RS=lib
RM_F=rm -f
DEFFL=/D
NAMOBJFL=/Fo
LDNAMEXEFL=/out:
NAMEXEFL=/Fe
COMPILEFL=/c
DEBUG_OPTFL=/O2 
OPTFL=/O2
DEBUGFL=/Zi
LDDEBUGFL=/DEBUG
CCFLAGS=/W4 /I. /GS- /GR- /EHs-c- /nologo /D_CRT_SECURE_NO_DEPRECATE /J /MT /I../vm/ /I../ectworks/ /DWINVER=0x0500
LDFLAGS=/nologo /subsystem:windows /incremental:no /nodefaultlib:libcmt
AROUTFL=/out:
AROUTFL_RS=
OBJ_EXT=.obj
EXE_EXT=.exe
LIB_EXT=.lib
DEFINES+=${DEFFL}SCAN_WINDOWS

LDFLAGS+=${LDDEBUGFL}

ifeq (${BUILD_TYPE}, DEBUG)
  CCFLAGS+=${DEFFL}_DEBUG ${DEBUGFL} ${DEBUG_OPTFL}
else
  ifeq (${BUILD_TYPE}, RELEASE)
    CCFLAGS+=${OPTFL}
  else
   $(error Unknown build type: $(BUILD_TYPE))
  endif
endif

ifeq (${PROFILE}, on)
   CCFLAGS+=-pg
   LDFLAGS+=-pg
endif

SCC=../vcsh -c 

TARGETS=vcalc${EXE_EXT}

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
           VCInit${OBJ_EXT}


VCALC_LISP_SRCS=vcinit.scm subrs.scm standard-windows.scm utils.scm \
		formatted-text.scm constants.scm vcalc-commands.scm \
		default-keymap.scm keymap.scm colors.scm vcalc-window.scm core.scm

VCALC_SRCS:=$(VCALC_OBJS:${OBJ_EXT}=.cpp)

vcalc${EXE_EXT}: ..\vm\scanlib${LIB_EXT} ..\ectworks\ectworks${LIB_EXT} ${VCALC_OBJS} vcalc.res
	${LINK} ${LDFLAGS} ${LDNAMEXEFL}vcalc${EXE_EXT} ${VCALC_OBJS} vcalc.res ../vm/scanlib${LIB_EXT} ../ectworks/ectworks${LIB_EXT} 

%${OBJ_EXT}: %.cpp Makefile ..\vm\scan.h vcalc.h
	$(CC) ${CCFLAGS} $(DEFINES) ${COMPILEFL} ${NAMOBJFL}$@ $<

str_keys.i:  ids_exp.awk resource.h
	gawk -f ids_exp.awk < resource.h > str_keys.i

cmd_keys.i: cmd_exp.awk resource.h
	gawk -f cmd_exp.awk < resource.h > cmd_keys.i

VCSystem.obj: VCSystem.cpp str_keys.i

lisp-window.obj: lisp-window.cpp lisp-window.h cmd_keys.i

VCInit.cpp: ${VCALC_LISP_SRCS}
	../vm/vcsh -c vcinit.scm -o:vcinit.scf
	../vm/to-c-source vcinit.scf vcInit > VCInit.cpp

vcalc.res: vcalc.rc ../ectworks/util-resource.h ../ectworks/util-resource.rc
	rc  /i "../ectworks" /fo vcalc.res vcalc.rc

todo:
	(find . \( -iname "*.cpp" -or -iname "*.h" -or -name "*.scm" -or -iname "TODO" \) | xargs grep -n XXX) || true
	(find . \( -iname "*.cpp" -or -iname "*.h" -or -name "*.scm" -or -iname "TODO" \) | xargs grep -n TODO) || true
	(find . \( -iname "*.cpp" -or -iname "*.h" -or -name "*.scm" -or -iname "TODO" \) | xargs grep -n TESTTHIS) || true
	(find . \( -iname "*.cpp" -or -iname "*.h" -or -name "*.scm" -or -iname "TODO" \) | xargs grep -n REVISIT) || true


clean-cov:	
	${RM_F} *.gcda *.gcov

clean-scheme:
	${RM_F} s-core${OBJ_EXT} fasl-compiler${OBJ_EXT} fasl-compiler-run${OBJ_EXT} 
	${RM_F} s-core.cpp s-core.scf 
	${RM_F} fasl-compiler-run.cpp fasl-compiler-run.scf 
	${RM_F} fasl-compiler.cpp fasl-compiler.scf 
	${RM_F} scanlib.a scanlib.lib

clean: clean-cov clean-scheme
	${RM_F} *.pdb *.ilk
	${RM_F} *.o *.obj TAGS *~ *.ii *.exe *.s ${TARGETS} *.i *.res
	${RM_F} scheme/*~ gmon${OBJ_EXT}ut *.gcno


%${OBJ_EXT}: %.cpp Makefile
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}$@ $<

TAGS:
	etags *.[ch] *.cpp

depend: $(VM_SRCS) $(SHO_SRCS)
	$(CC) -MM $(CCFLAGS) $(DEFINES) $^ 1>.depend


ifneq ($(wildcard .depend),)
include .depend
endif

