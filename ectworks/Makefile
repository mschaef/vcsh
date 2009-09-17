
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
AR_RCS=lib
AR_RS=lib
RM_F=rm -f
DEFFL=/D
NAMOBJFL=/Fo
NAMEXEFL=/Fe
COMPILEFL=/c
DEBUG_OPTFL=/O2 
OPTFL=/O2
DEBUGFL=/Zi
CCFLAGS=/W4 /I. /GS- /GR- /EHs-c- /nologo /D_CRT_SECURE_NO_DEPRECATE /J /MT /I../vm/ /DWINVER=0x0500
LDFLAGS=/nologo /MT
AROUTFL=/out:
AROUTFL_RS=
OBJ_EXT=.obj
EXE_EXT=.exe
LIB_EXT=.lib
DEFINES+=${DEFFL}SCAN_WINDOWS

LDFLAGS+=${DEBUGFL}

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

TARGETS=ectworks${LIB_EXT}

all: ${TARGETS} TAGS

tested: all
	$(error No unit tests yet.)

ECTWORKS_OBJS=CAboutBox${OBJ_EXT} \
              CAppBranding${OBJ_EXT} \
              CIntroDialog${OBJ_EXT} \
              CNewKeyDialog${OBJ_EXT} \
              CRegistrationDialog${OBJ_EXT} \
              CRegistryBindings${OBJ_EXT} \
              IDCombo${OBJ_EXT} \
              oneshot-timer${OBJ_EXT} \
              registration${OBJ_EXT} \
              registrationUtil${OBJ_EXT} \
              SmartPictureStatic${OBJ_EXT} \
              TextServicesGUIDs${OBJ_EXT} \
              TimeoutRegistration${OBJ_EXT}

ECTWORKS_SRCS:=$(ECTWORKS_OBJS:${OBJ_EXT}=.cpp)

ectworks${LIB_EXT}: ${ECTWORKS_OBJS}
	${AR_RCS} ${AROUTFL}ectworks${LIB_EXT} ${ECTWORKS_OBJS}
ifdef ${RANLIB}
	${RANLIB} ectworks${LIB_EXT}
endif

%${OBJ_EXT}: %.cpp Makefile
	$(CC) ${CCFLAGS} $(DEFINES) ${COMPILEFL} ${NAMOBJFL}$@ $<

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
	${RM_F} *.o *.obj TAGS *~ *.ii *.exe *.s ${TARGETS} *.i
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

