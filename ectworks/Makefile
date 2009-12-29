
TARGETS=ectworks${LIB_EXT}

include ../build-settings

.PHONY: all clean clean-cov tested tested-individually todo

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

%${OBJ_EXT}: %.cpp Makefile
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}$@ $<

depend: $(VM_SRCS) $(SHO_SRCS)
	$(CC) -MM $(CCFLAGS) $(DEFINES) $^ 1>.depend


ifneq ($(wildcard .depend),)
include .depend
endif

