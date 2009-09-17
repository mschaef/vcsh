# Makefile for Build Utilities

TARGETS=to-c-source${EXE_EXT}

include ../build-settings

TO_C_SOURCE_OBJS=to-c-source${OBJ_EXT}

TO_C_SOURCE_SRCS:=$(TO_C_SOURCE_OBJS:${OBJ_EXT}=.cpp)

to-c-source${EXE_EXT}: $(TO_C_SOURCE_OBJS)
	${CC} ${LDFLAGS} ${NAMOBJFL}to-c-source${EXE_EXT} $(TO_C_SOURCE_OBJS)

depend: $(VM_SRCS) $(SHO_SRCS)
	$(CC) -MM $(CCFLAGS) $(DEFINES) $^ 1>.depend

%${OBJ_EXT}: %.cpp Makefile
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}$@ $<

ifneq ($(wildcard .depend),)
include .depend
endif

