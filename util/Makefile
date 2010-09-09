# Makefile for Build Utilities

.PHONY: indented

TARGETS=to-c-source${EXE_EXT} show-retval${EXE_EXT}

include ../build-settings

TO_C_SOURCE_OBJS=to-c-source${OBJ_EXT}

TO_C_SOURCE_SRCS:=$(TO_C_SOURCE_OBJS:${OBJ_EXT}=.cpp)

to-c-source${EXE_EXT}: $(TO_C_SOURCE_OBJS)
	${CC} ${LDFLAGS} ${NAMOBJFL}to-c-source${EXE_EXT} $(TO_C_SOURCE_OBJS)

SHOW_RETVAL_OBJS=show-retval${OBJ_EXT}

SHOW_RETVAL_SRCS:=$(SHOW_RETVAL_OBJS:${OBJ_EXT}=.cpp)

show-retval${EXE_EXT}: $(SHOW_RETVAL_OBJS)
	${CC} ${LDFLAGS} ${NAMOBJFL}show-retval${EXE_EXT} $(SHOW_RETVAL_OBJS)

depend: $(VM_SRCS) $(SHO_SRCS)
	$(CC) -MM $(CCFLAGS) $(DEFINES) $^ 1>.depend

indented:
	gnuindent ${SHOW_RETVAL_SRCS} ${TO_C_SOURCE_SRCS} base-assert.h base-stdint.h base-tchar.h base-types.h chemeris-stdint.h 

%${OBJ_EXT}: %.cpp Makefile
	$(CC) $(CCFLAGS) $(DEFINES) ${COMPILEFL} ${NAMOBJFL}$@ $<

ifneq ($(wildcard .depend),)
include .depend
endif

