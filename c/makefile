include $(HOME)/Makefiles/common.mk

LIBS := libcutil libx_mng libposix2pcre
DBM := $(wildcard /usr/include/gdbm-ndbm.h)
ifneq ($(DBM),)
LIBS += libcndbm
OBJS_libcndbm := cndbm.o
endif
EXES := test_x

OBJS_libcutil := timeval.o sys_calls.o wait_evt.o socket.o socket_afux.o
OBJS_libposix2pcre := posix2pcre.o
OBJS_libx_mng := x_color.o x_font.o x_line.o x_export.o x_file.o x_key.o \
                 x_screen.o

LARGS_test_x := ./lib_Linux/libx_mng.so ./lib_Linux/libcutil.so -lX11

DEST_LIBS := /usr/local/lib
INST_LIBS = $(LIBS)

DEST_HEADS := /usr/local/include
INST_HEADS := boolean.h timeval.h sys_calls.h wait_evt.h socket.h posix2pcre.h

include $(TEMPLATES)/c.mk

