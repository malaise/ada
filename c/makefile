include $(HOME)/Makefiles/common.mk

LIBS := libcndbm libcutil libx_mng
EXES := test_x

OBJS_libcndbm := cndbm.o
OBJS_libcutil := timeval.o sys_calls.o wait_evt.o socket.o socket_afux.o regex.o
OBJS_libx_mng := x_color.o x_font.o x_line.o x_export.o x_file.o x_key.o \
                 x_screen.o

LARGS_test_x := -lx_mng -lcutil -lX11 -lm

DEST_LIBS := /usr/local/lib
INST_LIBS := $(LIBS)

DEST_HEADS := /usr/local/include
INST_HEADS := boolean.h timeval.h sys_calls.h wait_evt.h socket.h

include $(TEMPLATES)/c.mk

