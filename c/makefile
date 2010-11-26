include $(HOME)/Makefiles/common.mk

LIBS := libcndbm libcutil libx_mng libposix2pcre
EXES := test_x

CARGS_FEETYPE := `freetype-config  --cflags`
CARGS_x_color := $(CARGS_FEETYPE)
CARGS_x_font := $(CARGS_FEETYPE)
CARGS_x_line := $(CARGS_FEETYPE)
CARGS_x_export := $(CARGS_FEETYPE)
CARGS_x_file := $(CARGS_FEETYPE)
CARGS_x_key := $(CARGS_FEETYPE)
CARGS_x_screen := $(CARGS_FEETYPE)


OBJS_libcndbm := cndbm.o
OBJS_libcutil := timeval.o sys_calls.o wait_evt.o socket.o socket_afux.o
OBJS_libposix2pcre := posix2pcre.o
OBJS_libx_mng := x_color.o x_font.o x_line.o x_export.o x_file.o x_key.o \
                 x_screen.o
LARGS_test_x := -lx_mng -lcutil -lXft `freetype-config --libs` -lX11

DEST_LIBS := /usr/local/lib
INST_LIBS := $(LIBS)

DEST_HEADS := /usr/local/include
INST_HEADS := boolean.h timeval.h sys_calls.h wait_evt.h socket.h posix2pcre.h

include $(TEMPLATES)/c.mk

