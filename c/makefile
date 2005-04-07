include $(HOME)/Makefiles/common.mk

LIBS := libcndbm libcutil libx_mng
EXES := test_x

OBJS_libcndbm := cndbm.o
OBJS_libcutil := timeval.o sys_calls.o wait_evt.o socket.o
OBJS_libx_mng := x_color.o x_font.o x_line.o\
	x_export.o x_file.o x_key.o x_screen.o

LARGS_test_x := -lx_mng -lcutil -lX11 -lm

DEST_LIBS := /usr/local/lib
INST_LIBS := $(LIBS:%=$(DEST_LIBS)/%.a) $(LIBS:%=$(DEST_LIBS)/%.so)

include $(TEMPLATES)/c.mk

$(LIB)/libcndbm.a $(LIB)/libcndbm.so : $(OBJS_libcndbm:%=$(LIB)/%)

$(LIB)/libcutil.a $(LIB)/libcutil.so : $(OBJS_libcutil:%=$(LIB)/%)

$(LIB)/libx_mng.a $(LIB)/libx_mng.so : $(OBJS_libx_mng:%=$(LIB)/%)

cndbm.c : cndbm.h

socket.c : socket_prv.h wait_evt.h
wait_evt.c : wait_evt.h
sys_calls.c : sys_calls.h
timeval.c : timeval.h
socket_prv.h : socket.h socket_net.h
socket.h : boolean.h
wait_evt.h : timeval.h

x_export.c : x_export.h x_screen.h x_line.h timeval.h
x_file.c : x_file.h
x_font.c : x_font.h
x_key.c : x_key.h
x_line.c : x_line.h x_file.h
x_screen.c : x_screen.h x_color.h x_font.h
x_color.h : x_stdinc.h x_file.h
x_export.h : boolean.h wait_evt.h
x_file.h : x_stdinc.h
x_font.h :  x_stdinc.h x_file.h
x_line.h : x_stdinc.h x_font.h x_color.h x_screen.h x_key.h
x_screen.h : x_stdinc.h

