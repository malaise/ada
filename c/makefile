include $(HOME)/Makefiles/common.mk

LIBS := cutil x_mng
ifeq ($(HOST), Linux)
DEST := /usr/local/lib
INSTALLED := $(LIBS:%=$(DEST)/lib%.a) $(LIBS:%=$(DEST)/lib%.so)
endif
EXES := test_x

U_OBJS := timeval.o sys_calls.o wait_evt.o socket.o
X_OBJS := x_color.o x_font.o x_line.o\
	x_export.o x_file.o x_key.o x_screen.o
OBJS := $(X_OBJS) $(U_OBJS) test_x.o

include $(TEMPLATES)/c.mk

U_OBJS := $(U_OBJS:%=$(LIB)/%)
X_OBJS := $(X_OBJS:%=$(LIB)/%)

$(LIB)/test_x.o : test_x.c x_export.h

$(BIN)/test_x : $(LIB)/test_x.o $(LIB)/libx_mng.so $(LIB)/libcutil.so
	$(CC) -o $(BIN)/test_x $(LIB)/test_x.o -L$(LIB) -lx_mng -lcutil -lX11

test_x : $(BIN)/test_x
	$(LN) $< $@


$(LIB)/libx_mng.a : $(X_OBJS)
	ar crvs $@ $(X_OBJS)

$(LIB)/libx_mng.so : $(X_OBJS) $(LIB)/libcutil.so
	ld -shared $(SOOPT) -o $@ $(X_OBJS) -L$(LIB) -lcutil -lX11 -lc
	-$(RM) so_locations

$(LIB)/libcutil.so : $(U_OBJS)
	ld -shared $(SOOPT) -o $@ $(U_OBJS) -lc
	-$(RM) so_locations

$(LIB)/libcutil.a : $(U_OBJS)
	ar crvs $@ $(U_OBJS)



