include $(HOME)/Makefiles/common.mk

LIBS := cndbm cutil x_mng
ifeq ($(HOST), Linux)
DEST := /usr/local/lib
INSTALLED := $(LIBS:%=$(DEST)/lib%.a) $(LIBS:%=$(DEST)/lib%.so)
endif
EXES := test_x

D_OBJS := cndbm.o
U_OBJS := timeval.o sys_calls.o wait_evt.o socket.o
X_OBJS := x_color.o x_font.o x_line.o\
	x_export.o x_file.o x_key.o x_screen.o
OBJS := $(D_OBJS) $(X_OBJS) $(U_OBJS) test_x.o

include $(TEMPLATES)/c.mk

D_OBJS := $(D_OBJS:%=$(LIB)/%)
U_OBJS := $(U_OBJS:%=$(LIB)/%)
X_OBJS := $(X_OBJS:%=$(LIB)/%)

$(LIB)/test_x.o : test_x.c x_export.h

$(BIN)/test_x : $(LIB)/test_x.o $(LIB)/libx_mng.so $(LIB)/libcutil.so
	$(CC) -o $(BIN)/test_x $(LIB)/test_x.o -L$(LIB) \
	  -lx_mng -lcutil -lX11 -lm

$(LIB)/libcndbm.so : $(D_OBJS)
	ld -shared $(SOOPT) -o $@ $(D_OBJS) -lc
	-$(RM) so_locations

$(LIB)/libcndbm.a : $(D_OBJS)
	ar crvs $@ $(D_OBJS)

$(LIB)/libcutil.so : $(U_OBJS)
	ld -shared $(SOOPT) -o $@ $(U_OBJS) -lm -lc
	-$(RM) so_locations

$(LIB)/libcutil.a : $(U_OBJS)
	ar crvs $@ $(U_OBJS)

$(LIB)/libx_mng.so : $(X_OBJS) $(LIB)/libcutil.so
	ld -shared $(SOOPT) -o $@ $(X_OBJS) -L$(LIB) -lcutil -lX11 -lc
	-$(RM) so_locations

$(LIB)/libx_mng.a : $(X_OBJS)
	ar crvs $@ $(X_OBJS)

