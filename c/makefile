DEST=/usr/local/lib
CC=gcc
CCOPT=-g -c -DDEBUG -Wall -pthread

.SUFFIXES: .h .c .o .a .so 

%.o : %.c
	$(CC) $(CCOPT) -c $<


$(DEST)/%.a : %.a
	cp $< $(DEST)
	chmod a+r $(DEST)/$<

$(DEST)/%.so : %.so
	cp $< $(DEST)
	chmod a+r $(DEST)/$<

LIBS=libx_mng.a libcutil.a
SOLIBS=$(LIBS:%.a=%.so)
DESTLIBS=$(LIBS:lib%=$(DEST)/lib%)
DESTSOLIBS=$(SOLIBS:lib%=$(DEST)/lib%)


all : $(DESTLIBS) $(DESTSOLIBS) test_x

test_x.o : test_x.c x_export.h

test_x : $(DESTSOLIBS) test_x.o
	$(CC) -pthread -o test_x test_x.o -lx_mng -lcutil -lX11


X_OBJS=x_color.o x_font.o x_line.o\
	x_export.o x_file.o x_key.o x_screen.o

libx_mng.a : $(X_OBJS)
	ar crvs $@ $(X_OBJS)

libx_mng.so : $(X_OBJS) $(DEST)/libcutil.so
	ld -shared -o $@ $(X_OBJS) -lcutil -lX11 -lc


U_OBJS=timeval.o sys_calls.o wait_evt.o socket.o

libcutil.a : $(U_OBJS)
	ar crvs libcutil.a $(U_OBJS)

libcutil.so : $(U_OBJS)
	ld -shared -o $@ $(U_OBJS) -lc

OBJS=$(X_OBJS) $(U_OBJS) test_x.o

clean :
	rm -f $(OBJS) $(LIBS) $(SOLIBS)

clean_exe :
	rm -f test_x

clean_all : clean clean_exe

