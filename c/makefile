DEST=/usr/local/lib
CC=gcc
CCOPT=-g -c -DDEBUG -Wall -pthread
OBJS=x_color.o x_font.o x_line.o x_export.o x_file.o x_key.o x_screen.o \
     timeval.o socket.o sys_calls.o


.c.o:
	$(CC) $(CCOPT) $<

all: $(DEST)/libx_mng.so $(DEST)/libx_mng.a test_x

clean :
	rm *.o libx_mng.a libx_mng.so

clean_exe :
	rm test_x

socket.o : socket.h socket_prv.h

test_x : libx_mng.a test_x.c x_export.h
	$(CC) -o test_x test_x.c libx_mng.a -lX11

libx_mng.a : $(OBJS)
	ar rvs libx_mng.a $(OBJS)

libx_mng.so : $(OBJS)
	ld -shared -o libx_mng.so $(OBJS)

$(DEST)/libx_mng.a : libx_mng.a
	cp libx_mng.a $(DEST)
	chmod a+r $(DEST)/libx_mng.a

$(DEST)/libx_mng.so : libx_mng.so
	cp libx_mng.so $(DEST)
	chmod a+r $(DEST)/libx_mng.so

