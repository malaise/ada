CC=gcc
CCOPT=-g -c -DDEBUG
OBJS=x_color.o x_font.o x_line.o x_export.o x_file.o x_key.o x_screen.o timeval.o


.c.o:
	$(CC) $(CCOPT) $<

all: libx_mng.a test_x

clean :
	rm *.o libx_mng.a

test_x : libx_mng.a test_x.c
	$(CC) -o test_x test_x.c libx_mng.a -lX11

libx_mng.a : $(OBJS)
	ar rvs libx_mng.a $(OBJS)

