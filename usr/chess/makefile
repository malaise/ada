LIBC_DIR=$(HOME)/ada/c
LIBX_MNG=x_mng
LX=-largs -L$(LIBC_DIR) -l$(LIBX_MNG) -lX11

EXE=chess

all: $(EXE)

new: 
	rm -f $(EXE)
	make
	
chess:
	amake $@ $(LX)

clean:
	rm -f *.o *.ali b~*

clean_exe:
	rm -f $(EXE)

