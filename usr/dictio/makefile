LIBC_DIR=$(HOME)/ada/c
LIBX_MNG=x_mng
LX=-largs -L$(LIBC_DIR) -l$(LIBX_MNG) -lX11

EXE=dictio t_dictio

all: $(EXE)

new: 
	rm -f $(EXE)
	make

dictio:
	amake $@ $(LX)

t_dictio:
	amake $@ $(LX)

clean:
	-rm -f *.o *.ali b~*

clean_exe:
	-rm -f $(EXE)

clean_all: clean clean_exe


