LXC=-largs -lx_mng -lcutil -lX11

EXE=chess

all: $(EXE)

new: 
	rm -f $(EXE)
	make
	
chess:
	amake $@ $(LXC)

clean:
	rm -f *.o *.ali b~*

clean_exe:
	rm -f $(EXE)

