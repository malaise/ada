LC=-largs -lcutil

EXE=dictio t_dictio

all: $(EXE)

new: 
	rm -f $(EXE)
	make

dictio:
	amake $@ $(LC)

t_dictio:
	amake $@ $(LC)

clean:
	-rm -f *.o *.ali b~*

clean_exe:
	-rm -f $(EXE)

clean_all: clean clean_exe


