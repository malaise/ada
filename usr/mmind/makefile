LC=-largs -lcutil
LXC=-largs -lx_mng -lcutil -lX11

EXE=mmind mmind_asc

all: $(EXE)

mmind:
	amake mmind $(LXC)

mmind_asc:
	amake mmind_asc $(LC)

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

clean_exe:
	rm -f AFPX.DSC AFPX.FLD AFPX.INI $(EXE)

