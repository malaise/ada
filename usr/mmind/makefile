LXC=-largs -lx_mng -lcutil -lX11

EXE=mmind mmind_asc

all: exes

exes:
	@for file in $(EXE); do \
	  amake $$file $(LXC); \
	done;

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

clean_exe:
	rm -f AFPX.DSC AFPX.FLD AFPX.INI $(EXE)

