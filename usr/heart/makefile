LXC=-largs -lx_mng -lcutil -lX11

all: do_heart AFPX.DSC

do_heart:
	amake heart.adb $(LXC)

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

clean_exe:
	rm heart AFPX.DSC AFPX.FLD AFPX.INI

