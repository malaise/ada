LXC=-largs -lx_mng -lcutil -lX11
all: do_nimmari AFPX.DSC

do_nimmari:
	amake nimmari.adb $(LXC)

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

clean_exe:
	rm nimmari AFPX.DSC AFPX.FLD AFPX.INI

