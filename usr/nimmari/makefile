all: do_nimmari

do_nimmari:
	amake nimmari.adb -largs -lx_mng -lX11

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

clean_exe:
	rm nimmari AFPX.DSC AFPX.FLD AFPX.INI

