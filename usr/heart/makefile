LIBX_MNG=-lx_mng

all: do_heart AFPX.DSC

do_heart:
	amake heart.adb -largs $(LIBX_MNG) -lX11

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

clean_exe:
	rm heart AFPX.DSC AFPX.FLD AFPX.INI

