LXC=-largs -lx_mng -lcutil -lX11

all: do_all AFPX.DSC

do_all:
	@for file in *.adb ; do amake $$file $(LXC) ; done

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali b~*

clean_exe:
	@for file in *.adb ; do rm `basename $$file .adb` ; done
	rm AFPX.DSC AFPX.FLD AFPX.INI

