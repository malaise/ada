LXC=-lx_mng -lcutil -lX11

all: do_approxp do_import do_export AFPX.DSC

do_approxp: 
	amake approxp.adb -largs $(LXC)

do_import: 
	amake import.adb -largs $(LXC)

do_export: 
	amake export.adb -largs $(LXC)

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

clean_exe:
	rm approxp import export AFPX.DSC AFPX.FLD AFPX.INI


