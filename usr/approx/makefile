LIBX_MNG=-lx_mng
ADA_OPT=-g

all: do_approxp do_import do_export AFPX.DSC

do_approxp: 
	amake approxp.adb -largs $(LIBX_MNG) -lX11

do_import: 
	amake import.adb -largs $(LIBX_MNG) -lX11

do_export: 
	amake export.adb -largs $(LIBX_MNG) -lX11

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali approxp import


