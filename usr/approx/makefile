LIBX_MNG=/users/malaise/ada/c/libx_mng.a
ADA_OPT=-g

all: do_approxp do_import AFPX.DSC

do_approxp: 
	amake $(ADA_OPT) approxp.adb -largs $(LIBX_MNG) -lX11

do_import: 
	amake $(ADA_OPT) import.adb -largs $(LIBX_MNG) -lX11

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali approxp import


