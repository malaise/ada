C_DIR=/users/malaise/ada/c

all: do_nimmari

do_nimmari:
	amake nimmari.adb -largs $(C_DIR)/libx_mng.a -lX11

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

