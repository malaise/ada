C_DIR=/users/malaise/ada/c

all: AFPX.DSC
	amake nimmari.adb -largs $(C_DIR)/libx_mng.a -lX11

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

