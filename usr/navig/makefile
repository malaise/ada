C_DIR=/users/malaise/ada/c

all: navig

navig:
	amake -g navig.adb -largs $(C_DIR)/libx_mng.a -lX11

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

