LIBX_MNG=-lx_mng
LX=-largs $(LIBX_MNG) -lX11

all: do_all AFPX.DSC

do_all:

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

clean_exe:
	rm -f account AFPX.DSC AFPX.FLD AFPX.INI

