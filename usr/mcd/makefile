LIBC_DIR=$(HOME)/ada/c
LC=-largs -L$(LIBC_DIR) -lcutil
ADA_FLAG=-gnato

EXE=mcd

all: $(EXE)

mcd:
	amake $(ADA_FLAG) $@ $(LC)

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm -f *.o *.ali mcd_storage.dat

clean_exe:
	rm -f AFPX.DSC AFPX.FLD AFPX.INI $(EXE)

