LIBC_DIR=$(HOME)/ada/c
LIBX_MNG=x_mng
LX=-largs -L$(LIBC_DIR) -l$(LIBX_MNG) -lX11
ADA_FLAG=-gnato

EXE=mcd

all: $(EXE)

mcd:
	amake $(ADA_FLAG) $@ $(LX)

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

clean_exe:
	rm -f AFPX.DSC AFPX.FLD AFPX.INI $(EXE)

