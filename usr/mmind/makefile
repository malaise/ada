LIBC_DIR=$(HOME)/ada/c
LIBX_MNG=x_mng
LX=-largs -L$(LIBC_DIR) -l$(LIBX_MNG) -lX11

EXE=mmind mmind_asc

all: exes

exes:
	@for file in $(EXE); do \
	  amake $$file $(LX); \
	done;

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

clean_exe:
	rm -f AFPX.DSC AFPX.FLD AFPX.INI $(EXE)

