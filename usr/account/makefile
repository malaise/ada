LIBC_DIR=$(HOME)/ada/c
LIBX_MNG=x_mng
LIBCUTIL=cutil
LXC=-largs -l$(LIBX_MNG) -L$(LIBC_DIR) -l$(LIBCUTIL) -lX11
LXC_STATIC=-largs $(LIBC_DIR)/lib$(LIBX_MNG).a $(LIBC_DIR)/lib$(LIBCUTIL).a \
           -lX11

EXE=account.static account import export

all: $(EXE) AFPX.DSC

new: 
	rm -f $(EXE)
	amake account $(LXC)
	
account:
	amake $@ $(LXC)

import:
	amake $@ $(LXC)

export:
	amake $@ $(LXC)

account.static:
	amake account -o $@ $(LXC_STATIC) 

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm -f *.o *.ali b~*

clean_exe:
	rm -f AFPX.DSC AFPX.FLD AFPX.INI $(EXE)

