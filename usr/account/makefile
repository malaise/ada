LIBC_DIR=$(HOME)/ada/c
LIBX_MNG=x_mng
LX=-largs -L$(LIBC_DIR) -l$(LIBX_MNG) -lX11
LX_STATIC=-largs $(LIBC_DIR)/lib$(LIBX_MNG).a -lX11

EXE=account.static account

all: $(EXE) AFPX.DSC

new: 
	rm -f $(EXE)
	amake account $(LX)
	
account:
	amake $@ $(LX)

account.static:
	amake account -o $@ $(LX_STATIC) 

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm -f *.o *.ali b~*

clean_exe:
	rm -f AFPX.DSC AFPX.FLD AFPX.INI $(EXE)

