include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)
AFPX := true
EXES := account import export account.stat

CLIBS := cutil
CLIBS_account := x_mng X11

TPATH := $(CPATH:%=%/$(LIB))
LARGS_account.stat := $(TPATH)/libx_mng.a $(TPATH)/libcutil.a -lX11

include $(TEMPLATES)/ada.mk

