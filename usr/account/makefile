include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)
AFPX := true
EXES := account import export account.stat

CLIBS_account := x_mng cutil X11

CLIBS_import := cutil

CLIBS_export := cutil

TPATH := $(CPATH:%=%/$(LIB))
LARGS_account.stat := $(TPATH)/libx_mng.a $(TPATH)/libcutil.a -lX11

include $(TEMPLATES)/ada.mk

