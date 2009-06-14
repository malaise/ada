include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)
ADAWARN := true
AFPX := true
EXES := account import export account.stat

CLIBS := cutil
CLIBS_account := x_mng X11

LARGS_account.stat := $(CPATHD)/libx_mng.a $(CPATHD)/libcutil.a -lX11

include $(TEMPLATES)/ada.mk

