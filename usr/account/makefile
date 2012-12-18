include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)

AFPX := true
AFPX_XREF := afpx_xref

SUBUNITS := account-quit mng-deletion mng-edition mng-search
EXES := account import export account.stat

CLIBS := cutil
CLIBS_account := x_mng X11

LARGS_account.stat := $(CPATHD)/libx_mng.a $(CPATHD)/libcutil.a -lX11

include $(TEMPLATES)/ada.mk

