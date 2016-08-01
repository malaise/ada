include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

AFPX := true
AFPX_XREF := afpx_xref

SUBUNITS := account-quit mng-deletion mng-edition mng-search
EXES := account import export account.stat

CLIBS := $(UTILS_SLIBS)
CLIBS_account := $(X11_SLIBS)

LARGS_account.stat := $(X11_ALIBS)

include $(TEMPLATES)/ada.mk

