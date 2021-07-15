include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

SUBUNITS := action.treat_click action.treat_release
EXES := mmind mmind.stat

CLIBS_mmind := $(X11_SLIBS)

LARGS_mmind.stat := $(PCRE_ALIBS) $(X11_ALIBS)

include $(TEMPLATES)/ada.mk

