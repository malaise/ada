include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(USR)/mapcode $(REPOSIT)
ADACTL := true

SUBUNITS := intercept.fpl
AFPX := true
AFPX_XREF := afpx_xref

EXES := gc hp_gc intercept

CLIBS := $(UTILS_SLIBS)
CLIBS_gc := $(PCRE_SLIBS) $(X11_SLIBS)
CLIBS_hp_gc := $(PCRE_SLIBS)
CLIBS_intercept := $(PCRE_SLIBS)

include $(TEMPLATES)/ada.mk

