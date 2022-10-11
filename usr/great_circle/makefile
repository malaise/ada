include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(USR)/mapcode $(USR)/olc $(REPOSIT)
ADACTL := true

SUBUNITS := intercept.fpl
AFPX := true
AFPX_XREF := afpx_xref

EXES := gc hp_gc intercept conv

CLIBS := $(UTILS_SLIBS) $(PCRE_SLIBS)
CLIBS_gc := $(PCRE_SLIBS) $(X11_SLIBS)

include $(TEMPLATES)/ada.mk

