include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

SUBUNITS := action.treat_click action.treat_release action.clock
EXES := mmind mmind.stat

CLIBS_mmind := $(X11_SLIBS)

LARGS_mmind.stat := $(PCRE_ALIBS) $(X11_ALIBS)

TXT := Mmind
DOCOPTS_Mmind := -a toc


include $(TEMPLATES)/ada.mk

