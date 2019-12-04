include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk
include $(TEMPLATES)/units.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

EXES := lapeau lapeau.stat

CLIBS := $(CARDS_SLIBS) $(X11_SLIBS)
LARGS_lapeau.stat := $(CARDS_ALIBS) $(X11_ALIBS)


include $(TEMPLATES)/ada.mk

