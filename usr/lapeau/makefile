include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk
include $(TEMPLATES)/units.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

EXES := lapeau lapeau.stat

CLIBS := cards $(X11_SLIBS) Xext
LARGS_lapeau.stat :=-lcards $(X11_ALIBS) -lXext


include $(TEMPLATES)/ada.mk

