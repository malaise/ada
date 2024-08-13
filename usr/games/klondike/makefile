include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk
include $(TEMPLATES)/units.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

EXES := klondike klondike.stat

CLIBS := $(CARDS_SLIBS) $(X11_SLIBS)
LARGS_klondike.stat := $(CARDS_ALIBS) $(X11_ALIBS)

TXT := Klondike
DOCOPTS_Klondike := -a toc

include $(TEMPLATES)/ada.mk

