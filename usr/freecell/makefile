include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk
include $(TEMPLATES)/units.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

EXES := freecell freecell.stat

CLIBS := $(CARDS_SLIBS) $(X11_SLIBS)
LARGS_freecell.stat := $(CARDS_ALIBS) $(X11_ALIBS)

TXT := Freecell
DOCOPTS_Freecell := -a toc

include $(TEMPLATES)/ada.mk

