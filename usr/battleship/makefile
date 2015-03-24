include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)

AFPX := true
AFPX_XREF := afpx_xref

EXES := battleship battleship.stat

CLIBS := $(PCRE_SLIBS) $(X11_SLIBS)

LARGS_battleship.stat := $(PCRE_ALIBS) $(X11_ALIBS)

TXT := BattleShip
DOCOPTS_BattleShip := -a toc

include $(TEMPLATES)/ada.mk

