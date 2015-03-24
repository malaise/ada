include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)

AFPX := true
AFPX_XREF := afpx_xref

EXES := battleship battleship.stat

CLIBS := x_mng cutil $(PCRE_SLIBS) X11

LARGS_battleship.stat := $(CPATHD)/libx_mng.a $(CPATHD)/libcutil.a \
                         $(PCRE_ALIBS) -lX11

TXT := BattleShip
DOCOPTS_BattleShip := -a toc

include $(TEMPLATES)/ada.mk

