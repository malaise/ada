include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

SUBUNITS := pieces.actions_of_bishop pieces.actions_of_king \
pieces.actions_of_knight pieces.actions_of_pawn pieces.actions_of_queen \
pieces.actions_of_rook screen.text screen.moves \
screen.graphic screen.graphic.bitmaps 
EXES := chess chess.stat

CLIBS := $(X11_SLIBS)
LARGS_chess.stat := $(X11_ALIBS)


GITIGNORE := Default.chs

include $(TEMPLATES)/ada.mk

