include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)

SUBUNITS := pieces-actions_of_bishop pieces-actions_of_king \
pieces-actions_of_knight pieces-actions_of_pawn pieces-actions_of_queen \
pieces-actions_of_rook screen-text screen-moves screen-graphic-bitmaps \
screen-graphic
EXES := chess

CLIBS := $(X11_SLIBS)

GITIGNORE := Default.chs

include $(TEMPLATES)/ada.mk

