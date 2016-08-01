# For Mine Detector 6.0
include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(HOME)/ada/cots/pragmarc
ADAWARN := false

EXES := mine_detector

OPTIMIZED := mine_detector field field-operations user_if
GARGS := `gtkada-config`

include $(TEMPLATES)/ada.mk

