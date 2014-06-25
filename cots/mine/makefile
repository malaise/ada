# For Mine Detector 6.0
include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(HOME)/ada/cots/pragmarc
ADAWARN := false

include $(TEMPLATES)/units.mk
NOTUNITS := $(filter-out mine_detector,$(UNITS))

EXES := mine_detector

GARGS := `gtkada-config`

include $(TEMPLATES)/ada.mk

