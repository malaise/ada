include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

ADAOPT = -g
EXES := mapcode
PARGS_mapcode.apb =

include $(TEMPLATES)/ada.mk

export :
	-rm mapcode.adb
	$(MAKE) 'PARGS_mapcode.apb=Export'

