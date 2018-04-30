include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

ADAOPT = -g
EXES := t_mapcode
PARGS_t_mapcode.apb =

include $(TEMPLATES)/ada.mk

Export :
	rm t_mapcode.adb
	$(MAKE) "PARGS_t_mapcode.apb=Export"



