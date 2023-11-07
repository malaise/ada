include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true
ADACTL_EXCL=ndata.ads countries.ads language_defs.ads

EXES := t_mapcode t_accuracy
PARGS_t_mapcode.apb =
CLIBS := $(UTILS_SLIBS)

include $(TEMPLATES)/ada.mk

Export :
	-rm t_mapcode.adb
	$(MAKE) "PARGS_t_mapcode.apb=Export"


Local :
	-rm t_mapcode.adb
	$(MAKE)

