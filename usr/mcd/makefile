include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

SUBUNITS := mcd_mng.call_stack mcd_mng.operations mcd_mng.dates \
mcd_mng.registers mcd_mng.file mcd_mng.stack mcd_mng.ios mcd_mng.strings \
mcd_mng.misc
EXES := mcd mcd.stat

CLIBS := $(UTILS_SLIBS)
CLIBS_mcd := $(PCRE_SLIBS)

LARGS_mcd.stat := $(PCRE_ALIBS) $(UTILS_ALIBS)


include $(TEMPLATES)/ada.mk

