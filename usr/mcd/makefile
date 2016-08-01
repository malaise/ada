include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

SUBUNITS := mcd_mng-call_stack mcd_mng-operations mcd_mng-dates \
mcd_mng-registers mcd_mng-file mcd_mng-stack mcd_mng-ios mcd_mng-strings \
mcd_mng-misc
EXES := mcd

CLIBS := $(UTILS_SLIBS)
CLIBS_mcd := $(PCRE_SLIBS)

include $(TEMPLATES)/ada.mk

