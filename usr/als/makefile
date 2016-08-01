include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

SUBUNITS := als-parse_date als-set_criteria
EXES := als als.stat

CLIBS := $(UTILS_SLIBS)
CLIBS_als := $(PCRE_SLIBS)

LARGS_als.stat := $(PCRE_ALIBS) $(UTILS_ALIBS)

TXT := Als
DOCOPTS_Als := -a toc

include $(TEMPLATES)/ada.mk

