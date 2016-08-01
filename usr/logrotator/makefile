include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

EXES := logrotator

CLIBS := $(UTILS_SLIBS) $(PCRE_SLIBS)

LARGS_logrotator.stat := $(PCRE_ALIBS) $(UTILS_ALIBS)

TXT := Logrotator
DOCOPTS_Asubst := -a toc

include $(TEMPLATES)/ada.mk

