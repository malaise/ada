include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)

SUBUNITS := replace_pattern-file_content replace_pattern-shell_command
EXES := asubst asubst.stat

CLIBS := $(UTILS_SLIBS)
CLIBS_asubst = $(PCRE_SLIBS)

LARGS_asubst.stat := $(PCRE_ALIBS) $(UTILS_ALIBS)

TXT := Asubst
DOCOPTS_Asubst := -a toc

include $(TEMPLATES)/ada.mk

