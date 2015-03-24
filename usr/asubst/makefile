include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)

SUBUNITS := replace_pattern-file_content replace_pattern-shell_command
EXES := asubst asubst.stat

CLIBS := cutil
LARGS_asubst := -L$(PCRE_LIB)
CLIBS_asubst = $(PCRE_SLIBS)

LARGS_asubst.stat := $(CPATHD)/libcutil.a $(PCRE_ALIBS)

TXT := Asubst
DOCOPTS_Asubst := -a toc

include $(TEMPLATES)/ada.mk

