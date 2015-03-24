include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)

SUBUNITS := lsadeps-add_paths
EXES := lsadeps lsadeps.stat

CLIBS := cutil
LARGS_lsadeps := -L$(PCRE_LIB)
CLIBS_lsadeps := $(PCRE_SLIBS)

LARGS_lsadeps.stat := $(CPATHD)/libcutil.a $(PCRE_ALIBS)

TXT := Lsadeps
DOCOPTS_Lsadeps := -a toc

include $(TEMPLATES)/ada.mk

