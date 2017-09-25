include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

SUBUNITS := lsadeps.add_paths
EXES := lsadeps lsadeps.stat

CLIBS := $(UTILS_SLIBS)
CLIBS_lsadeps := $(PCRE_SLIBS)

LARGS_lsadeps.stat := $(PCRE_ALIBS) $(UTILS_ALIBS)

TXT := Lsadeps
DOCOPTS_Lsadeps := -a toc

include $(TEMPLATES)/ada.mk

