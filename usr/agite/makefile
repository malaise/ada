include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)

AFPX := true
AFPX_XREF := afpx_xref

SUBUNITS := agite-timer tags-list tags-add
EXES := agite agite.stat t_git

CLIBS := $(X11_SLIBS)
CLIBS_agite := $(PCRE_SLIBS)
CLIBS_t_git := $(PCRE_SLIBS)

LARGS_agite.stat := $(PCRE_ALIBS) $(X11_ALIBS)

TXT := Agite Cherry
DOCOPTS_Agite= -a toc

include $(TEMPLATES)/ada.mk

