include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

AFPX := true
AFPX_XREF := afpx_xref

EXES := xwords words_check t_analist

CLIBS_xwords := $(PCRE_SLIBS) $(X11_SLIBS)
CLIBS := $(UTILS_SLIBS)

TXT := Xwords
DOCOPTS_Asubst := -a toc


include $(TEMPLATES)/ada.mk

