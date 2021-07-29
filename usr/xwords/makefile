include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

AFPX := true
AFPX_XREF := afpx_xref

EXES := xwords xwords.stat words_check t_analist

CLIBS_xwords := $(PCRE_SLIBS) $(X11_SLIBS)
CLIBS := $(UTILS_SLIBS)

LARGS_xwords.stat = $(PCRE_ALIBS) $(X11_ALIBS)

include $(TEMPLATES)/ada.mk

