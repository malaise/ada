include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)

AFPX := true
AFPX_XREF := afpx_xref

SUBUNITS := agite-timer tags-list tags-add
EXES := agite agite.stat t_git

CLIBS := x_mng cutil X11
CLIBS_agite := posix2pcre $(PCRE_LIBS)
CLIBS_t_git := posix2pcre $(PCRE_LIBS)

LARGS_agite.stat := $(CPATHD)/libx_mng.a $(CPATHD)/libcutil.a \
  $(CPATHD)/libposix2pcre.a $(PCRE_ALIBS) -lX11

TXT := Agite
DOCOPTS_Agite= -a toc

include $(TEMPLATES)/ada.mk

