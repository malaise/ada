include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)
AFPX := true
AFPX_XREF := afpx_xref
EXES := agite agite.stat t_git

CLIBS := x_mng cutil X11
CLIBS_agite := posix2pcre pcre

LARGS_agite.stat := $(CPATHD)/libx_mng.a $(CPATHD)/libcutil.a \
  $(CPATHD)/libposix2pcre.a $(PCRE)/libpcre.a -lX11

TXT := Agite

include $(TEMPLATES)/ada.mk

