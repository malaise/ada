include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)
ADAWARN := true
EXES := asubst asubst.stat

CLIBS := cutil
CLIBS_asubst := posix2pcre pcre

PCRE := `pcre-config --prefix`/lib
LARGS_asubst.stat := $(CPATHD)/libcutil.a $(CPATHD)/libposix2pcre.a \
  $(PCRE)/libpcre.a

include $(TEMPLATES)/ada.mk

