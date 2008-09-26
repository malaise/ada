include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)
EXES := asubst asubst.stat

CLIBS := cutil
CLIBS_asubst := pcreposix pcre

PCRE := `pcre-config --prefix`/lib
LARGS_asubst.stat := $(CPATHD)/libcutil.a $(PCRE)/libpcreposix.a \
  $(PCRE)/libpcre.a

include $(TEMPLATES)/ada.mk

