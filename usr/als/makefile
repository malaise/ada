
include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)
EXES := als als.stat

CLIBS := cutil
CLIBS_als := pcreposix pcre

PCRE := `pcre-config --prefix`/lib
LARGS_als.stat := $(CPATHD)/libcutil.a $(PCRE)/libpcreposix.a \
  $(PCRE)/libpcre.a

include $(TEMPLATES)/ada.mk

