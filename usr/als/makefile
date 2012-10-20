
include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)
EXES := als als.stat

CLIBS := cutil
CLIBS_als := posix2pcre pcre

LARGS_als.stat := $(CPATHD)/libcutil.a $(CPATHD)/libposix2pcre.a \
  $(PCRE)/libpcre.a

TXT := Als
DOCOPTS_Als := -a toc

include $(TEMPLATES)/ada.mk

