include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)
EXES := asubst asubst.stat

CLIBS := cutil
CLIBS_asubst := posix2pcre pcre

LARGS_asubst.stat := $(CPATHD)/libcutil.a $(CPATHD)/libposix2pcre.a \
  $(PCRE)/libpcre.a

TEXI=Asubst History
History_WIDTH := 120

include $(TEMPLATES)/ada.mk

