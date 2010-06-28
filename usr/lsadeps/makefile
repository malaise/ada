include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)
ADAWARN := true
EXES := lsadeps lsadeps.stat

CLIBS := cutil
CLIBS_lsadeps := posix2pcre pcre

LARGS_lsadeps.stat := $(CPATHD)/libcutil.a $(CPATHD)/libposix2pcre.a \
  $(PCRE)/libpcre.a

include $(TEMPLATES)/ada.mk

