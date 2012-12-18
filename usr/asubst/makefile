include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)

SUBUNITS := replace_pattern-file_content replace_pattern-shell_command
EXES := asubst asubst.stat

CLIBS := cutil
CLIBS_asubst := posix2pcre pcre

LARGS_asubst.stat := $(CPATHD)/libcutil.a $(CPATHD)/libposix2pcre.a \
  $(PCRE)/libpcre.a

TXT := Asubst History
DOCOPTS_Asubst := -a toc

include $(TEMPLATES)/ada.mk

