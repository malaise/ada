include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)

SUBUNITS := als-parse_date als-set_criteria
EXES := als als.stat

CLIBS := cutil
CLIBS_als := posix2pcre $(PCRE_LIBS)

LARGS_als.stat := $(CPATHD)/libcutil.a $(CPATHD)/libposix2pcre.a $(PCRE_ALIBS)

TXT := Als
DOCOPTS_Als := -a toc

include $(TEMPLATES)/ada.mk

