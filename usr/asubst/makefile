include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)
EXES := asubst asubst.stat

CLIBS := cutil

TPATH := $(CPATH:%=%/$(LIB))
LARGS_asubst.stat := $(TPATH)/libcutil.a

include $(TEMPLATES)/ada.mk

