include $(HOME)/Makefiles/common.mk

include $(HOME)/Makefiles/units.mk

ADAVIEW := $(REPOSIT)
AFPX := true
EXES := $(filter-out t_smart_int, $(UNITS))

CLIBS := x_mng cutil X11
CLIBS_t_ndbm := cndbm gdbm \
 $(patsubst /usr/lib/lib%.so, %, $(wildcard /usr/lib/libgdbm_compat.so))

include $(TEMPLATES)/ada.mk

