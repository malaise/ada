include $(HOME)/Makefiles/common.mk

include $(TEMPLATES)/units.mk

SUBUNITS := afpx-af_dscr afpx-af_list afpx-af_ptg channels-file \
            pattern-storage x_mng-dispatch

LIBS := $(filter-out $(SUBUNITS), $(UNITS))

EXES := afpx_bld clear_screen

CLIBS_afpx_bld := x_mng cutil X11

CLIBS_clear_screen := x_mng cutil X11

GNATHTMLOPT := -D

include $(TEMPLATES)/ada.mk

