include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)

SUBUNITS := nav_data-add nav_data-check nav_data-comp_nav nav_data-comp_speed \
nav_data-comp_trajs nav_data-set_before nav_data-set_drift_after \
file nav_screen-show_time
NOTUNITS := t_scr
EXES := navig

CLIBS := $(X11_SLIBS) $(PCRE_SLIBS)

include $(TEMPLATES)/ada.mk

