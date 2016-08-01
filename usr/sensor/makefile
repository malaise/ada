include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

EXES := sensor sensor.stat 

CLIBS := $(UTILS_SLIBS) $(PCRE_SLIBS)

LARGS_sensor.stat := $(UTILS_ALIBS) $(PCRE_ALIBS)

TXT := Sensor
DOCOPTS_Agite= -a toc

include $(TEMPLATES)/ada.mk

