include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

SUBUNITS := def_enigma.xml
EXES := anal_freq enigma scrambler_generator def_enigma letterator

CLIBS := $(UTILS_SLIBS)
CLIBS_enigma := $(PCRE_SLIBS)
CLIBS_def_enigma := $(PCRE_SLIBS)

TXT := Enigma
DOCOPTS_Enigma := -a toc

include $(TEMPLATES)/ada.mk

