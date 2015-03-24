include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)

SUBUNITS := def_enigma-xml
EXES := anal_freq enigma scrambler_generator def_enigma letterator

CLIBS := cutil
CLIBS_enigma := posix2pcre $(PCRE_LIBS)
CLIBS_def_enigma := posix2pcre $(PCRE_LIBS)

TXT := Enigma
DOCOPTS_Enigma := -a toc

include $(TEMPLATES)/ada.mk

