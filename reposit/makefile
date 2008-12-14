include $(HOME)/Makefiles/common.mk

include $(TEMPLATES)/units.mk

SUBUNITS := afpx-af_dscr afpx-af_list afpx-af_ptg channels-file \
  pattern-storage x_mng-dispatch \
  xml_parser-file_mng xml_parser-parse_mng \
  xml_parser-parse_mng-dtd xml_parser-parse_mng-entity_mng \
  xml_parser-parse_mng-util xml_parser-tree_mng

LIBS := $(filter-out $(SUBUNITS), $(UNITS))

EXES := afpx_bld clear_screen

CLIBS_afpx_bld := cutil x_mng posix2pcre pcre X11
CLIBS_clear_screen := cutil

GNATHTMLOPT := -D

include $(TEMPLATES)/ada.mk

