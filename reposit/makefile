include $(HOME)/Makefiles/common.mk

SUBUNITS := afpx-af_dscr afpx-af_list afpx-af_ptg afpx_bld-xref \
  autobus-config channels-file pattern-storage \
  xml_parser-parse_mng \
  xml_parser-parse_mng-dtd xml_parser-parse_mng-entity_mng \
  xml_parser-parse_mng-namespaces \
  xml_parser-parse_mng-resolve_uri xml_parser-parse_mng-util \
  xml_parser-tree_mng \
  x_mng-dispatch 
EXES := afpx_bld afpx_rnb clear_screen

CLIBS_afpx_bld := cutil x_mng posix2pcre pcre X11
CLIBS_afpx_rnb := cutil x_mng posix2pcre pcre X11
CLIBS_clear_screen := cutil

HTML := true
GNATHTMLOPT := -d

TXT := REPOSIT_LIST
DOCOPTS_REPOSIT_LIST := -a toc

include $(TEMPLATES)/ada.mk

