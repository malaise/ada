include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

SUBUNITS := afpx-af_dscr afpx-af_list afpx-af_ptg afpx_bld-xref \
  autobus-config channels-file pattern-storage \
  xml_parser-parse_mng \
  xml_parser-parse_mng-dtd xml_parser-parse_mng-entity_mng \
  xml_parser-parse_mng-namespaces \
  xml_parser-parse_mng-resolve_uri xml_parser-parse_mng-util \
  xml_parser-tree_mng \
  x_mng-dispatch 
EXES := afpx_bld afpx_rnb clear_screen

OPTIMIZED := snappy lzf lz4
PARGS_assertion.apb := # NoAssert
PARGS_lz4.apb :=  # Debug
PARGS_lzf.apb :=  # Debug # Fast | Slow # LocalTrace
PARGS_mutexes.apb := # Debug
PARGS_snappy.apb := # Debug
PARGS_rnd.aps := Urand
PARGS_rnd.apb := Urand
CLIBS_app := $(UTILS_SLIBS) $(PCRE_SLIBS)
CLIBS_afpx_bld := $(PCRE_SLIBS) $(X11_SLIBS)
CLIBS_afpx_rnb := $(PCRE_SLIBS) $(X11_SLIBS)
CLIBS_clear_screen := $(UTILS_SLIBS)

ADACTL := true
HTML := true
GNATHTMLOPT := -d

TXT := REPOSIT_LIST App Afpx
DOCOPTS_REPOSIT_LIST := -a toc

include $(TEMPLATES)/ada.mk

