include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk
include $(TEMPLATES)/units.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

AFPX := true

EXES := $(filter-out t_smart_int, $(UNITS))

CLIBS := $(X11_SLIBS)
CLIBS_t_ndbm := cndbm gdbm \
 $(patsubst /usr/lib/$(ARCH)/lib%.so, %, $(wildcard /usr/lib/$(ARCH)/libgdbm_compat.so))
CLIBS_t_async := $(PCRE_SLIBS)
CLIBS_t_autobus := $(PCRE_SLIBS)
CLIBS_t_bencode := $(PCRE_SLIBS)
CLIBS_t_byte_to_unicode := $(PCRE_SLIBS)
CLIBS_t_channels := $(PCRE_SLIBS)
CLIBS_t_date_text := $(PCRE_SLIBS)
CLIBS_t_delta_date := $(PCRE_SLIBS)
CLIBS_t_evp_digest := ssl crypto
CLIBS_t_regex_filters := $(PCRE_SLIBS)
CLIBS_t_regexp := $(PCRE_SLIBS)
CLIBS_t_scanner := $(PCRE_SLIBS)
CLIBS_t_state_machine := $(PCRE_SLIBS)
CLIBS_t_string := $(PCRE_SLIBS)
CLIBS_t_str_error := $(PCRE_SLIBS)
CLIBS_t_xml_gen := $(PCRE_SLIBS)
CLIBS_t_xml_string := $(PCRE_SLIBS)

include $(TEMPLATES)/ada.mk

