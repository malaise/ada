include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)


SUBUNITS := dtd_generator-merge dtd_generator-merge-sequences \
 xml_checker-canon_callback
EXES := dtd_checker dtd_generator xml_checker \
 dtd_checker.stat dtd_generator.stat xml_checker.stat

CLIBS := $(UTILS_SLIBS)
CLIBS_dtd_checker := $(PCRE_SLIBS)
CLIBS_dtd_generator := $(PCRE_SLIBS)
CLIBS_xml_checker := $(PCRE_SLIBS)

LARGS_dtd_checker.stat := $(PCRE_ALIBS) $(UTILS_ALIBS)
LARGS_dtd_generator.stat := $(PCRE_ALIBS) $(UTILS_ALIBS)
LARGS_xml_checker.stat := $(PCRE_ALIBS) $(UTILS_ALIBS)

TXT := Xml_Checker
DOCOPTS_Xml_Checker := -a toc

include $(TEMPLATES)/ada.mk

