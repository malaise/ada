include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)

AFPX := true
AFPX_XREF := afpx_xref

SUBUNITS := dtd_generator-merge xml_checker-canon_callback
EXES := adand add_nums alook arbipas astat azf beep comp_vars ddir \
 dtd_checker dtd_generator e2f fact full_path g hanoi \
 impot_04 mlbx_overflow one_of_arg pascal pingpong prime \
 random relay rndsok renamer \
 second statfile status strcase tcping trail_spaces udp_spy \
 wait wake xml_checker \
 astat.stat dtd_checker.stat dtd_generator.stat pingpong.stat \
 tcping.stat tcp_spy udp_spy.stat xml_checker.stat

CLIBS := $(UTILS_SLIBS)
CLIBS_comp_vars := $(PCRE_SLIBS)
CLIBS_dtd_checker := $(PCRE_SLIBS)
CLIBS_dtd_generator := $(PCRE_SLIBS)
CLIBS_relay := $(PCRE_SLIBS)
CLIBS_renamer := $(X11_SLIBS)
CLIBS_wake := $(PCRE_SLIBS)
CLIBS_xml_checker := $(PCRE_SLIBS)

LARGS_astat.stat := $(UTILS_ALIBS)
LARGS_dtd_checker.stat := $(PCRE_ALIBS) $(UTILS_ALIBS)
LARGS_dtd_generator.stat := $(PCRE_ALIBS) $(UTILS_ALIBS)
LARGS_pingpong.stat := $(UTILS_ALIBS)
LARGS_tcping.stat := $(UTILS_ALIBS)
LARGS_udp_spy.stat := $(UTILS_ALIBS)
LARGS_xml_checker.stat := $(PCRE_ALIBS) $(UTILS_ALIBS)

TXT := Xml_Checker
DOCOPTS_Xml_Checker := -a toc

include $(TEMPLATES)/ada.mk

