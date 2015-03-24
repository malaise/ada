include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)

AFPX := true
AFPX_XREF := afpx_xref

SUBUNITS := xml_checker-canon_callback
EXES := adand add_nums alook arbipas astat azf beep comp_vars ddir \
 dtd_checker dtd_checker.stat e2f fact full_path g hanoi impot_04 \
 mlbx_overflow one_of_arg pascal pingpong prime \
 random relay rndsok renamer \
 second statfile status strcase tcping trail_spaces udp_spy \
 wait wake xml_checker astat.stat pingpong.stat tcping.stat tcp_spy \
 udp_spy.stat xml_checker.stat

CLIBS := cutil
CLIBS_renamer := x_mng X11
CLIBS_comp_vars := posix2pcre $(PCRE_LIBS)
CLIBS_dtd_checker := posix2pcre $(PCRE_LIBS)
CLIBS_relay := posix2pcre $(PCRE_LIBS)
CLIBS_wake := posix2pcre $(PCRE_LIBS)
CLIBS_xml_checker := posix2pcre $(PCRE_LIBS)

LARGS_astat.stat := $(CPATHD)/libcutil.a
LARGS_tcping.stat := $(CPATHD)/libcutil.a
LARGS_udp_spy.stat := $(CPATHD)/libcutil.a
LARGS_pingpong.stat := $(CPATHD)/libcutil.a
LARGS_dtd_checker.stat := $(CPATHD)/libcutil.a $(CPATHD)/libposix2pcre.a \
  $(PCRE_ALIBS)
LARGS_xml_checker.stat := $(CPATHD)/libcutil.a $(CPATHD)/libposix2pcre.a \
  $(PCRE_ALIBS)

TXT := Xml_Checker
DOCOPTS_Xml_Checker := -a toc

include $(TEMPLATES)/ada.mk

