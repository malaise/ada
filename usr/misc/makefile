include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)

AFPX := true
AFPX_XREF := afpx_xref

SUBUNITS := xml_checker-canon_callback
EXES := adand add_nums alook arbipas astat beep comp_vars ddir \
 dtd_checker dtd_checker.stat e2f fact full_path g hanoi impot_04 \
 mlbx_overflow one_of_arg pascal pingpong prime \
 random relay rndsok renamer \
 second statfile status strcase tcping trail_spaces udp_spy \
 wait wake xml_checker pingpong.stat tcping.stat tcp_spy udp_spy.stat \
 xml_checker.stat

CLIBS := cutil
CLIBS_renamer := x_mng X11
CLIBS_comp_vars := posix2pcre pcre
CLIBS_dtd_checker := posix2pcre pcre
CLIBS_relay := posix2pcre pcre
CLIBS_wake := posix2pcre pcre
CLIBS_xml_checker := posix2pcre pcre

LARGS_tcping.stat := $(CPATHD)/libcutil.a
LARGS_udp_spy.stat := $(CPATHD)/libcutil.a
LARGS_pingpong.stat := $(CPATHD)/libcutil.a
LARGS_dtd_checker.stat := $(CPATHD)/libcutil.a $(CPATHD)/libposix2pcre.a \
  $(PCRE)/libpcre.a
LARGS_xml_checker.stat := $(CPATHD)/libcutil.a $(CPATHD)/libposix2pcre.a \
  $(PCRE)/libpcre.a

TXT := Xml_Checker
DOCOPTS_Xml_Checker := -a toc

include $(TEMPLATES)/ada.mk

