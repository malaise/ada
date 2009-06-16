include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)
ADAWARN := true
AFPX := true

EXES := adand add_nums alook arbipas beep comp_vars ddir e2f fact g \
 hanoi impot_04 \
 mlbx_overflow one_of_arg pascal pingpong prime random relay rndsok renamer \
 second stat statfile status strcase tcping trail_spaces udp_spy \
 wait xml_checker pingpong.stat tcping.stat udp_spy.stat xml_checker.stat

CLIBS := cutil
CLIBS_renamer := x_mng X11
CLIBS_comp_vars := posix2pcre pcre
CLIBS_xml_checker := posix2pcre pcre

LARGS_tcping.stat := $(CPATHD)/libcutil.a
LARGS_udp_spy.stat := $(CPATHD)/libcutil.a
LARGS_pingpong.stat := $(CPATHD)/libcutil.a

PCRE := `pcre-config --prefix`/lib
LARGS_xml_checker.stat := $(CPATHD)/libcutil.a $(CPATHD)/libposix2pcre.a \
  $(PCRE)/libpcre.a

include $(TEMPLATES)/ada.mk

