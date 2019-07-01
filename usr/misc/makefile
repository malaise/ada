include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

ADAVIEW := $(REPOSIT)
ADACTL := true

AFPX := true
AFPX_XREF := afpx_xref

SUBUNITS :=
EXES := adand add_nums alook arbipas astat azf beep bernouilli \
 comp_vars day_of_week ddir e2f fact full_path g hanoi \
 impot_04 lenv alz4l mlbx_overflow one_of_arg pascal pingpong prime \
 random relay renamer \
 second statfile status strcase tcping trail_spaces udp_spy \
 wait wake \
 astat.stat pingpong.stat \
 tcping.stat tcp_spy udp_spy.stat

CLIBS := $(UTILS_SLIBS)
CLIBS_comp_vars := $(PCRE_SLIBS)
CLIBS_lenv := $(PCRE_SLIBS)
CLIBS_relay := $(PCRE_SLIBS)
CLIBS_renamer := $(X11_SLIBS)
CLIBS_wake := $(PCRE_SLIBS)

LARGS_astat.stat := $(UTILS_ALIBS)
LARGS_pingpong.stat := $(UTILS_ALIBS)
LARGS_tcping.stat := $(UTILS_ALIBS)
LARGS_udp_spy.stat := $(UTILS_ALIBS)

include $(TEMPLATES)/ada.mk

