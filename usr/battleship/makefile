include $(HOME)/Makefiles/common.mk

ADAVIEW := $(REPOSIT)

AFPX := true
AFPX_XREF := afpx_xref

EXES := battleship battleship.stat

CLIBS := x_mng cutil posix2pcre pcre X11

LARGS_battleship.stat := $(CPATHD)/libx_mng.a $(CPATHD)/libcutil.a \
 $(CPATHD)/libposix2pcre.a $(PCRE_LIB)/libpcre.a -lX11

TXT := BattleShip
DOCOPTS_Asubst := -a toc

include $(TEMPLATES)/ada.mk

