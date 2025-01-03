include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

GEO := $(USR)/geo_codes
ADAVIEW := $(GEO)/mapcode $(GEO)/olc $(GEO)/geohash36 $(GEO)/geohash $(REPOSIT)
ADACTL := true

SUBUNITS := intercept.fpl
AFPX := true
AFPX_XREF := afpx_xref

EXES := gc hp_gc intercept geo_conv

CLIBS := $(UTILS_SLIBS) $(PCRE_SLIBS)
CLIBS_gc := $(PCRE_SLIBS) $(X11_SLIBS)

TXT := Great_Circle
DOCOPTS_Great_Circle := -a toc


include $(TEMPLATES)/ada.mk

