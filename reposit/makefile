LIBX_MNG=-lx_mng
LX=-largs $(LIBX_MNG) -lX11

all: do_all afpx_bld clear_screen

do_all:
	@for file in *.ads *.adb ; do gnatmake -c $(ADA_OPT) $$file ; done

afpx_bld:
	gnatmake $(ADA_OPT) $@ $(LX)

clear_screen:
	gnatmake $(ADA_OPT) $@ $(LX)

clean:
	rm *.o *.ali

clean_exe:
	rm afpx_bld clear_screen

