LIBX_MNG=-lx_mng
LIBCUTIL=-lcutil
LXC=-largs $(LIBX_MNG) $(LIBCUTIL) -lX11
LUC=-largs $(LIBCUTIL)

all: reposit afpx_bld clear_screen

reposit:
	@for file in  *.ads; do \
	  if [ !  -f `basename $$file .ads`.adb ] ; then \
	    gnatmake $(ADA_OPT) -c $$file; \
	  fi \
	done;
	@for file in  *.adb; do \
	  gnatmake $(ADA_OPT) -c $$file; \
	done;

afpx_bld:
	gnatmake $(ADA_OPT) $@ $(LXC)

clear_screen:
	gnatmake $(ADA_OPT) $@ $(LUC)

clean:
	-rm *.o *.ali b~*

clean_exe:
	-rm afpx_bld clear_screen

clean_all: clean clean_exe


