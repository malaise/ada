LIBX_MNG=-lx_mng
LIBUTIL=-lcutil
LX=-largs $(LIBX_MNG) $(LIBUTIL) -lX11
LU=-largs $(LIBUTIL)

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
	gnatmake $(ADA_OPT) $@ $(LX)

clear_screen:
	gnatmake $(ADA_OPT) $@ $(LU)

clean:
	-rm *.o *.ali b~*

clean_exe:
	-rm afpx_bld clear_screen

clean_all: clean clean_exe


