DEST=/users/malaise/bin
LIBX_MNG=-lx_mng


all:
	amake adand.adb -largs $(LIBX_MNG) -lX11
	cp adand $(DEST)
	amake beep.adb
	cp beep $(DEST)
	amake ddir.adb -largs $(LIBX_MNG) -lX11
	cp ddir $(DEST)
	amake e2f.adb
	amake fact.adb
	amake g.adb -largs -largs $(LIBX_MNG) -lX11
	amake hanoi.adb
	amake impot_02 -largs $(LIBX_MNG) -lX11
	amake look_95.adb
	amake mlbx_overflow.adb
	amake one_of_arg.adb
	amake pascal.adb
	amake pipe.adb -largs $(LIBX_MNG) -lX11
	amake prime.adb
	amake random.adb -largs $(LIBX_MNG) -lX11
	amake rndsok.adb 
	amake second.adb
	amake stat.adb
	cp stat $(DEST)
	amake statfile.adb
	cp statfile $(DEST)
	amake wait.adb
	cp wait $(DEST)

clean:
	rm *.o *.ali

clean_exe:
	rm adand beep ddir e2f fact g hanoi impot_02 look_95 \
	   mlbx_overflow one_of_arg pascal pipe prime random rndsok second \
	   stat statfile wait

