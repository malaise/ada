DEST=/users/malaise/bin
LIBX_MNG=-lx_mng


all:
	amake adand.adb
	cp adand $(DEST)
	amake beep.adb
	cp beep $(DEST)
	amake ddir.adb -largs $(LIBX_MNG) -lX11
	cp ddir $(DEST)
	amake fact.adb
	amake g.adb
	amake hanoi.adb
	amake impot_98.adb
	amake impot_99.adb
	amake one_of_arg.adb
	amake pascal.adb
	amake random.adb -largs $(LIBX_MNG) -lX11
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
	rm adand beep ddir fact g hanoi impot_98 one_of_arg pascal second stat statfile wait

