LIBX_MNG=-lx_mng

all:
	amake code.adb -largs $(LIBX_MNG) -lX11
	amake tg1.adb
	amake tg2.adb

clean:
	rm *.o *.ali

clean_exe:
	rm code tg1 tg2

