ADA_HOME=/users/malaise/ada
LIBX_MNG=$(ADA_HOME)/c/libx_mng.a
X_LIBS=-largs $(LIBX_MNG) -lX11
ADA_OPT=-g
all:
	amake $(ADA_OPT) ex1.adb
	amake $(ADA_OPT) garde.adb
	amake $(ADA_OPT) iole.adb
	amake $(ADA_OPT) para.adb
	amake $(ADA_OPT) servprio.adb
	amake $(ADA_OPT) stamp.adb
	amake $(ADA_OPT) termin.adb
	amake $(ADA_OPT) wheeles.adb

clean:
	rm *.o *.ali

