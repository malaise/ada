LXC=-largs -lx_mng -lcutil -lX11

all:
	amake t_input.adb $(LXC)
	amake sokoban.adb $(LXC)
	amake trans.adb
clean:
	rm *.o *.ali

clean_exe:
	rm t_input sokoban trans

