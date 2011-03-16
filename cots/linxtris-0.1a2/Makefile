all:
	gnatmake -O2 -gnatp src/linxtris.adb `gtkada-config` -bargs -shared -largs -s
clean:
	- rm *.o
	- rm *.ali
	- rm b~*
clean_exe:
	- rm linxtris
clean_all: clean clean_exe
scratch: clean_all all
static:
	gnatmake -O2 -gnatp src/linxtris.adb `gtkada-config --static` -bargs -static -largs -s
debug:
	gnatmake -O2 -g src/linxtris.adb `gtkada-config`
