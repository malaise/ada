LIBX_MNG=/users/malaise/ada/c/libx_mng.a

all: AFPX.DSC
	amake t_afpx.adb -largs $(LIBX_MNG) -lX11
	amake t_arg.adb
	amake t_con.adb -largs $(LIBX_MNG) -lX11
	amake t_curve.adb -largs $(LIBX_MNG) -lX11
	amake t_delta_date.adb
	amake t_dd.adb
	amake t_dir.adb
	amake t_dl.adb
	amake t_getenv.adb
	amake t_get_line.adb
	amake t_fifo.adb
	amake t_hash.adb
	amake t_key.adb -largs $(LIBX_MNG) -lX11
	amake t_lifo.adb
	amake t_math.adb
	amake t_norm.adb
	amake t_perpet.adb
	amake t_rnd.adb
	amake t_sorts.adb
	amake t_state_machine.adb
	amake t_str_error.adb
	amake t_syslin.adb
	amake t_urand.adb
	amake t_x.adb -largs $(LIBX_MNG) -lX11

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

