LIBX_MNG=-lx_mng
LX=-largs $(LIBX_MNG) -lX11

all: do_all AFPX.DSC

do_all:
	amake t_afpx.adb $(LX)
	amake t_arg.adb
	amake t_con.adb $(LX)
	amake t_curve.adb $(LX)
	amake t_day_mng.adb
	amake t_delta_date.adb
	amake t_dd.adb
	amake t_dir.adb
	amake t_dl.adb
	amake t_dscr.adb $(LX)
	amake t_except
	amake t_getenv.adb
	amake t_get_line.adb
	amake t_fifo.adb
	amake t_hash.adb
	amake t_key.adb $(LX)
	amake t_lifo.adb
	amake t_math.adb
	amake t_mut.adb
	amake t_norm.adb
	amake t_perpet.adb
	amake t_rnd.adb
	amake t_sorts.adb
	amake t_select_file.adb $(LX)
	amake t_state_machine.adb
	amake t_str_error.adb
	amake t_syslin.adb
	amake t_urand.adb
	amake t_x.adb $(LX)

AFPX.DSC: AFPX.LIS
	afpx_bld

clean:
	rm *.o *.ali

