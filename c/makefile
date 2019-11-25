include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

LIBS := libx_mng libposix2pcre libevp libcards libx_mng libcutil
EXES := test_x t_cards

OBJS_libcutil := timeval sys_calls wait_evt socket socket_afux
OBJS_libx_mng := x_color x_font x_line x_export x_file x_key x_screen
OBJS_libevp := evp_digest
OBJS_libposix2pcre := posix2pcre
OBJS_libcards := x_cards x_bitmaps

CARGS_posix2pcre := $(PCRE_INCL)

LIBS_test_x := libx_mng libcutil 
LARGS_test_x := -lXpm -lX11
LIBS_t_cards := libcards libx_mng libcutil
LARGS_t_cards := -lXext -lXpm -lX11

DEST_LIBS := /usr/local/lib
INST_LIBS = $(LIBS)

DEST_HEADS := /usr/local/include
INST_HEADS := boolean.h timeval.h sys_calls.h wait_evt.h socket.h posix2pcre.h

include $(TEMPLATES)/c.mk

