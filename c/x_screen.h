#ifndef _X_SCREEN_H
#define _X_SCREEN_H
#include "x_stdinc.h"

void scr_set_attrib(Display *x_server, GC x_context, XFontStruct *x_fonts[],
                    int no_font, unsigned long color_ids[],
                    int paper, int ink, int reverse);

void scr_put_char(Display *x_server, GC x_context, Window x_window,
                  XFontSet x_font_set, int x, int y, char car, int xor_mode);

void scr_overwrite_char(Display *x_server, GC x_context, Window x_window,
                        XFontSet x_font_set, int x, int y, char car);

void scr_underline_char(Display *x_server, GC x_context, Window x_window, int x, int y);

void scr_put_string(Display *x_server, GC x_context, Window x_window,
                    XFontSet x_font_set, int x, int y,
                    const char *p_car, int number, int xor_mode);

void scr_underline_string(Display *x_server, GC x_context, Window x_window, int x, int y, int number);

void scr_draw_array (Display *x_server, GC x_context, Window x_window, int x, int y, int width, int height);

#endif
/* _X_SCREEN_H */

