#include "x_screen.h"
#include "x_color.h"
#include "x_font.h"

#define STD_MASK GCForeground | GCBackground | GCFont

/* Change GC in the window */
void scr_set_gc (Display *x_server, GC x_context, unsigned long paper,
                 unsigned long ink, Font font);

/* Sets the attribute of the window */
void scr_set_attrib (Display *x_server, GC x_context, XFontStruct *x_fonts[],
                     int no_font, unsigned long color_ids[],
                     int paper, int ink, int reverse) {

unsigned long foreground, background;

    if (reverse) {
        background = ink;
        ink = paper;
        paper = background;
    }

    foreground = col_get(ink, color_ids);
    background = col_get(paper, color_ids);

    scr_set_gc (x_server, x_context,
      background, foreground, x_fonts[no_font]->fid);

    return;
}



void scr_put_char(Display *x_server, GC x_context, Window x_window,
                  XFontSet x_font_set, int x, int y, char car, int xor_mode) {

    if (!xor_mode) {
      XmbDrawImageString (x_server, x_window, x_font_set, x_context, x, y, &car,
                          1);
    } else {
      XmbDrawString (x_server, x_window, x_font_set, x_context, x, y, &car, 1);
    }

}

void scr_overwrite_char(Display *x_server, GC x_context, Window x_window,
                        XFontSet x_font_set, int x, int y, char car) {

    XmbDrawString (x_server, x_window, x_font_set, x_context, x, y, &car, 1);

}

void scr_put_string(Display *x_server, GC x_context, Window x_window,
                    XFontSet x_font_set, int x, int y,
                    const char *p_car, int number, int xor_mode) {
    if (!xor_mode) {
      XmbDrawImageString (x_server, x_window, x_font_set, x_context,
                          x, y, p_car, number);
    } else {
      XmbDrawString (x_server, x_window, x_font_set, x_context,
                     x, y, p_car, number);
    }
}


static char UNDERLINE = '_';
void scr_underline_char (Display *x_server, GC x_context,  Window x_window, int x, int y) {

    XDrawString (x_server, x_window, x_context, x, y, &UNDERLINE, 1);
}

#define MAX_LENGTH 0xFF
void scr_underline_string (Display *x_server, GC x_context,  Window x_window,
                           int x, int y, int number) {

int i;
char str[MAX_LENGTH];

    for(i=0; i<MAX_LENGTH; i++) {
        str[i] = UNDERLINE;
    }

    XDrawString (x_server, x_window, x_context, x, y, str, number);
}


void scr_draw_array (Display *x_server, GC x_context, Window x_window,
                     int x, int y, int width, int height) {

    XFillRectangle(x_server, x_window,  x_context, x, y,
         (unsigned int) width, (unsigned int) height);
}

/* Change GC in the window */
void scr_set_gc (Display *x_server, GC x_context,
                 unsigned long paper, unsigned long ink, Font font) {

unsigned long gc_mask = STD_MASK;
XGCValues gc_values;


    gc_values.background = paper;
    gc_values.foreground = ink;
    gc_values.font = font;

    XChangeGC (x_server, x_context, gc_mask, &gc_values);
}

