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
                     int paper, int ink, int superbright, int blink, int reverse) {

unsigned long foreground, background;

    if (reverse) {
        background = ink;
        ink = paper;
        paper = background;
    }

    if (blink) {
        foreground = col_get_blk (paper, ink, color_ids);
        background = col_get_std (paper, paper, color_ids);
    } else {
        foreground = col_get_std (ink, ink, color_ids);
        background = col_get_std (paper, paper, color_ids);
    }

    if ( superbright || (blink && blink_bold() ) ) {
        no_font =  fon_get_bold (no_font);
    }

    scr_set_gc (x_server, x_context, 
      background, foreground, x_fonts[no_font]->fid);
}



void scr_put_char(Display *x_server, GC x_context, Window x_window,
                   int x, int y, char car, int xor_mode) {

    if (!xor_mode) {
      XDrawImageString (x_server, x_window, x_context, x, y, &car, 1); 
    } else {
      XDrawString (x_server, x_window, x_context, x, y, &car, 1);
    }

} 

void scr_overwrite_char(Display *x_server, GC x_context, Window x_window, int x, int y, char car) {

    XDrawString (x_server, x_window, x_context, x, y, &car, 1);

} 

void scr_put_string (Display *x_server, GC x_context, Window x_window,
                     int x, int y, const char *p_car, int number, int xor_mode) {

    if (!xor_mode) {
      XDrawImageString (x_server, x_window, x_context, x, y, p_car, number);
    } else {
      XDrawString (x_server, x_window, x_context, x, y, p_car, number);
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

    XFillRectangle(x_server, x_window,  x_context, x, y, width, height);
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
