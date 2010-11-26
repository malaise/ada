#ifndef _X_COLOR_H
#define _X_COLOR_H
#include "x_stdinc.h"

/* Limitation in default color map */
#define NBRE_COLOR 14
#define COLOR_NAME_MAX_SIZE  255
#define MAX_SIZE_TAB_COLOR NBRE_COLOR*NBRE_COLOR

/* Define the NBRE_COLOR color names */
void col_set_names (const char* names[]);

/* Loads and init colors, close and free colors (for each screen) */
boolean col_open(Display *x_server, int x_screen,
                 unsigned long color_id[],
                 XftColor xft_colors[],
                 Colormap *colormap);

void col_close(Display *x_server, int x_screen,
               unsigned long color_id[],
               XftColor xft_colors[],
               Colormap colormap);

/* Checks a color number (in attributes) */
boolean col_check(int color_id);

/* Gets the color_id of a color, on the background */
unsigned long col_get(int foreground, unsigned long color_id[]);

#endif
