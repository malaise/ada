#ifndef _X_COLOR_H
#define _X_COLOR_H
/*  @(#) TAAATS PROGRAM FILE %M% Release %I% %H% %T% ~  */
static char H_ROLOC_X []=" @(#) TAAATS PROGRAM FILE %M% Release %I% %H% %T% ~";
/* Sep 05, 1993 : Image compliant X ressources management (PM)     */
#include "x_stdinc.h"
#include "x_file.h"

#define SIZE_TAB_COLOR NBRE_COLOR*NBRE_COLOR

/* Loads and init colors, close and free colors (for each screen) */
boolean col_open(Display *x_server, int x_screen, unsigned long color_id[]);
void col_close(Display *x_server, int x_screen, unsigned long color_id[]);

/* Store colors loaded by image */
boolean col_open_image(unsigned long color_ids[], unsigned long colors[]);

/* Checks a color number (in attributes) */
boolean col_check(int color_id);

/* Gets the color_id of a color, non blinking on the background */
int col_get_std(int background, int foreground, unsigned long color_id[]);

/* Gets the color_id of a color, blinking on the background */
int col_get_blk(int background, int foreground, unsigned long color_id[]);

/* Makes the colors blinking, or not blinking (alternatively) */
boolean col_set_blinking (Display *x_server, int x_screen, unsigned long color_id[],
                          boolean blinking);
#endif
/* _X_COLOR_H */
