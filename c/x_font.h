#ifndef _X_FONT_H
#define _X_FONT_H
/* Sep 05, 1993 : Image compliant X ressources management (PM)     */
#include "x_stdinc.h"
#include "x_file.h"

/* Loads the fonts, free the fonts */
boolean fon_open(Display *x_server, XFontStruct *font[]);
void fon_close(Display *x_server, XFontStruct *font[]);

/* Stores the fonts allocated by image */
boolean fon_open_image(Display *x_server, Font font_ids[], XFontStruct *font[]);

/* Checks a font number (in open_line) */
boolean fon_check(int font_id);

/* Gives the width of the fonts */
int fon_get_width(XFontStruct *font);
/* Gives the height of the fonts */
int fon_get_height(XFontStruct *font);
/* Gives the vertical reference pixel */
int fon_get_offset(XFontStruct *font);

/* Gives the index of the bold font of a font*/
int fon_get_bold (int font);
#endif
/* _X_FONT_H */

