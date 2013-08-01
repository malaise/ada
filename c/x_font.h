#ifndef _X_FONT_H
#define _X_FONT_H

#include "x_stdinc.h"
#include "x_file.h"

/* Loads the fonts, free the fonts */
boolean fon_open(Display *x_server, XFontSet font_set[],
                                    XFontStruct *font[]);
void fon_close(Display *x_server, XFontSet font_set[], XFontStruct *font[]);

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
/* Gives the name of a font*/
char* fon_get_name (int font);

/* Gives the number of characters of an UTF-8 sequence */
int fon_nb_chars (char first_char);
/* Convert UTF-8 to plain ASCII. Ascii will not be longer that utf8, */
void utf82ascii (char *utf8, char *ascii);

#endif
/* _X_FONT_H */

