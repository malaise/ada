#include "x_font.h"

/* Loads the fonts in the server */
boolean fon_open (Display *x_server, XFontStruct *font[]) {

int i, err;
Font font_id;

    /* Load the fonts */
    err = -1;
    for (i=0; i<NBRE_FONT; i++) {
        /* XLoadQueryFont has a bug, it always loads */
        /* so use 2 separate calls */
        font_id = XLoadFont (x_server, font_name[i]);
        font[i] = XQueryFont (x_server, font_id);
        if (font[i] == 0) {
            err = i;
            break;
        }
    }

    if (err == -1) {
        return (True);
    } else {

#ifdef DEBUG
        printf ("X_FONT :X can't load font no %d named %s.\n",
         err, font_name[err]);
#endif
        /* An error, free previous fonts */
        for (i=0; i<err; i++) {
            XFreeFont (x_server, font[i]);
        }
        return (False);

    }

}


/* Free the fonts in the server */
void fon_close (Display *x_server, XFontStruct *font[]) {

int i;
    for (i=0; i<NBRE_FONT; i++) {
        XFreeFont (x_server, font[i]);
    }
}

/* Verifies that the font number in line_open is valid */
boolean fon_check (int font_id) {

    return (font_id < (NBRE_FONT / 2));
}


/* Gives the width of the fonts */
int fon_get_width (XFontStruct *font) {

    return ((int) font->max_bounds.width);
}

/* Gives the height of the fonts */
int fon_get_height (XFontStruct *font) {

    return (font->ascent + font->descent);
}

/* Gives the vertical reference pixel */
int fon_get_offset (XFontStruct *font) {

    return (font->ascent);
}

/* Gives the Bold font of a font */
int fon_get_bold (int font) {

    return (font + 1);
}
