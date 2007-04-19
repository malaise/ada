#include "x_font.h"

/* Loads the fonts in the server */
boolean fon_open (Display *x_server, XFontSet font_set[],
                                     XFontStruct *font[]) {

int i, res, err;
char **name_list;
int missing_count;
XFontStruct **fonts;


    /* Create the font sets */
    err = -1;
    for (i = 0; i < NBRE_FONT; i++) {
        font_set[i] = XCreateFontSet(x_server, font_name[i],
                       &name_list, &missing_count, NULL);
        if (font_set[i] == NULL) {
     
#ifdef DEBUG
            printf ("X_FONT : can't create font set of %s.\n", font_name[i]);
#endif
            err=i;
            break;
        } else if (missing_count != 0)  {
#ifdef DEBUG
            printf ("X_FONT warning : missing %d fonts for font set of %s.\n", missing_count, font_name[i]);
            {
                int j;
                for (j = 0; j < missing_count; j++) {
                    printf ("X_FONT warning :     missing font %s.\n", name_list[j]);
                }
            }
#endif
            XFreeStringList (name_list);
        }

        /* Load font characteristics */
        res = XFontsOfFontSet (font_set[i], &fonts, &name_list);
        if (res == -1) {
#ifdef DEBUG
            printf ("X_FONT : cannot get font from set %s.\n", font_name[i]);
#endif
            err = i;
            break;
        }
        /* Copy font characteristics */
#ifdef DEBUG
            printf ("X_FONT info : got font from set %s.\n", name_list[0]);
#endif
        font[i] = XLoadQueryFont (x_server, font_name[i]);
        if (font[i] == NULL) {
#ifdef DEBUG
            printf ("X_FONT : cannot load font %s.\n", font_name[i]);
#endif
            err = i;
            break;
        }
        
    }

    if (err != -1) {
        for (i = 0; i < err; i++) {
           XFreeFontSet (x_server, font_set[i]);
        }
        if (font[i] !=  NULL) {
            XFreeFont (x_server, font[i]);
        }
        return (False);
    }
    return (True);

}


/* Free the fonts in the server */
void fon_close (Display *x_server, XFontSet font_set[], XFontStruct *font[]) {

int i;
    for (i = 0; i < NBRE_FONT; i++) {
        XFreeFontSet (x_server, font_set[i]);
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

