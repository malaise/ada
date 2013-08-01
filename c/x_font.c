#include <stdlib.h>
#include "x_font.h"

/* Loads the fonts in the server */
boolean fon_open (Display *x_server, XFontSet font_set[],
                                     XFontStruct *font[]) {

int i, res, err;
char **name_list;
int missing_count;
boolean debug;
XFontStruct **fonts;

    /* See if debug */
    {
        char *p;
        p = getenv("X_FONT_DEBUG");
        debug = (p != NULL) && ( (*p == 'y') || (*p == 'Y') );
    }
#ifdef DEBUG
    debug = True;
#endif


    /* Create the font sets */
    err = -1;
    for (i = 0; i < NBRE_FONT; i++) {
        font_set[i] = XCreateFontSet(x_server, font_name[i],
                       &name_list, &missing_count, NULL);
        if (font_set[i] == NULL) {

            printf ("X_FONT : can't create font set of %s.\n", font_name[i]);
            err=i;
            break;
        } else if (missing_count != 0)  {
            if (debug) {
                printf ("X_FONT warning : missing %d fonts for font set of %s.\n",
                        missing_count, font_name[i]);
                {
                    int j;
                    for (j = 0; j < missing_count; j++) {
                        printf ("X_FONT warning :     missing font %s.\n",
                                 name_list[j]);
                    }
                }
            }
            XFreeStringList (name_list);
        }

        /* Load font characteristics */
        res = XFontsOfFontSet (font_set[i], &fonts, &name_list);
        if (res == -1) {
            if (debug) {
                printf ("X_FONT : cannot get font from set %s.\n",
                        font_name[i]);
            }
            err = i;
            break;
        }
        /* Copy font characteristics */
        if (debug) {
            printf ("X_FONT info : got font from set %s.\n", name_list[0]);
        }
        font[i] = XLoadQueryFont (x_server, font_name[i]);
        if (font[i] == NULL) {
            if (debug) {
                printf ("X_FONT : cannot load font %s.\n", font_name[i]);
            }
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

    return (font->max_bounds.ascent + font->max_bounds.descent);
}

/* Gives the vertical reference pixel */
int fon_get_offset (XFontStruct *font) {

    return (font->max_bounds.ascent);
}

/* Gives the Bold font of a font */
int fon_get_bold (int font) {

    return (font + 1);
}

/* Gives the name of a font */
char* fon_get_name (int font) {
    return font_name[font];
}

/* Gives the number of characters of an UTF-8 sequence */
int fon_nb_chars (char first_char) {
  const unsigned char c = (unsigned char) first_char;
  if      ((c & 0x80) == 0) return 1;    /* Normal ASCII */
  else if ((c & 0x40) == 0) return 0;    /* Invalid */
  else if ((c & 0x20) == 0) return 2;
  else if ((c & 0x10) == 0) return 3;
  else if ((c & 0x01) == 0) return 4;
  else return 0;
}

/* Convert UTF-8 to plain ASCII. Ascii will not be longer that utf8, */
#define DEF_CHAR '#'
void utf82ascii (char *utf8, char *ascii) {

  int n;
  char *ps, *pd;

  ps = utf8;
  pd = ascii;
  for (;;) {
    /* Number of UTF-8 bytes for current char */
    n = fon_nb_chars (*ps);
    if (n == 0) {
      /* Invalid */
      *ascii = '\0';
      return;
    } else if (n == 1) {
      /* Plain ASCII */
      *pd = *ps;
    } else {
      /* Non ASCII */
      *pd = DEF_CHAR;
    }
    if (*ps == '\0') return;
    ps += n;
    pd++;
  }
}

