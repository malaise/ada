
#define FILE_LOCAL
#include "x_file.h"

static char *default_color_name[NBRE_COLOR]={
  "black",      "blue",
  "dark green", "cyan",
  "red",        "magenta",
  "brown",      "light grey",
  "dark grey",  "light blue",
  "light green","orange",
  "yellow",     "white"
 };

static char *default_font_name[NBRE_FONT] = {
  "8x13", "8x13bold", "9x15", "9x15bold"}; 


boolean fil_init (void) {
    int i;

    /* Set to default values */
    for (i=0; i<NBRE_COLOR; i++) {
        strcpy (color_name[i], default_color_name[i]);
    }
    for (i=0; i<NBRE_FONT; i++) {
        strcpy (font_name[i], default_font_name[i]);
    }
    return (True);
}
