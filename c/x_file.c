#include <stdlib.h>

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

#define FONT_1N_NAME "X_FONT_1N"
#define FONT_1B_NAME "X_FONT_1B"
#define FONT_2N_NAME "X_FONT_2N"
#define FONT_2B_NAME "X_FONT_2B"

void set_from_env (int font_index, char *env_name);

boolean fil_init (void) {
    int i;

    /* Set to default values */
    for (i=0; i<NBRE_COLOR; i++) {
        strcpy (color_name[i], default_color_name[i]);
    }
    for (i=0; i<NBRE_FONT; i++) {
        strcpy (font_name[i], default_font_name[i]);
    }

    /* Check if env sets font name */
    set_from_env(0, FONT_1N_NAME);
    set_from_env(1, FONT_1B_NAME);
    set_from_env(2, FONT_2N_NAME);
    set_from_env(3, FONT_2B_NAME);
    return (True);
}

void set_from_env (int font_index, char *env_name) {
    char *str;

    str = getenv(env_name);
    if ( (str != NULL) && (str[0] != '\0') ) {     
        strcpy (font_name[font_index], str);
    }
}
