#include <stdlib.h>
#include <string.h>

#define FILE_LOCAL
#include "x_file.h"

static const char *default_color_name[NBRE_COLOR]={
  "black",      "blue",
  "dark green", "cyan",
  "red",        "magenta",
  "brown",      "light grey",
  "grey",  "light blue",
  "lime green","orange",
  "yellow",     "white"
 };

static const char *default_font_name[NBRE_FONT] = {
  "8x13", "8x13bold",
  "9x15", "9x15bold",
   "-b&h-lucidatypewriter-medium-r-normal-sans-18-180-75-75-m-110-iso8859-1",
      "-b&h-lucidatypewriter-bold-r-normal-sans-18-180-75-75-m-110-iso8859-1",
   "-b&h-lucidatypewriter-medium-r-normal-sans-25-180-100-100-m-150-iso8859-1",
      "-b&h-lucidatypewriter-bold-r-normal-sans-25-180-100-100-m-150-iso8859-1"};

#define FONT_NAME "X_FONT_"

void set_from_env (int font_index, char *env_name);

boolean fil_init (void) {
    int i;
    char env_name[10];

    /* Set to default values */
    for (i=0; i<NBRE_COLOR; i++) {
        strcpy (color_name[i], default_color_name[i]);
    }
    for (i=0; i<NBRE_FONT; i++) {
        strcpy (font_name[i], default_font_name[i]);
    }

    /* Check if env sets font name */
    for (i=0; i<NBRE_FONT; i++) {
      sprintf (env_name, "%s%1d", FONT_NAME, i + 1);
      set_from_env(i, env_name);
    }
    return (True);
}

void set_from_env (int font_index, char *env_name) {
    char *str;

    str = getenv(env_name);
    if ( (str != NULL) && (str[0] != '\0') ) {     
        strcpy (font_name[font_index], str);
    }
}
