#include <stdlib.h>
#include <string.h>

#define FILE_LOCAL
#include "x_file.h"


static const char *lucidatypewriter[NBRE_FONT] = {
   "-b&h-lucidatypewriter-medium-r-normal-sans-12-120-75-75-m-70-iso10646-*",
     "-b&h-lucidatypewriter-bold-r-normal-sans-12-120-75-75-m-70-iso10646-*",
   "-b&h-lucidatypewriter-medium-r-normal-sans-14-140-75-75-m-90-iso10646-*",
     "-b&h-lucidatypewriter-bold-r-normal-sans-14-140-75-75-m-90-iso10646-*",
   "-b&h-lucidatypewriter-medium-r-normal-sans-18-180-75-75-m-110-iso10646-*",
     "-b&h-lucidatypewriter-bold-r-normal-sans-18-180-75-75-m-110-iso10646-*",
   "-b&h-lucidatypewriter-medium-r-normal-sans-25-180-100-100-m-150-iso10646-*",
     "-b&h-lucidatypewriter-bold-r-normal-sans-25-180-100-100-m-150-iso10646-*"};
static const char *courier[NBRE_FONT] = {
   "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso10646-*",
     "-adobe-courier-bold-r-normal--12-120-75-75-m-70-iso10646-*",
   "-adobe-courier-medium-r-normal--14-140-75-75-m-90-iso10646-*",
     "-adobe-courier-bold-r-normal--14-140-75-75-m-90-iso10646-*",
   "-adobe-courier-medium-r-normal--18-180-75-75-m-110-iso10646-*",
     "-adobe-courier-bold-r-normal--18-180-75-75-m-110-iso10646-*",
   "-adobe-courier-medium-r-normal--25-180-100-100-m-150-iso10646-*",
     "-adobe-courier-bold-r-normal--25-180-100-100-m-150-iso10646-*" };
static const char *fixed[NBRE_FONT] = {
   "-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso10646-*",
     "-misc-fixed-bold-r-normal--13-120-75-75-c-70-iso10646-*",
   "-misc-fixed-medium-r-normal--14-130-75-75-c-70-iso10646-*",
     "-misc-fixed-bold-r-normal--14-130-75-75-c-70-iso10646-*",
   "-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso10646-*",
     "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso10646-*",
   "-misc-fixed-medium-r-normal--18-120-100-100-c-90-iso10646-*",
     "-misc-fixed-bold-r-normal--18-120-100-100-c-90-iso10646-*"};

#define FONT_NAME "X_FONT_"

void set_from_env (int font_index, char *env_name);

boolean fil_init (void) {
    int i;
    char env_name[10];
    const char **default_font_name;
    char *p;


    /* Set X font kind */
    default_font_name = lucidatypewriter;
    p = getenv("X_FONT_KIND");
    if (p != NULL) {
      if (strcmp (p , "courier") == 0) {
        default_font_name = courier;
      } else if (strcmp (p , "lucidatypewriter") == 0) {
        default_font_name = lucidatypewriter;
      } else if (strcmp (p , "fixed") == 0) {
        default_font_name = fixed;
      }
    }

    /* Set to default values */
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

