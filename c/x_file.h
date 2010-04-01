#ifndef _X_FILE_H
#define _X_FILE_H
#include "x_stdinc.h"

#define NBRE_FONT 8
#define FONT_NAME_MAX_SIZE   255

/* extern for all includer except x_file.c */
#ifdef FILE_LOCAL
  char font_name [NBRE_FONT] [FONT_NAME_MAX_SIZE];
#else
  extern char font_name [NBRE_FONT] [FONT_NAME_MAX_SIZE];
#endif

/* Set font names fonts */
boolean fil_init(void);

#endif
