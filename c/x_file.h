#ifndef _X_FILE_H
#define _X_FILE_H
#include "x_stdinc.h"

#define NBRE_FONT 8
#define FONT_NAME_MAX_SIZE   132

/* Limitation in default color map */
#define NBRE_COLOR 14
#define COLOR_NAME_MAX_SIZE  20

/* extern for all includer except x_file.c */
#ifdef FILE_LOCAL
  char font_name [NBRE_FONT] [FONT_NAME_MAX_SIZE];
  char color_name[NBRE_COLOR][COLOR_NAME_MAX_SIZE];
#else
  extern char font_name [NBRE_FONT] [FONT_NAME_MAX_SIZE];
  extern char color_name[NBRE_COLOR][COLOR_NAME_MAX_SIZE];
#endif

/* Set names (colors and fonts) */
boolean fil_init(void);

#endif
