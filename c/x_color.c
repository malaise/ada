/* Oct 23, 1998 : Use private color map                   (PM)     */
#include <stdlib.h>

#include "x_color.h"

static boolean envs_got = False;

#define N_PLANES 0

#define X_COLOR_MAP "X_COLOR_MAP"
#define PRIVATE_COLOR_MAP   "PRIVATE"
#define PUBLIC_COLOR_MAP    "PUBLIC"
static boolean private_color_map;

#define X_BLINK_KIND "X_BLINK_KIND"
#define FULL_BLINK   "FULL"
#define ON_0_BLINK   "ON_0"
#define BOLD_BLINK   "BOLD"
typedef enum {full_blink, on_0_blink, bold_blink} blink_kind_list; 
static blink_kind_list blink_kind = full_blink;
static int color_tab_size;

XColor color_value[NBRE_COLOR];



static void get_envs (void) {
  char * color_map_type;
  char * blink_kind_var;

  /* Get env color map type */
  color_map_type = getenv(X_COLOR_MAP);
  private_color_map = (color_map_type != NULL) && (strcmp(color_map_type, PRIVATE_COLOR_MAP) == 0);

  
  /* Getenv blink kind */
  blink_kind_var = getenv (X_BLINK_KIND);
  if ( (blink_kind_var == NULL) || (blink_kind_var[0] == '\0')
    || (strcmp(blink_kind_var, FULL_BLINK) == 0) ) {
    blink_kind = full_blink;
    color_tab_size = NBRE_COLOR * NBRE_COLOR;
  } else if (strcmp(blink_kind_var, ON_0_BLINK) == 0) {
    blink_kind = on_0_blink;
    color_tab_size = NBRE_COLOR * 2;
  } else if (strcmp(blink_kind_var, BOLD_BLINK) == 0) {
    blink_kind = bold_blink;
    color_tab_size = NBRE_COLOR;
  } else {
    blink_kind = full_blink;
    color_tab_size = NBRE_COLOR * NBRE_COLOR;
  }
  envs_got = True;
}



boolean blink_bold(void) {
  if (!envs_got) get_envs();
  return (blink_kind == bold_blink);
}

/* Loads the colors in the server */
boolean col_open(Display *x_server, int x_screen, unsigned long color_id[], Colormap *colormap) {
  int i, cr;
  unsigned long plane_mask[1];

    if (!envs_got) get_envs();

    /* Create / get color map */
    if (private_color_map) {
      *colormap = XCreateColormap(x_server, RootWindow (x_server, x_screen), 
                                  XDefaultVisual (x_server, x_screen), AllocNone);
    } else {
      *colormap = DefaultColormap (x_server, x_screen);
    }



    /* Alloc colors */
    cr = XAllocColorCells (x_server, *colormap, False, plane_mask, N_PLANES,
     color_id, color_tab_size);
    if (cr == 0) {
#ifdef DEBUG
      printf ("X_COLOR : X can't alloc cells for colors.\n");
#endif
      return (False);
    }
 
 
    /*  Store Colors for X */
    for (i = 0; i < NBRE_COLOR; i++){
      /* Parse color from name to RGB value */
      if (XParseColor (x_server, *colormap, color_name[i], &color_value[i]) == 0) {
#ifdef DEBUG
        printf ("X_COLOR : X can't find color named %s.\n", color_name[i]);
#endif
        return (False);
      }
    }

    /* Init colors in non blinking */
    (void) col_set_blinking (x_server, x_screen, color_id, *colormap, False);
    XInstallColormap (x_server, *colormap);
    return (True);
}


void col_close(Display *x_server, int x_screen, unsigned long color_id[], Colormap colormap) {

    XFreeColors (x_server, colormap, color_id, color_tab_size, 0);
}

/* Checks a color number (in set_attributes) */
boolean col_check(int color_id) {

    return (color_id < NBRE_COLOR);
}

/* Gives the normal value for a color */
int col_get_std (int background, int foreground, unsigned long color_id[]) {

    switch (blink_kind) {
      case full_blink :
        return (color_id[foreground * NBRE_COLOR + foreground]);
      break;
      case on_0_blink :
        return (color_id[foreground]);
      break;
      case bold_blink :
        return (color_id[foreground]);
      break;
    }
}

/* Gives the blinking value for a color */
int col_get_blk (int background, int foreground, unsigned long color_id[]) {

    switch (blink_kind) {
      case full_blink :
        return (color_id[background * NBRE_COLOR + foreground]);
      break;
      case on_0_blink :
        return (color_id[foreground + NBRE_COLOR]);
      break;
      case bold_blink :
        return (color_id[foreground]);
      break;
    }
}

/* Sets the color map in non_blinking or in blinking state */
boolean col_set_blinking (Display *x_server, int x_screen, unsigned long color_id[],
           Colormap colormap, boolean blinking) {

XColor tab_color[NBRE_COLOR * NBRE_COLOR];
int i, j, p;


    /* Set pixel values */
    for (i = 0; i < color_tab_size; i++) {
        tab_color[i].pixel = color_id[i];
        tab_color[i].flags = DoRed | DoGreen | DoBlue;
    }

    /* Set color values */
    switch (blink_kind) {
      case full_blink :
        if (blinking) {
          for (i = 0, j = 0; i < NBRE_COLOR; i++) {
            for (p = 0; p < NBRE_COLOR; p++, j++) {
              tab_color[j].red   = color_value[i].red;
              tab_color[j].green = color_value[i].green;
              tab_color[j].blue  = color_value[i].blue;
            }
          }
        } else {
          for (i = 0, j = 0; i < NBRE_COLOR; i++) {
            for (p = 0; p < NBRE_COLOR; p++, j++) {
              tab_color[j].red   = color_value[p].red;
              tab_color[j].green = color_value[p].green;
              tab_color[j].blue  = color_value[p].blue;
          }
        }
      }
      break;
      case on_0_blink :
        if (blinking) {
          for (j = 0; j < NBRE_COLOR; j++) {
            tab_color[j].red   = color_value[j].red;
            tab_color[j].green = color_value[j].green;
            tab_color[j].blue  = color_value[j].blue;
            tab_color[j + NBRE_COLOR].red   = color_value[0].red;
            tab_color[j + NBRE_COLOR].green = color_value[0].green;
            tab_color[j + NBRE_COLOR].blue  = color_value[0].blue;
          }
        } else {
          for (j = 0; j < NBRE_COLOR; j++) {
            tab_color[j].red   = color_value[j].red;
            tab_color[j].green = color_value[j].green;
            tab_color[j].blue  = color_value[j].blue;
            tab_color[j + NBRE_COLOR].red   = color_value[j].red;
            tab_color[j + NBRE_COLOR].green = color_value[j].green;
            tab_color[j + NBRE_COLOR].blue  = color_value[j].blue;
          }
      }
      break;
      case bold_blink :
        for (j = 0; j < NBRE_COLOR; j++) {
          tab_color[j].red   = color_value[j].red;
          tab_color[j].green = color_value[j].green;
          tab_color[j].blue  = color_value[j].blue;
        }
      break;
    }


    /* Store colors */
    XStoreColors (x_server, colormap, tab_color, color_tab_size);

    return (True);
} 

