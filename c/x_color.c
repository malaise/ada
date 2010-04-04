/* Oct 23, 1998 : Use private color map                   (PM)     */
#include <stdlib.h>
#include <string.h>

#include "x_color.h"

static boolean color_set = False;
static boolean envs_got = False;

#define N_PLANES 0

#define X_COLOR_MAP "X_COLOR_MAP"
#define PRIVATE_COLOR_MAP   "PRIVATE"
#define PUBLIC_COLOR_MAP    "PUBLIC"
static boolean private_color_map;

static char color_name[NBRE_COLOR][COLOR_NAME_MAX_SIZE];
static XColor color_value[NBRE_COLOR];

static const char *default_color_name[NBRE_COLOR]={
  "black",      "blue",
  "dark green", "cyan",
  "red",        "magenta",
  "brown",      "light grey",
  "grey",       "light blue",
  "lime green", "orange",
  "yellow",     "white"
 };

extern void col_set_names (const char* names[]) {
  int i;

  for (i = 0; i < NBRE_COLOR; i++) {
    strcpy (color_name[i], names[i]);
  }
  color_set = True;
}


static void get_envs (void) {
  char * color_map_type;

  /* Get env color map type */
  color_map_type = getenv(X_COLOR_MAP);
  private_color_map = (color_map_type != NULL)
                   && (strcmp(color_map_type, PRIVATE_COLOR_MAP) == 0);

  /* Set default color name if not set */
  if (! color_set) {
    col_set_names (default_color_name);
  }

  envs_got = True;
}

/* Loads the colors in the server */
boolean col_open(Display *x_server, int x_screen, unsigned long color_id[],
                 Colormap *colormap) {
  int i, cr;
  unsigned long plane_mask[1];
  XColor exact_color_value[NBRE_COLOR];


  if (!envs_got) get_envs();

  /* Create / get color map */
  if (private_color_map) {
    *colormap = XCreateColormap(x_server,
                                RootWindow (x_server, x_screen),
                                XDefaultVisual (x_server, x_screen),
                                AllocNone);
    /* Alloc colors */
    cr = XAllocColorCells (x_server, *colormap, False, plane_mask, N_PLANES,
                           color_id, (unsigned int) NBRE_COLOR);
    if (cr == 0) {
#ifdef DEBUG
      printf ("X_COLOR : X can't alloc %d cells for colors.\n",
               NBRE_COLOR);
#endif
      return (False);
    }

    /* Parse color names */
    for (i = 0; i < NBRE_COLOR; i++) {
      /* Parse color from name to RGB value */
      if (XParseColor (x_server, *colormap, color_name[i],
                         &color_value[i]) == 0) {
#ifdef DEBUG
        printf ("X_COLOR : X can't find color named %s.\n", color_name[i]);
#endif
        return (False);
      }
    }

    /* Store colors */
    XInstallColormap (x_server, *colormap);

  } else {
    *colormap = DefaultColormap (x_server, x_screen);

    /* Simple colors */
    /* Get pixels of colors  and store them */
    for (i = 0; i < NBRE_COLOR; i++) {
      if (XAllocNamedColor (x_server, *colormap, color_name[i],
                            &color_value[i], &exact_color_value[i]) == 0) {
#ifdef DEBUG
        printf ("X_COLOR : X can't alloc color named %s\n", color_name[i]);
#endif
        return (False);
      } else {
        color_id[i] = color_value[i].pixel;
      }
    }
  }

  return (True);
}


void col_close(Display *x_server, unsigned long color_id[],
               Colormap colormap) {

  XFreeColors (x_server, colormap, color_id, NBRE_COLOR, 0);
}

/* Checks a color number (in set_attributes) */
boolean col_check(int color_id) {

  return (color_id < NBRE_COLOR);
}

/* Gives the normal value for a color */
unsigned long col_get (int foreground,
                           unsigned long color_id[]) {
  return (color_id[foreground]);
}

