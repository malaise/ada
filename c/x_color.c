/*  @(#) TAAATS PROGRAM FILE %M% Release %I% %H% %T% ~  */
static char C_ROLOC_X []=" @(#) TAAATS PROGRAM FILE %M% Release %I% %H% %T% ~";
/* Sep 05, 1993 : Image compliant X ressources management (PM)     */
#include "x_color.h"

#define N_PLANES 0

XColor color_value[NBRE_COLOR];

/* Loads the colors in the server */
boolean col_open(Display *x_server, int x_screen, unsigned long color_id[]) {
int i, cr;
Colormap colmap;
unsigned long plane_mask[1];

 
    /* Alloc colors */
    colmap = DefaultColormap (x_server, x_screen);
    cr = XAllocColorCells (x_server, colmap, False, plane_mask, N_PLANES,
     color_id, SIZE_TAB_COLOR);
    if (cr == 0) {
#ifdef DEBUG
      printf ("X_COLOR : X can't alloc cells for colors.\n");
#endif
      return (False);
    }
 
 
    /*  Store Colors for X */
    for (i=0; i<NBRE_COLOR; i++){
        /* Parse color from name to RGB value */
        if (XParseColor (x_server, colmap, color_name[i], &color_value[i]) == 0) {
#ifdef DEBUG
            printf ("X_COLOR : X can't find color named %s.\n", color_name[i]);
#endif
            return (False);
        }
  }

    /* Init colors in non blinking */
    return (col_set_blinking (x_server, x_screen, color_id, False));
}

/* Loads the colors in the server */
boolean col_open_image(unsigned long color_ids[], unsigned long colors[]) {
                       /* in                      out */

int i, j;
 
    /* Store colors no blink */
    for (i=0; i<NBRE_COLOR; i++) {
        for (j=0; j<NBRE_COLOR; j++) {
            colors[i * NBRE_COLOR + j] = color_ids[i];
        }
    }

    /* Ok */
    return (True);
}

void col_close(Display *x_server, int x_screen, unsigned long color_id[]) {

    XFreeColors (x_server, DefaultColormap (x_server, x_screen),
     color_id, SIZE_TAB_COLOR, 0);
}

/* Checks a color number (in set_attributes) */
boolean col_check(int color_id) {

    return (color_id < NBRE_COLOR);
}

/* Gives the normal value for a color */
int col_get_std (int background, int foreground, unsigned long color_id[]) {

    return (color_id[foreground * NBRE_COLOR + foreground]);
}

/* Gives the blinking value for a color */
int col_get_blk (int background, int foreground, unsigned long color_id[]) {

    return (color_id[background * NBRE_COLOR + foreground]);
}

/* Sets the color map in non_blinking or in blinking state */
boolean col_set_blinking (Display *x_server, int x_screen, unsigned long color_id[],
                          boolean blinking) {

Colormap col_map;
XColor tab_color[SIZE_TAB_COLOR];
int i, j, p;

    col_map = DefaultColormap (x_server, x_screen);

    /* Set pixel values */
    for (i=0; i<SIZE_TAB_COLOR; i++) {
        tab_color[i].pixel = color_id[i];
        tab_color[i].flags = DoRed | DoGreen | DoBlue;
    }

    /* Set color values */
    if (blinking) {
        for (i=0, j=0; i<NBRE_COLOR; i++) {
            for (p=0; p<NBRE_COLOR; p++, j++) {
                tab_color[j].red   = color_value[i].red;
                tab_color[j].green = color_value[i].green;
                tab_color[j].blue  = color_value[i].blue;
            }
        }
    } else {
        for (i=0, j=0; i<NBRE_COLOR; i++) {
            for (p=0; p<NBRE_COLOR; p++, j++) {
                tab_color[j].red   = color_value[p].red;
                tab_color[j].green = color_value[p].green;
                tab_color[j].blue  = color_value[p].blue;
            }
        }
    }

    /* Store colors */
    XStoreColors (x_server, col_map, tab_color, SIZE_TAB_COLOR);

    return (True);
} 

