/* Oct 21, 1998 : Use private color map                            */
/* Nov 07, 1998 : Store size at creation of win, in pixels         */
#ifndef _X_LINE_H
#define _X_LINE_H

#include "x_stdinc.h"
#include "x_font.h"
#include "x_color.h"
#include "x_screen.h"
#include "x_key.h"

/* #define EVENT_MASK ButtonReleaseMask | ButtonPressMask | KeyPressMask | ExposureMask | ResizeRedirectMask | StructureNotifyMask */
#define EVENT_MASK ButtonReleaseMask | ButtonPressMask | KeyPressMask | ExposureMask | EnterWindowMask

/* Number of supported types of XA_PRIMARY clipborad exchanges */
#define NB_SELECTION_TYPES 3

typedef struct {
    Display *x_server;
    XFontSet x_font_set[NBRE_FONT];
    XFontStruct *x_font[NBRE_FONT];
    XIM xim;
    XIMStyle xim_style;
    int backing_store;
    Atom wm_protocols_code;
    Atom delete_code;
    Atom selection_types[NB_SELECTION_TYPES];
    Atom select_code;
}t_server;

typedef struct {
    /* Link */
    t_server *server;
    /* X reference */
    int x_screen;
    /* X root window */
    Drawable x_root_win;
    /* X color pixels */
    boolean blinking;
    unsigned long color_id[MAX_SIZE_TAB_COLOR];
    Colormap colormap;
}t_screen;

/* Specific values of select_index, other (natural) values
  are indexes in selection_types */
#define SELEC_NONE -1
#define SELEC_STORED 1000  /* Stored index + SELEC_STORED */

typedef struct {
    /* Links */
    t_server *server;
    t_screen *screen;
    /* X reference */
    Window x_window;
    /* Font id specified in open */
    int no_font;
    /* Background specifierd in open */
    int background_color;
    /* Size at creation */
    int wwidth, wheight;
    /* Graphic characteristics */
    boolean underline;
    boolean bold;
    boolean xor_mode;
    GC x_graphic_context;
    XIC xic;
    /* Event characteristics */
    boolean motion_enabled;
    boolean control;
    boolean shift;
    boolean code;
    int key_buf[NBRE_MAX_KEY];
    int nbre_key;
    int button;
    int tid_x;
    int tid_y;
    char *selection;
    int nbre_drop_clear;
    int select_index;
}t_window;

/* extern for all includer except x_line.c */
/* The X server of this host */
#ifdef LINE_LOCAL
    t_server local_server;
#else
    extern t_server local_server;
#endif

/* Initialize a line (return True if OK) */
boolean lin_initialise (const char *server_name);

/* Open a line and gives it's Line_id, or NULL if error */
t_window *lin_open(int screen_id, int y, int x, int height, int width,
                   int background, int border, int no_font);

/* Close a line */
boolean lin_close(t_window *p_window);

/* Clears a line */
boolean lin_clear(t_window *p_window);

/* Verifies that a line is open */
boolean lin_check(t_window *p_window);

/* Gives the t_window reference of a x_window (sequencial search) */
/* Returns NULL if error */
t_window *lin_get_win (Window x_window);

/* Alternatively swaps colors for blinking */
void lin_blink_colors(boolean blink);

/* Get font no for line (bold or not) */
int lin_get_font (t_window *p_window);

#endif
/* _X_LINE_H */

