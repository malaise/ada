/* Oct 21, 1998 : Use private color map                            */
/* Nov 07, 1998 : Store size at creation of win, in pixels         */
#include <malloc.h>

#define LINE_LOCAL 
#include "x_line.h"
#include "x_file.h"

#include <X11/cursorfont.h>

#define DEF_WIN_MASK CWBackPixel | CWBorderPixel | CWEventMask | CWCursor | CWOverrideRedirect | CWBackingStore
#define DEF_WIN_ATTRIB 2, 0, InputOutput, CopyFromParent, win_mask, &win_attrib

#define NBRE_MAX_WINDOW     50
#define SIZE_MAX_NAME       50
#define STD_SUFFIX_NAME     ":0.0"

t_window *list_window[NBRE_MAX_WINDOW];
static int nbre_window = 0;


t_screen *find_screen(int screen_id);
t_screen *open_screen(int screen_id);
void close_screen (t_screen *p_screen);

/* Handler of errors on synchronous mode */
static int my_error_handler (Display *display, XErrorEvent *error_p) {

        char msg[80];

        XGetErrorText(display, error_p->error_code, msg, sizeof(msg));
        fprintf (stderr, "Error code %s\n", msg);
        return (0);
}

/* Init comunication with the local X server */
/* opens a display and create a root window. Send that back */
boolean lin_initialise (char *server_name) {

unsigned long win_mask;
XSetWindowAttributes win_attrib;
Window x_window;

    /* Open X display */
    local_server.image = False;
    local_server.x_server = XOpenDisplay (server_name);
    if (local_server.x_server == NULL) {
#ifdef DEBUG
        printf ("X_LINE : X Can't open display.\n");
#endif
        return (False);
    }
#ifdef SYNCHRO
    (void) XSynchronize (local_server.x_server, True);
#endif

    (void) XSetErrorHandler (my_error_handler);

    /* Read color, font names */
    if (!fil_init()) {
        return (False);
    }

    /* Set backing store */
      local_server.backing_store = WhenMapped;

    if (! DoesBackingStore(DefaultScreenOfDisplay(local_server.x_server)) ) {
#ifdef DEBUG
        printf ("X_LINE warning : Backing store not supported on this screen.\n");
#endif
      local_server.backing_store = NotUseful;
    }


    /* Set auto repeat */
    XAutoRepeatOn (local_server.x_server);

    /* Load fonts */
    if (!fon_open (local_server.x_server, local_server.x_font)) {
        XCloseDisplay (local_server.x_server);
        return (False);
    }

    /* Create a little window  for events */ 
    win_mask =  CWEventMask;
    win_attrib.event_mask = EVENT_MASK;

    x_window = XCreateWindow (
      local_server.x_server, 
      RootWindow (local_server.x_server, 0), 
      0, 0, 1, 1, DEF_WIN_ATTRIB);

    if (x_window == None) {
#ifdef DEBUG
        printf ("X_LINE : X Can't create initial window.\n");
#endif
        XCloseDisplay (local_server.x_server);
        return (False);
    }

    /* Ok */
    return (True);
}


/* Opens a window (opens gc,... if necessary ) */
t_window *lin_open (int screen_id, int y, int x, int height, int width,
                    int background, int border, int no_font) {

t_screen *p_screen;
t_window *p_window;
boolean screen_created;


    /* Check consistency */
    if (local_server.image) {
#ifdef DEBUG
        printf ("X_LINE : Open not IMAGE window on IMAGE display.\n");
#endif
        return (NULL);
    }

    /* Checks number of lines already open */
    if (nbre_window == NBRE_MAX_WINDOW) {
#ifdef DEBUG
        printf ("X_LINE : Too many windows.\n");
#endif
        return (NULL);
    }

    /* Parameter check */
    if ( (x<0) || (y<0) || (height<1) || (width<1) ) {
#ifdef DEBUG
        printf ("X_LINE : Parameter wrong value.\n");
#endif
        return (NULL);
    }

    /* Checks that the font number is valid */
    if (!fon_check(no_font)) {
#ifdef DEBUG
        printf ("X_LINE : Font no %d is too big.\n", no_font);
#endif
        return (NULL);
    }

    /* Check if screen already open and opens if necessary */
    if (screen_id < 0) {
        screen_id = DefaultScreen (local_server.x_server);
    }
    p_screen = find_screen (screen_id);
    if (p_screen == NULL) {
        p_screen = open_screen (screen_id);
        screen_created = True;
        if (p_screen==NULL) {
            return (NULL);
        }
    } else {
        screen_created = False;
    }

    /* Create line data */
    p_window = (t_window*) malloc (sizeof (t_window));
    if (p_window == NULL) { 
        if (screen_created) {
            close_screen(p_screen);
        }
#ifdef DEBUG
        printf ("X_LINE : Can't alloc memory for window structure.\n");
#endif
        return (NULL);
    }

    /* If font requested is 0 then x font is 0 */
    /* If font requested is 1 then x font is 2 */
    no_font = no_font * 2;

    /* Fill data */
    p_window->server = &local_server;
    p_window->screen = p_screen;
    p_window->no_font = no_font;
    p_window->background_color = background;
    p_window->underline = False;
    p_window->xor_mode = False;
    p_window->motion_enabled = False;
    p_window->cur_row = 0;
    p_window->cur_column = 0;
    p_window->nbre_key = 0;
    p_window->button = 0;
    p_window->tid_x = 0;
    p_window->tid_y = 0;

    /* Graphic context of the window */
    {
        unsigned long gc_mask;
        XGCValues gc_values;

        gc_mask = GCForeground | GCBackground | GCFont;
        gc_values.font = p_window->server->x_font[no_font]->fid;
        gc_values.foreground = col_get_std(1, 1, p_window->screen->color_id);
        gc_values.background = col_get_std(0, 0, p_window->screen->color_id);

        p_window->x_graphic_context = XCreateGC (p_window->server->x_server, 
                          p_window->screen->x_root_win, gc_mask, &gc_values);
    }

    /* Verify success */
    if (p_window->x_graphic_context == NULL) {
#ifdef DEBUG
        printf ("X_LINE : X can't open graphic context for window.\n");
#endif
        if (screen_created) {
            close_screen(p_screen);
        }
        return ((t_window*)NULL);
    }

    /* Create X window */
    {
        unsigned long win_mask;
        XSetWindowAttributes win_attrib;
        int x_pix, y_pix;

        x_pix = x * fon_get_width(p_window->server->x_font[no_font]);
        y_pix = y * fon_get_height(p_window->server->x_font[no_font]);
        p_window->wwidth = width * fon_get_width(p_window->server->x_font[no_font]);
        p_window->wheight = height * fon_get_height(p_window->server->x_font[no_font]);
        win_mask = DEF_WIN_MASK; 
        win_attrib.background_pixel = 
          col_get_std(background, background, p_window->screen->color_id);
        win_attrib.border_pixel = 
          col_get_std(border, border, p_window->screen->color_id);
        win_attrib.event_mask = EVENT_MASK;
        win_attrib.cursor = XCreateFontCursor (p_window->server->x_server, XC_left_ptr);
        win_attrib.override_redirect = False;
        win_attrib.backing_store = p_window->server->backing_store;
        p_window->x_window = XCreateWindow (
          p_window->server->x_server, p_window->screen->x_root_win, 
          x_pix, y_pix, p_window->wwidth, p_window->wheight, DEF_WIN_ATTRIB);
        if (p_window->x_window == None) {
#ifdef DEBUG
            printf ("X_LINE : X can't create window.\n");
#endif
            XFreeGC (p_window->server->x_server, p_window->x_graphic_context);
            if (screen_created) {
                close_screen(p_screen);
            }
            free (p_window);
            return (NULL);
        }
    }

    XSetWindowColormap (p_window->server->x_server, p_window->x_window, 
                        p_window->screen->colormap);

    list_window[nbre_window] = p_window;
    nbre_window ++;

    /* Map Window */
    XMapWindow (p_window->server->x_server, p_window->x_window); 
 
    /* Wait for Window exposure event and then draw */
    {
        XEvent event;
        XMaskEvent(p_window->server->x_server, ExposureMask, &event);
    }


    return (p_window);
}

/* Closes a line. Eventualy closes the screen and the X server */
int lin_close (t_window *p_window) {

int i, no;
boolean found;

    if ( ! lin_check (p_window)) {
        return (False);
    }

    /* Free graphic context */
    XFreeGC (p_window->server->x_server, p_window->x_graphic_context);

    /* Delete window (if not image) */
    if (! p_window->server->image) {
        XDestroyWindow (p_window->server->x_server, p_window->x_window);
    }

    /* Check if it is the last window of its screen */
    found = False;
    for (i=0; i<nbre_window; i++) {
        if ( (list_window[i]->screen == p_window->screen) &&
          (list_window[i] != p_window) ) {
            found = True;
            break;
        }
    }
    if (! found) {
        /* This is the last window of this screen */
        close_screen (p_window->screen);
    }

    /* Free memory */
    free (p_window);

    /* Reorganise array of windows */
    for (i=0; i<nbre_window; i++) {
        if (list_window[i] == p_window) {
            no = i;
            break;
        }
    }
    nbre_window--;
    for (i=no; i<nbre_window; i++) {
        list_window[i] = list_window[i+1];
    }
    return (True);
}

/* Clears a line */
int lin_clear (t_window *p_window) {

    if ( ! lin_check (p_window)) {
        return (False);
    }

    /* CLear line */
    XClearWindow (p_window->server->x_server, p_window->x_window);

    /* Reset underline attribute */
    p_window->underline = False;

    /* Reset graphic context */
    scr_set_attrib (p_window->server->x_server, p_window->x_graphic_context, 
      p_window->server->x_font, p_window->no_font,
      p_window->screen->color_id, 0, 1, 
      False, False, False);

    /* Cursor at home */
    p_window->cur_row = 0;
    p_window->cur_column = 0;

    return(True);
}


/* Verifies that a line is open */
boolean lin_check (t_window *p_window) {

    return (p_window != NULL);
}


/* Gives the t_window reference of a x_window (sequencial search) */
t_window *lin_get_win (Window x_window) {

int i;

    for (i=0; i<nbre_window; i++) {
        if (list_window[i]->x_window == x_window) {
            return (list_window[i]);
        }
    }
    return (NULL);
}


/* Finds the adress of the screen (if it exists, otherwise NULL) */
t_screen *find_screen (int screen_id) {

int i;

    for (i=0; i<nbre_window; i++) { 
        if (screen_id == list_window[i]->screen->x_screen) {
            return (list_window[i]->screen);
        }
    }
    return (NULL);
}


/* Opens a screen and returns it's adress */
t_screen *open_screen (int screen_id) {

t_screen *p_screen;


    /* Alloc structure */
    p_screen = (t_screen*) malloc (sizeof (t_screen));
    if (p_screen == NULL) {
#ifdef DEBUG
        printf ("X_LINE : Can't alloc memory for screen structure.\n");
#endif
        return (NULL);
    }
    p_screen->server = &local_server;
    p_screen->x_screen = screen_id;

    /* Root window */
    p_screen->x_root_win = RootWindow (p_screen->server->x_server, p_screen->x_screen);

    /* Loads and inits colors */
    if (! col_open (p_screen->server->x_server, p_screen->x_screen,
                    p_screen->color_id, &(p_screen->colormap)) ) {
        free (p_screen);
        return (NULL);
    }


    /* Success */
    p_screen->blinking = False;
    return (p_screen);
} 

/* Closes a screen. */
void close_screen (t_screen *p_screen) {


    /* Free colors */
    if (! p_screen->server->image) {
        col_close (p_screen->server->x_server, p_screen->x_screen, p_screen->color_id,
                   p_screen->colormap);
    }

    /* Free memory */
    free (p_screen);
}



/* Swaps the color map for each open window */
void lin_blink_colors(blink)
    boolean blink;
{
int i;

    /* No blink with Image */
    if (local_server.image) return;

    for (i=0; i<nbre_window; i++) {
        if (blink != list_window[i]->screen->blinking) {
            /* Screen is not in the proper colors */
            col_set_blinking (list_window[i]->server->x_server,
                              list_window[i]->screen->x_screen,
                              list_window[i]->screen->color_id,
                              list_window[i]->screen->colormap,
                              blink);
        }
        /* Screen is now in the proper colors */
        list_window[i]->screen->blinking = blink;
    }
}
