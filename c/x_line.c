/* Oct 21, 1998 : Use private color map                            */
/* Nov 07, 1998 : Store size at creation of win, in pixels         */
#include <stdlib.h>
#include <malloc.h>
#include <locale.h>


#define LINE_LOCAL
#include "x_line.h"
#include "x_file.h"

#include <X11/cursorfont.h>
#include <X11/Xutil.h>

#define DEF_WIN_MASK CWBackPixel | CWBorderPixel | CWEventMask | CWCursor | CWOverrideRedirect | CWBackingStore
#define DEF_WIN_ATTRIB 2, 0, InputOutput, CopyFromParent, win_mask, &win_attrib

#define NBRE_MAX_WINDOW     50
#define SIZE_MAX_NAME       50
#define STD_SUFFIX_NAME     ":0.0"

const char *selection_type_names[NB_SELECTION_TYPES] =
   {"UTF8_STRING", "STRING", "TEXT"};

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
boolean lin_initialise (const char *server_name) {

unsigned long win_mask;
XSetWindowAttributes win_attrib;
Window x_window;

    char *modifiers;
    XIMStyles *xim_styles;
    char *imvalret;
    int i;

    if (setlocale (LC_ALL, "") == NULL) {
        printf ("X_LINE : X cannot set locale.\n");
        return (False);
    }
    if (!XSupportsLocale()) {
        printf ("X_LINE : X does not support locale %s.",
                setlocale (LC_ALL, NULL));
        return (False);
    }


    /* Open X display */
    local_server.x_server = XOpenDisplay (server_name);
    if (local_server.x_server == NULL) {
        printf ("X_LINE : X Can't open display %s.\n", server_name);
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

    local_server.backing_store = NotUseful;

    /* Set default input method */
    modifiers = XSetLocaleModifiers ("@im=none");
    if (modifiers == NULL) {
        printf ("X_LINE : Can't set locale modifiers.\n");
    }

    local_server.xim_style = 0;
    local_server.xim = XOpenIM (local_server.x_server, NULL, NULL, NULL);
    if (local_server.xim == NULL) {
#ifdef DEBUG
        printf ("X_LINE : Can't open input method.\n");
        xim_styles = NULL;
#endif
    } else {
        imvalret = XGetIMValues (local_server.xim, XNQueryInputStyle,
                                 &xim_styles, NULL);
        if (imvalret != NULL || xim_styles == NULL) {
            xim_styles = NULL;
#ifdef DEBUG
            printf ("X_LINE: Input method doesn't support any style.\n");
#endif
        }
    }

    if (xim_styles != NULL) {
        for (i = 0;  i < xim_styles->count_styles;  i++) {
            if (xim_styles->supported_styles[i] ==
                    (XIMPreeditNothing | XIMStatusNothing)) {
                local_server.xim_style = xim_styles->supported_styles[i];
             break;
             }
        }

        if (local_server.xim_style == 0) {
            printf ("X_LINE: Input method doesn't support the expected style.\n");
        }
        XFree (xim_styles);
    }

    /* Set auto repeat */
    XAutoRepeatOn (local_server.x_server);

    /* Load fonts */
    if (!fon_open (local_server.x_server,
                   local_server.x_font_set,
                   local_server.x_font)) {
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
        printf ("X_LINE : X Can't create initial window.\n");
        XCloseDisplay (local_server.x_server);
        return (False);
    }

    /* Declare/store Atoms for WM_protocol and delete_window request */
    local_server.wm_protocols_code = XInternAtom (local_server.x_server,
                                            "WM_PROTOCOLS", False);
    if (local_server.wm_protocols_code != None) {
      local_server.delete_code = XInternAtom (local_server.x_server,
                                              "WM_DELETE_WINDOW", False);
    } else {
      local_server.delete_code = None;
    }

    /* Initialise Atoms of supported PRIMARY clipborad formats */
    for (i = 0; i < NB_SELECTION_TYPES; i++) {
      local_server.selection_types[i] = XInternAtom (local_server.x_server,
                             selection_type_names[i], True);
    }

    /* Declare/store Atom for receiving selection */
    local_server.select_code = XInternAtom (local_server.x_server,
                                            "X_MNG_SELECTION", False);

    /* Ok */
    return (True);
}


/* Opens a window (opens gc,... if necessary ) */
t_window *lin_open (int screen_id, int y, int x, int height, int width,
                    int background, int border, int no_font) {

t_screen *p_screen;
t_window *p_window;
boolean screen_created;
Status res;

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
    p_window->bold = False;
    p_window->xor_mode = False;
    p_window->motion_enabled = False;
    p_window->nbre_key = 0;
    p_window->button = 0;
    p_window->tid_x = 0;
    p_window->tid_y = 0;
    p_window->selection = NULL;
    p_window->nbre_drop_clear = 0;
    p_window->select_index = SELEC_NONE;
    p_window->cursor = XCreateFontCursor(p_window->server->x_server, XC_arrow);

    /* Init subwindows */
    p_window->last_subwindow.window = None;
    p_window->last_subwindow.ref = NULL;
    p_window->nbre_subwindows = 0;

    /* Graphic context of the window */
    {
        unsigned long gc_mask;
        XGCValues gc_values;

        gc_mask = GCForeground | GCBackground | GCFont;
        gc_values.font = p_window->server->x_font[no_font]->fid;
        gc_values.foreground = col_get(1, p_window->screen->color_id);
        gc_values.background = col_get(0, p_window->screen->color_id);

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
        p_window->wwidth = width
            * fon_get_width(p_window->server->x_font[no_font]);
        p_window->wheight = height
            * fon_get_height(p_window->server->x_font[no_font]);
        win_mask = DEF_WIN_MASK;
        win_attrib.background_pixel =
          col_get(background, p_window->screen->color_id);
        win_attrib.border_pixel =
          col_get(border, p_window->screen->color_id);
        win_attrib.event_mask = EVENT_MASK;
        win_attrib.cursor = XCreateFontCursor (p_window->server->x_server, XC_left_ptr);
        win_attrib.override_redirect = False;
        win_attrib.backing_store = p_window->server->backing_store;
        p_window->x_window = XCreateWindow (
          p_window->server->x_server, p_window->screen->x_root_win,
          x_pix, y_pix, (unsigned int)p_window->wwidth,
          (unsigned int)p_window->wheight, DEF_WIN_ATTRIB);
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

    if ( (p_window->server->xim != NULL)
      && (p_window->server->xim_style != 0) ) {
        p_window->xic = XCreateIC (p_window->server->xim,
                        XNInputStyle, p_window->server->xim_style,
                        XNClientWindow, p_window->x_window,
                        XNFocusWindow, p_window->x_window,
                        NULL);

        if (p_window->xic == NULL) {
#ifdef DEBUG
            printf ("X_LINE : X Can't create input method context.\n");
#endif
        }
    }

    /* Set protocol to receive DELETE_WINDOW requests from window manager */
    res =  XSetWMProtocols (p_window->server->x_server, p_window->x_window, &p_window->server->delete_code, 1);
    if (res == 0) {
            printf ("X_LINE : X Can set delete-window WM protocol.\n");
    }

    /* Force fixed size in the window manager */
    {
        XSizeHints hints;
        hints.min_width = (unsigned int)p_window->wwidth;
        hints.max_width = (unsigned int)p_window->wwidth;
        hints.min_height = (unsigned int)p_window->wheight;
        hints.max_height = (unsigned int)p_window->wheight;
        hints.flags = PMaxSize|PMinSize;
        XSetWMNormalHints(p_window->server->x_server, p_window->x_window,
                          &hints);
    }

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

    /* Delete window */
    XDestroyWindow (p_window->server->x_server, p_window->x_window);

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

    /* Reset underline and superbright attributes */
    p_window->underline = False;
    p_window->bold = False;

    /* Reset graphic context */
    scr_set_attrib (p_window->server->x_server, p_window->x_graphic_context,
      p_window->server->x_font, p_window->no_font,
      p_window->screen->color_id, 1, 0, False);

    return(True);
}


/* Verifies that a line is open */
boolean lin_check (t_window *p_window) {

    return (p_window != NULL);
}


/* Gives the t_window reference of a x_window (sequencial search) */
t_window *lin_get_win (Window x_window) {

int i, j, l, h;

    /* Search in windows for the event windows */
    for (i=0; i<nbre_window; i++) {
        if (list_window[i]->x_window == x_window) {
            /* Parent window */
            list_window[i]->last_subwindow.window = None;
            list_window[i]->last_subwindow.ref = NULL;
            return (list_window[i]);
        } else if (list_window[i]->last_subwindow.window == x_window) {
            /* Same subwindow as before */
            return (list_window[i]);
        }
    }
    /* Search in subwindows */
    for (i=0; i<nbre_window; i++) {
        /* Dichotomy */
        l = 0; h = list_window[i]->nbre_subwindows;
        while (l < h) {
            j = (l + h ) / 2;
            if (list_window[i]->subwindows[j].window == x_window) {
                list_window[i]->last_subwindow = list_window[i]->subwindows[j];
                return (list_window[i]);
            }
            if (list_window[i]->subwindows[j].window < x_window) {
                l = j + 1;
            } else {
                h = j;
            }
        }
        if (l >= h) {
            return (NULL);
        }
        if (list_window[i]->subwindows[l].window == x_window) {
            list_window[i]->last_subwindow = list_window[i]->subwindows[l];
            return (list_window[i]);
        }
    }

    /* Not found */
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
    return (p_screen);
}

/* Closes a screen. */
void close_screen (t_screen *p_screen) {


    /* Free colors */
    col_close (p_screen->server->x_server, p_screen->color_id,
               p_screen->colormap);

    /* Free memory */
    free (p_screen);
}

/* Get font no for line (bold or not) */
int lin_get_font (t_window *p_window) {
    if (p_window->bold) {
        return fon_get_bold (p_window->no_font);
    } else {
        return p_window->no_font;
    }
}

/* (Un) Register a subwindow of the line */
int lin_register (t_window *p_window, Window window, void *ref, boolean add) {

int i, j;

    if (add) {
        /* Insert if not full */
        if (p_window->nbre_subwindows == MAX_SUBWINDOWS) {
            return (False);
        }
        /* Keep list sorted by crescent window */
        for (i=0; i<p_window->nbre_subwindows; i++) {
            if (p_window->subwindows[i].window > window) {
                break;
            }
        }
        /* Shift right */
        for (j=p_window->nbre_subwindows; j>i; j--) {
            p_window->subwindows[j] = p_window->subwindows[j-1];
        }
        p_window->subwindows[p_window->nbre_subwindows].window = window;
        p_window->subwindows[p_window->nbre_subwindows].ref = ref;
        (p_window->nbre_subwindows)++;
        return (True);
    } else {
        /* Locate and shift (overwriting it) */
        for (i=0; i<p_window->nbre_subwindows; i++) {
            if (p_window->subwindows[i].window == window) {
                for (j=i + 1; j<p_window->nbre_subwindows; j++) {
                    p_window->subwindows[j-1] = p_window->subwindows[j];
                    (p_window->nbre_subwindows)--;
                }
                return (True);
            }
        }
        /* Not found */
        return (False);
    }
}

