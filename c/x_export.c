#include <string.h>
#include <malloc.h>
#include <unistd.h>
#include <X11/cursorfont.h>
#include <X11/Xatom.h>

#include "x_export.h"
#include "x_screen.h"
#include "x_line.h"
#include "timeval.h"

/* The maximum of sucessive bells */
#define NBRE_MAX_BELL 5

/* The delay in milli seconds after receiving 1st expose */
/* before looking for any further expose (and filter) */
#define DELAY_EXPOSE_MS 25

/*
 * static void print_date (void) {
 *   timeout_t cur_time;
 *   get_time (&cur_time);
 *   printf ("    >> %06d %06d << ", (int)cur_time.tv_sec,
 *                                   (int)cur_time.tv_usec);
 * }
 */

/***** Blinking management *****/
/* The percentage of blinking when colors are seeable */
#define PERCENT_ON  75
#define PERCENT_OFF (100 - PERCENT_ON)
int curr_percent = 0;

timeout_t next_blink;

/* The real blinking routine */
static void x_do_blink (void);

/* The blinking task */
static void x_do_blinking (void) {

  /* Protection */
  if (curr_percent == 0) {
    return;
  }
  x_do_blink();
  if (curr_percent == PERCENT_OFF) {
    curr_percent = PERCENT_ON;
  } else {
    curr_percent = PERCENT_OFF;
  }
  (void) incr_time (&next_blink, (unsigned int)curr_percent * 10);
}

extern int x_stop_blinking (void) {

  /* Set to the non_blink state */
  if (curr_percent == PERCENT_OFF) {
    x_do_blink();
  }
  curr_percent = 0;
  return (OK);
}

extern int x_start_blinking (void) {
  curr_percent = PERCENT_ON;
  get_time (&next_blink);
  (void) incr_time (&next_blink, (unsigned int)curr_percent * 10);
  return (OK);
}

static int timeout_to_ms (timeout_t *p_time) {
  return (p_time->tv_sec * 1000 + p_time->tv_usec / 10000);
}


/***** Event management *****/
static void x_clear_in_selection (t_window *win_id ) {
    /* Clean previous in selection */
    if (win_id->select_index >= SELEC_STORED) {
        XDeleteProperty (win_id->server->x_server,
                         win_id->x_window,
                         win_id->server->select_code);
        win_id->select_index = SELEC_NONE;
    }
}
static void x_request_in_selection (t_window *win_id ) {
    /* Request a selection with a type */
    (void) XConvertSelection (win_id->server->x_server, XA_PRIMARY,
        win_id->server->selection_types[win_id->select_index],
        win_id->server->select_code, win_id->x_window, CurrentTime);
}

extern int x_select (int *p_fd, boolean *p_read, int *timeout_ms) {
  boolean timeout_is_active, blink_is_active;
  timeout_t exp_time, cur_time, tmp_timeout;
  timeout_t *select_exp;
  int select_ms;
  int x_fd;
  int res;

  timeout_is_active = *timeout_ms >= 0;
  blink_is_active = (curr_percent != 0);

  if (local_server.x_server == NULL) {
    /* Cannot be set by wait_evt */
    x_fd = X_EVENT;
  } else {
    XFlush (local_server.x_server);
    x_fd = ConnectionNumber (local_server.x_server);
    /* Don't wait if an event is pending */
    res = XPending (local_server.x_server);
    if (res > 0) {
      *p_fd = X_EVENT;
      *p_read = TRUE;
      return (OK);
    } else if (res < 0) {
      return (ERR);
    }
  }

  /* Compute expiration time */
  if (timeout_is_active) {
    get_time (&exp_time);
    incr_time (&exp_time, (unsigned int)*timeout_ms);
  }

  for (;; ) {


    /* Compute select expiration time */
    /* Nearest between exp_time and next_blink */
    if (blink_is_active) {
      if (timeout_is_active) {
        if (comp_time(&next_blink, &exp_time) < 0) {
          select_exp = &next_blink;
        } else {
          select_exp = &exp_time;
        }
      } else {
        select_exp = &next_blink;
      }
    } else {
      if (timeout_is_active) {
        select_exp = &exp_time;
      } else {
        select_exp = NULL;
      }
    }

    /* Compute select timeout */
    if (select_exp == NULL) {
      select_ms = -1;
    } else {
      get_time(&cur_time);
      memcpy (&tmp_timeout, select_exp, sizeof(timeout_t));
      if (sub_time (&tmp_timeout, &cur_time) <= 0 ) {
        select_ms = 0;
      } else {
        select_ms = timeout_to_ms (&tmp_timeout);
      }
    }

    /* Call the real select */
    res = evt_wait (p_fd, p_read, &select_ms);
    if (res == ERR) {
      return (ERR);
    }

    /* Check Blink */
    if ( (blink_is_active) && time_is_reached (&next_blink) ) {
      /* Blink and continue */
      x_do_blinking();
    }

    if (*p_fd != NO_EVENT) {
      /* Some event: Check for X event */
      if (*p_fd == x_fd) {
        *p_fd = X_EVENT;
        *p_read = TRUE;
      }
      /* Done */
      evt_time_remaining (timeout_ms, &exp_time);
      return (OK);
    } else {
      /* Timeout. Check expiration */
      if (timeout_is_active && time_is_reached (&exp_time) ) {
        *timeout_ms = 0;
        return (OK);
      }
    }

  } /* For */
}

/***** Line management *****/

/* Initialise communication with the 'local' X server */
/*  opening a little window */
/* Hangs handler for blinking */
extern int x_initialise (const char *server_name) {

    int result;

    result = (lin_initialise (server_name) ? OK : ERR);
    if (result == OK) {
      result = evt_add_fd (ConnectionNumber(local_server.x_server), TRUE);
    }

    if ( (result == OK) && (!blink_bold()) ) {
      (void) x_start_blinking();
    }
    return (result);
}


/* Opens a line */
extern int x_open_line (int screen_id, int row, int column,
  int height, int width,
  int background, int border, int no_font, void **p_line_id) {

    t_window *line;

    line = lin_open (screen_id, row-1, column-1,
      height, width, background, border, no_font);
    if (line != NULL) {
        *p_line_id = (void*) line;
        return (OK);
    } else {
        return (ERR);
    }
}

/* Closes a line */
/* The line_id is the token, previously given by open_line */
extern int x_close_line (void *line_id) {
    int result;

    result = (lin_close( (t_window*) line_id) ? OK : ERR);

    return (result);
}

/* Set line name */
extern int x_set_line_name (void *line_id, const char *line_name) {
    int result;

    t_window *win_id = (t_window*) line_id;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    result = XStoreName(local_server.x_server, win_id->x_window, line_name);
    /* Strange: it works but returns error */
    /*    return ((result == Success) ? OK : ERR); */
    return (OK);
}



/* Flushes all the lines on this host line */
extern int x_flush (void) {

    /* Check that display is init */
    if (local_server.x_server == NULL) {
        return (ERR);
    }

    /* Flush the outputs */
    XFlush (local_server.x_server);

    return (OK);
}

/* Clears a line */
extern int x_clear_line (void *line_id) {
    int result;


    result = (lin_clear( (t_window*) line_id) ? OK : ERR);

    return(result);
}

/***** Put and attributes management *****/

/* Sets the attributes for a further put in the same window */
extern int x_set_attributes (void *line_id, int paper, int ink,
  boolean superbright, boolean underline, boolean blink, boolean reverse) {

    t_window *win_id = (t_window*) line_id;
    int no_font;

    /* Check that window is open */
        if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Checks that the colors are valid */
    if ( (! col_check (paper)) || (! col_check (ink)) ) {
        return (ERR);
    }

    /* Store underline and superbright attribute */
    win_id->underline = underline;
    win_id->bold = (superbright || (blink && blink_bold() ) );

    /* Update graphic context */
    no_font = lin_get_font (win_id);
    scr_set_attrib (win_id->server->x_server,
      win_id->x_graphic_context, win_id->server->x_font,
      lin_get_font(win_id), win_id->screen->color_id,
      paper, ink, blink, reverse);

    return (OK);

}

/* Set further put on window in Xor or back to Copy mode */
extern int x_set_xor_mode (void *line_id, boolean xor_mode) {
    t_window *win_id = (t_window*) line_id;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    if (xor_mode) {
        XSetFunction(win_id->server->x_server,
                     win_id->x_graphic_context, GXxor);
    } else {
        XSetFunction(win_id->server->x_server,
                     win_id->x_graphic_context, GXcopy);
    }
    win_id->xor_mode = xor_mode;
    return (OK);
}

/* Writes a char whith the attributes previously set */
/* The line_id is the token, previously given by open_line */
/* The character is the one to be written */
/* The output is not flushed */
extern int x_put_char (void *line_id, int car, int row, int column) {

    int x, y;
    t_window *win_id = (t_window*) line_id;
    int no_font = win_id->no_font;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Compute pixels */
    x = column *
      fon_get_width  (win_id->server->x_font[no_font]);
    y = row *
      fon_get_height (win_id->server->x_font[no_font]) +
      fon_get_offset (win_id->server->x_font[no_font]);

    /* Put char */
    scr_put_char (win_id->server->x_server,
      win_id->x_graphic_context,
      win_id->x_window,
      win_id->server->x_font_set[lin_get_font(win_id)],
      x, y, (char)car, win_id->xor_mode);

    /* Underline */
    if (win_id->underline) {
        scr_underline_char (win_id->server->x_server,
          win_id->x_graphic_context, win_id->x_window, x, y);
    }

    return (OK);
}

/* Writes a char whith the attributes previously set */
/* The line_id is the token, previously given by open_line */
/* The character is the one to be written */
/* The output is not flushed */
extern int x_overwrite_char (void *line_id, int car, int row, int column) {

    int x, y;
    t_window *win_id = (t_window*) line_id;
    int no_font = win_id->no_font;


    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Compute pixels */
    x = column *
      fon_get_width  (win_id->server->x_font[no_font]);
    y = row *
      fon_get_height (win_id->server->x_font[no_font]) +
      fon_get_offset (win_id->server->x_font[no_font]);

    /* Put char */
    scr_overwrite_char (win_id->server->x_server,
      win_id->x_graphic_context,
      win_id->x_window,
      win_id->server->x_font_set[lin_get_font(win_id)],
      x, y, (char)car);

    return (OK);
}


/* Writes a string whith the attributes previously set */
/* The line_id is the token, previously given by open_line */
/* The str is the adress of the first character to write */
/* The length is the number of characters to write */
/* The output is not flushed */
extern int x_put_string (void *line_id, const char *p_char, int number,
                  int row, int column) {

    int x, y;
    t_window *win_id = (t_window*) line_id;
    int no_font = win_id->no_font;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Compute pixels */
    x = column *
      fon_get_width  (win_id->server->x_font[no_font]);
    y = row *
      fon_get_height (win_id->server->x_font[no_font]) +
      fon_get_offset (win_id->server->x_font[no_font]);

    /* Put string */
    scr_put_string (win_id->server->x_server,
      win_id->x_graphic_context, win_id->x_window,
      win_id->server->x_font_set[lin_get_font(win_id)],
      x, y, p_char, number, win_id->xor_mode);

    /* Underline */
    if (win_id->underline) {
        scr_underline_string (win_id->server->x_server,
          win_id->x_graphic_context, win_id->x_window, x, y, number);
    }

    return (OK);
}

/* Writes a char on a line with specified characteristics */
/* The output is not flushed */
extern int x_put_char_attributes (void *line_id, int car, int row, int column,
  int paper, int ink,
  boolean superbright, boolean underline, boolean blink, boolean reverse) {

    if (x_set_attributes (line_id, paper, ink,
                   superbright, underline, blink, reverse) == ERR) {
        return (ERR);
    }

    return (x_put_char (line_id, car, row, column));
}

extern int x_draw_area (void *line_id, int width, int height,
                        int row, int column) {

    int x_from, y_from;
    int pix_width, pix_height;
    t_window *win_id = (t_window*) line_id;
    int no_font = win_id->no_font;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Compute pixels */
    x_from = column *
      fon_get_width  (win_id->server->x_font[no_font]);
    pix_width = width *
      fon_get_width  (win_id->server->x_font[no_font]);
    y_from = row *
      fon_get_height (win_id->server->x_font[no_font]);
    pix_height = height *
      fon_get_height (win_id->server->x_font[no_font]);

    /* draw */
    scr_draw_array (win_id->server->x_server,
      win_id->x_graphic_context,
      win_id->x_window, x_from, y_from, pix_width, pix_height);

    return (OK);
}

/* Give graphic characteristics of the windows and its font */
extern int x_get_graph_charact (void *line_id, int *p_w_width, int *p_w_height,
                      int *p_f_width, int *p_f_height, int *p_f_offset) {

    t_window *win_id = (t_window*) line_id;
    int no_font = win_id->no_font;


    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    *p_w_width  = win_id->wwidth;
    *p_w_height = win_id->wheight;
    *p_f_width  = fon_get_width  (win_id->server->x_font[no_font]);
    *p_f_height = fon_get_height (win_id->server->x_font[no_font]);
    *p_f_offset = fon_get_offset (win_id->server->x_font[no_font]);

    return (OK);
}

/* Writes a char on a line (characteristics are previously set) */
/* x is a number of pixels of vertical position (top down) */
/* y                          horizontal position (left right) */
/* The output is not flushed */
extern int x_put_char_pixels (void *line_id, int car, int x, int y) {

    t_window *win_id = (t_window*) line_id;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Put char */
    scr_put_char (win_id->server->x_server,
      win_id->x_graphic_context,
      win_id->x_window,
      win_id->server->x_font_set[lin_get_font(win_id)],
      x, y, (char)car, win_id->xor_mode);

    /* Underline */
    if (win_id->underline) {
      scr_underline_char (win_id->server->x_server,
      win_id->x_graphic_context,
      win_id->x_window, x, y);
    }

    return (OK);
}

/* Draw a point at x,y */
extern int x_draw_point (void *line_id, int x, int y) {
    t_window *win_id = (t_window*) line_id;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    XDrawPoint (win_id->server->x_server, win_id->x_window,
                win_id->x_graphic_context, x, y);
    return (OK);
}

/* Draw a line between  x1y1 and x2y2 */
extern int x_draw_line (void *line_id, int x1, int y1, int x2, int y2) {
    t_window *win_id = (t_window*) line_id;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Draw */
    XDrawLine (win_id->server->x_server,
      win_id->x_window,
      win_id->x_graphic_context, x1, y1, x2, y2);

    return (OK);

}

/* Draw a rectangle at x1y1, x1y2, x2y2, x2y1 */
extern int x_draw_rectangle (void *line_id, int x1, int y1, int x2, int y2) {
    t_window *win_id = (t_window*) line_id;
    int x, y;
    unsigned int width, height;


    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Set xy upper left, positive width and height */
    if (x1 <= x2) {
        x = x1;
        width = x2 - x1;
    } else {
        x = x2;
        width = x1 - x2;
    }
    if (y1 <= y2) {
        y = y1;
        height = y2 - y1;
    } else {
        y = y2;
        height = y1 - y2;
    }

    /* Draw */
    XDrawRectangle (win_id->server->x_server,
      win_id->x_window,
      win_id->x_graphic_context, x, y, width, height);

    return (OK);

}

/* Fill a rectangle at x1y1, x1y2, x2y2, x2y1 */
extern int x_fill_rectangle (void *line_id, int x1, int y1, int x2, int y2) {
    t_window *win_id = (t_window*) line_id;
    int x, y;
    unsigned int width, height;


    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Set xy upper left, positive width and height */
    if (x1 <= x2) {
        x = x1;
        width = x2 - x1;
    } else {
        x = x2;
        width = x1 - x2;
    }
    if (y1 <= y2) {
        y = y1;
        height = y2 - y1;
    } else {
        y = y2;
        height = y1 - y2;
    }

    /* Fill */
    XFillRectangle (win_id->server->x_server,
      win_id->x_window,
      win_id->x_graphic_context, x, y, width, height);

    return (OK);

}

/* Draw points in a rectangle, starting at x1, y1 and of width * height pixels */
/* The points array has to be width * height and contains a list of Zero (no put) */
/*  or not Zero (put) */
extern int x_draw_points (void *line_id, int x1, int y1, int width, int height,
                          unsigned char points[]) {
    t_window *win_id = (t_window*) line_id;
    int x, y;
    unsigned char *p;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Draw points if set in array */
    p = points;
    for (y = 0; y < height; y++) {
      for (x = 0; x < width; x++) {
        if (*p != (unsigned char) 0) {
          XDrawPoint (win_id->server->x_server, win_id->x_window,
                      win_id->x_graphic_context, x1 + x, y1 + y);
        }
        p++;
      }
    }
    return (OK);
}


/* Fill a convex area defined by points */
extern int x_fill_area (void *line_id, int xys[], int nb_points) {
    t_window *win_id = (t_window*) line_id;
    XPoint *p_points;
    int i;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Check and copy points */
    if (nb_points <= 2) {
      return (ERR);
    }

    p_points = malloc (nb_points * sizeof(XPoint));
    if (p_points == NULL) {
#ifdef DEBUG
        printf ("X_EXPORT : Can't alloc memory for points to fill.\n");
#endif
        return (ERR);
    }

    for (i = 0; i < nb_points; i++) {
      p_points[i].x = xys[i * 2];
      p_points[i].y = xys[i * 2 + 1];
    }
    /* Fill convex area and free */
    XFillPolygon (win_id->server->x_server, win_id->x_window,
                  win_id->x_graphic_context, p_points, nb_points,
                  Complex, CoordModeOrigin);
    free (p_points);
    return (OK);
}

static void grab_pointer (Window window, Cursor cursor) {
    XGrabPointer(local_server.x_server, window, TRUE, 0, GrabModeAsync,
                 GrabModeAsync, window, cursor, CurrentTime);

}

extern int x_set_graphic_pointer (void *line_id, boolean graphic, boolean grab) {
    t_window *win_id = (t_window*) line_id;
    Cursor cursor;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }
    if (graphic) {
      cursor = XCreateFontCursor(local_server.x_server, XC_tcross);
      XDefineCursor(local_server.x_server, win_id->x_window, cursor);
    } else {
      cursor = XCreateFontCursor(local_server.x_server, XC_arrow);
      XUndefineCursor(local_server.x_server, win_id->x_window);
    }

    if (grab) {
      grab_pointer(win_id->x_window, cursor);
    } else {
      XUngrabPointer (local_server.x_server, CurrentTime);
    }
    return (OK);
}


extern int x_hide_graphic_pointer (void *line_id, boolean grab) {
    t_window *win_id = (t_window*) line_id;
    Pixmap blank;
    XColor dummy;
    char data[1] = {0};
    Cursor cursor;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Make a blank cursor from blank pixmap*/
    blank = XCreateBitmapFromData (local_server.x_server, win_id->x_window,
                                   data, 1, 1);
    if (blank == None) {
#ifdef DEBUG
        printf ("X_EXPORT : Can't create blank cursor.\n");
#endif
      return (ERR);
    }
    cursor = XCreatePixmapCursor(local_server.x_server, blank, blank,
                                 &dummy, &dummy, 0, 0);
    /* Assign */
    XDefineCursor(local_server.x_server, win_id->x_window, cursor);
    XFreePixmap (local_server.x_server, blank);

    /* Grab */
    if (grab) {
      grab_pointer(win_id->x_window, cursor);
    } else {
      XUngrabPointer (local_server.x_server, CurrentTime);
    }
    return (OK);
}


/***** Event management *****/
/* Previous event stored if arrived just after an expose */
static XEvent prev_event;
static boolean prev_event_set = False;

/* To wait a bit after first expose */
static void delay_ms (unsigned int msecs) {
  struct timeval timeout;

  timeout.tv_sec = 0;
  timeout.tv_usec = msecs * 1000;

  (void) select (0, NULL, NULL, NULL, &timeout);
}


/* Process the next X event */
/* p_line_id is the line on which the event has occured */
/* p_kind is 1 if the event is a key hit, 0 if it's a TID */
/* p_kind is -1 if the event is discarted or if error */
/* p_next is True if another event is available */
extern int x_process_event (void **p_line_id, int *p_kind, boolean *p_next) {

    t_window *win_id;
    XEvent event;
    XSelectionEvent reply;
    int n_events;
    int result;
    char *str;
    int i;
    boolean found;

  if (local_server.x_server == NULL) return (ERR);

  /* Loop from which exit is done when */
  /*  - no more event */
  /*  - Event OK */
  result = ERR;
  while (result == ERR) {

    if ( (next_blink.tv_sec != 0) || (next_blink.tv_usec != 0) ) {
      if (time_is_reached (&next_blink) ) {
        x_do_blinking();
      }
    }

    if (prev_event_set) {
      /* An event has already been got and stored (for skipping multi expose) */
      memcpy (&event, &prev_event, sizeof(XEvent));
      prev_event_set = False;

    } else {
      /* Initial reading of the number of pending messages to avoid blocking */
      n_events = XPending (local_server.x_server);

      if (n_events == 0) {
        *p_kind = DISCARD;
        *p_next = False;
        result = OK;
        break;
      }
      /* Error ? Exit from loop */
      if (n_events < 0) {
        *p_kind = DISCARD;
        *p_next = False;
        result = ERR;
        break;
      }

      /* Some events are discarted */
      XNextEvent (local_server.x_server, &event);
    }

    switch (event.type) {
      case KeyPress :
        if (XFilterEvent(&event, None)) {
          /* Event is filtered (e.g. ^ then i) */
          break;
        }
        /* Find the window of event */
        win_id = lin_get_win (event.xany.window);
        if (win_id == NULL) {
          /* Window not found : Check next event */
          break;
        }
        /* Decode key */
        key_chain (win_id->xic,
                   &event.xkey, &win_id->control, &win_id->shift,
                   &win_id->code, win_id->key_buf, &win_id->nbre_key);
        if (win_id->nbre_key != 0) {
          /* Key is valid */
          *p_line_id = (void*)win_id;
          *p_kind = KEYBOARD;
          result = OK;
        }
      break;
      case ButtonPress :
      case ButtonRelease :
        /* Find the window of event */
        win_id = lin_get_win (event.xany.window);
        if (win_id == NULL) {
          break; /* Next Event */
        }
        /* Store button */
        if (event.xbutton.button == Button1) {
           if ((event.xbutton.state & ShiftMask) != 0) {
              win_id->button = 4;
           } else {
              win_id->button = 1;
           }
        } else if (event.xbutton.button == Button2) {
           win_id->button = 2;
        } else if (event.xbutton.button == Button3) {
           if ((event.xbutton.state & ShiftMask) != 0) {
              win_id->button = 5;
           } else {
              win_id->button = 3;
           }
        } else if (event.xbutton.button == Button4) {
           win_id->button = 4;
        } else if (event.xbutton.button == Button5) {
           win_id->button = 5;
        } else {
           break; /* Next Event */
        }
        /* Store position */
        win_id->tid_x = event.xbutton.x;
        win_id->tid_y = event.xbutton.y;

        *p_line_id = (void*) win_id;
        if (event.type == ButtonPress) {
          *p_kind = TID_PRESS;
        } else {
          *p_kind = TID_RELEASE;
        }
        result = OK;
      break;
      case MotionNotify :
        /* Find the window of event */
        win_id = lin_get_win (event.xany.window);
        if (win_id == NULL) {
          break; /* Next Event */
        }
        if (!(win_id->motion_enabled)) {
          break; /* Next Event */
        }
        win_id->button = 0;
        /* Store position */
        win_id->tid_x = event.xmotion.x;
        win_id->tid_y = event.xmotion.y;

        *p_line_id = (void*) win_id;
        *p_kind = TID_MOTION;
        result = OK;
      break;
      case Expose:
        /* Find the window of event */
        win_id = lin_get_win (event.xany.window);
        if (win_id == NULL) {
          break; /* Next Event */
        }
        /* Wait a bit for any other expose to arrive */
        delay_ms (DELAY_EXPOSE_MS);
        /* Look forward to next events to discard multi expose */
        /* Stop when not an expose event (and store for next call) or no more event */
        for (;;) {
          if (XPending (local_server.x_server) <= 0) {
            break;
          }
          XNextEvent (local_server.x_server, &event);
          if (event.type != Expose) {
             /* Save event for next call */
             memcpy (&prev_event, &event, sizeof(XEvent));
             prev_event_set = True;
             break;
          }
        }
        *p_line_id = (void*) win_id;
        *p_kind = REFRESH;
        result = OK;
      break;
      case EnterNotify:
        /* Find the window of event */
        win_id = lin_get_win (event.xany.window);
        if (win_id == NULL) {
          break; /* Next Event */
        }
        *p_line_id = (void*) win_id;
        *p_kind = REFRESH;
        result = OK;
      break;
      case ClientMessage:
        /* Find the window of event */
        win_id = lin_get_win (event.xany.window);
        if (win_id == NULL) {
          break; /* Next Event */
        }
        /* Check Message type */
        str = XGetAtomName (local_server.x_server, event.xclient.message_type);
        if ( ((Atom)event.xclient.message_type
                     == local_server.wm_protocols_code)
          && ((Atom)event.xclient.data.l[0]
                     == local_server.delete_code)) {
          *p_line_id = (void*) win_id;
          *p_kind = EXIT_REQ;
          result = OK;
        }
        XFree (str);
      break;
      case SelectionRequest:
        /* Find the window of event */
        win_id = lin_get_win (event.xany.window);
        if (win_id == NULL) {
          break; /* Next Event */
        }
        /* Check that the requested type (target) is one of the supported */
#ifdef DEBUG
        {
          char *str = XGetAtomName(local_server.x_server,
                                   event.xselectionrequest.target);
          printf ("X_PROCESS_EVENT : Selection request of target %s\n", str);
          XFree (str);
        }
#endif
        found = FALSE;
        for (i = 0; i < NB_SELECTION_TYPES; i++) {
          if (event.xselectionrequest.target ==
                     local_server.selection_types[i]) {
            found = TRUE;
            break;
          }
        }
        if (found && (win_id->selection != NULL) ) {
          char *str = malloc (strlen(win_id->selection)+1);
          if (str != NULL) {
            if (i == 0) {
              /* UTF8_STRING requested */
              strcpy (str, win_id->selection);
            } else {
              /* Non UTF8 string or text requested */
              utf82ascii (win_id->selection, str);
            }
            /* Store selection in property */
            (void) XChangeProperty (local_server.x_server,
                     event.xselectionrequest.requestor,
                     event.xselectionrequest.property,
                     event.xselectionrequest.target,
                     8, PropModeReplace,
                     (unsigned char *)str,
                     (int) strlen(str)+1);
            free (str);
          }
        } else {
          /* No selection available */
          found = FALSE;
        }

        /* Send reply event */
        reply.type = SelectionNotify;
        reply.send_event = True;
        reply.display = event.xselectionrequest.display;
        reply.requestor = event.xselectionrequest.requestor;
        reply.selection = event.xselectionrequest.selection;
        reply.target = event.xselectionrequest.target;
        reply.property = (found ? event.xselectionrequest.property : None);
        reply.property = event.xselectionrequest.property;
        reply.time = CurrentTime;
        (void) XSendEvent(local_server.x_server,
              event.xselectionrequest.requestor,
              False, 0L, (XEvent*) &reply);
        /* Next event */
      break;
      case SelectionClear:
        /* Find the window of event */
        win_id = lin_get_win (event.xany.window);
        if (win_id == NULL) {
          break; /* Next Event */
        }
        if (win_id->nbre_drop_clear == 0) {
          /* This is not the consequence of ourself clearing the selection */
          if (win_id->selection != NULL) {
            /* Clear out selection buffer */
            free (win_id->selection);
            win_id->selection = NULL;
          }
        } else {
          /* This is the consequence of ourself clearing the selection */
          /* But perhaps the selection has be re-set since, so no clear */
          (win_id->nbre_drop_clear)--;
        }
      break;
      case SelectionNotify:
        /* Find the window of event */
        win_id = lin_get_win (event.xany.window);
        if (win_id == NULL) {
          break; /* Next Event */
        }
        if (event.xselection.property == None) {
          /* Selection transfer failed  for this target type */
          (win_id->select_index)++;
          if (win_id->select_index < NB_SELECTION_TYPES) {
            /* Try next target ype */
            x_request_in_selection (win_id);
          } else {
            /* No more supported target types => failed */
            win_id->select_index = SELEC_NONE;
            *p_line_id = (void*) win_id;
            *p_kind = SELECTION;
            result = OK;
          }
        } else {
          /* Success */
          (win_id->select_index) += SELEC_STORED;
          *p_line_id = (void*) win_id;
          *p_kind = SELECTION;
          result = OK;
        }
      break;
      default :
        /* Other events discarded */
      break;
    } /* Switch */
  } /* while */

  /* Re read number of pending messages */
  /*  (XNextEvent may flush the buffer) */
  n_events = XPending (local_server.x_server);
  *p_next = (n_events > 0);

  return (result);
}

/* Reads the position on TID */
/* The line_id must be the one given by wait_event */
/* p_button is set to the button 1, 2 or 3 */
/* p_row and p_column are the position of the "finger" on the TID */
/* If row_col is FALSE, p_row is y and p_col is x */
extern int x_read_tid (void *line_id, boolean row_col,
                int *p_button, int *p_row, int *p_column) {

    t_window *win_id = (t_window*) line_id;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }


    /* Return coordinates and button */
    if (row_col) {
        *p_row = (win_id->tid_y /
             fon_get_height (win_id->server->x_font[win_id->no_font])) + 1;
        *p_column = (win_id->tid_x /
             fon_get_width  (win_id->server->x_font[win_id->no_font])) + 1;
    } else {
        *p_row = win_id->tid_x;
        *p_column = win_id->tid_y;
    }
    *p_button = win_id->button;

    return (OK);
}


/* Reads the sequence of codes of a key (4 codes maxi) */
/* The line_id must be the one given by wait_event */
/* p_key is the the address of a table where to put the codes */
/* p_nbre if for the number of codes for the key */
extern int x_read_key (void *line_id, boolean *p_control, boolean *p_shift,
                       boolean *p_code, int *p_key, int *p_nbre) {

    t_window *win_id = (t_window*) line_id;
    int i;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Check that there is a key to read */
    if (win_id->nbre_key == 0) {
        return (ERR);
    }

    /* Read the key */
    *p_control = win_id->control;
    *p_shift = win_id->shift;
    *p_code = win_id->code;
    *p_nbre = win_id->nbre_key;
    for (i = 0; i < win_id->nbre_key; i++) {
        *p_key = win_id->key_buf[i];
        p_key++;
    }

    /* All is read now */
    win_id->nbre_key = 0;

    return (OK);
}

extern int x_enable_motion_events (void *line_id, boolean enable_motion) {
    t_window *win_id = (t_window*) line_id;
    unsigned long valuemask;
    XSetWindowAttributes win_attrib;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }
    /* Set new attributes */
    valuemask = CWEventMask;
    win_attrib.event_mask = EVENT_MASK;

    if (enable_motion) {
      win_attrib.event_mask |= PointerMotionMask;
    }
    XChangeWindowAttributes(win_id->server->x_server, win_id->x_window,
      valuemask, &win_attrib);
    XChangeActivePointerGrab(win_id->server->x_server,
       PointerMotionMask | ButtonReleaseMask | ButtonPressMask,
       None, CurrentTime);
    XFlush(win_id->server->x_server);
    win_id->motion_enabled = enable_motion;
    return (OK);
}

/* Reads the current position on TID in pixels */
/* The line_id must be the one given by wait_event */
/* p_x and p_y are the position of the "finger" on the TID */
extern int x_get_pointer_pos (void *line_id, int *p_x, int *p_y) {

    t_window *win_id = (t_window*) line_id;
    Bool result;
    Window root, win;
    int root_x, root_y;
    unsigned int mask;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    result = XQueryPointer (win_id->server->x_server, win_id->x_window,
       &root, &win, &root_x, &root_y, p_x, p_y, &mask);

    /* Pointer not in same screen than window */
    if (result == False) {
        *p_x = -1;
        *p_y = -1;
    }

    return (OK);
}

/* Propose selection to other applis (cancel if selection if NULL) */
extern int x_set_selection (void *line_id, const char *selection) {
    t_window *win_id = (t_window*) line_id;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Clear selection buffer */
    if (win_id->selection != NULL) {
        free (win_id->selection);
        win_id->selection = NULL;

    }

    if (selection == NULL) {
        if (XGetSelectionOwner(win_id->server->x_server, XA_PRIMARY)
                                == win_id->x_window) {
            /* Cancel our role of selection owner */
            /* We will receive a SelectionClear event that must be dropped */
            /*  (must lead to clear the selection data */
            XSetSelectionOwner (win_id->server->x_server, XA_PRIMARY,
                                None, CurrentTime);
            (win_id->nbre_drop_clear)++;
         }
         return OK;
     }
     /* Set owner of selection */
     XSetSelectionOwner (win_id->server->x_server, XA_PRIMARY,
                         win_id->x_window, CurrentTime);

     if (XGetSelectionOwner(win_id->server->x_server, XA_PRIMARY)
                            != win_id->x_window) {
         /* We didn't get the selection ownership */
         return (ERR);
     }
     /* Store new selection */
     win_id->selection = malloc (strlen(selection) + 1);
     strcpy (win_id->selection, selection);
     return (OK);
}


/* Request a SELECTION event */
extern int x_request_selection (void *line_id) {
    t_window *win_id = (t_window*) line_id;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Clean previous selection if any */
    x_clear_in_selection (win_id);

    /* Request with first possible target type */
    win_id->select_index = 0;
    x_request_in_selection (win_id);
    return (OK);
}

/* Read and clean selection associated to SELECTION event */
/* Set len to 0 to clear */
extern int x_get_selection (void *line_id, char *p_selection, int len) {
    t_window *win_id = (t_window*) line_id;
    int res;
    Atom type_return;
    int format_return;
    unsigned long nitems_return, offset_return;
    char *data;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }
    if (p_selection == NULL) return (ERR);
    if (win_id->select_index < SELEC_STORED) return (ERR);

    /* Get the selection (delete it) */
    res = XGetWindowProperty (win_id->server->x_server, win_id->x_window,
            win_id->server->select_code,
            0L, (long)len,
            True, (win_id->select_index) % SELEC_STORED,
            &type_return, &format_return, &nitems_return, &offset_return,
            (unsigned char **)&data);
    win_id->select_index = SELEC_NONE;
    if (res != Success) {
      x_clear_in_selection (win_id);
      return ERR;
    }

    /* Trunc to len characters (including '\0') */
    if (len > 0) {
      if ((unsigned)nitems_return > (unsigned)len - 1) {
        strncpy (p_selection, data, len - 1);
        p_selection[len - 1] = '\0';
      } else if (nitems_return == 0) {
        /* Data is NULL */
        p_selection[0] = '\0';
      } else if (data[(unsigned)nitems_return] != '\0') {
        strncpy (p_selection, data, (unsigned)nitems_return - 1);
        p_selection[(unsigned)nitems_return] = '\0';
      } else {
        strcpy (p_selection, data);
      }
    }

    /* Clear */
    if (data != NULL) XFree (data);
    return (OK);
}

/* Special Blink primitive must be called twice a second to generate */
/* blink if blinking is stopped */
extern int x_blink(void) {

    /* Check that the server is initialised */
    if (local_server.x_server == NULL) return (OK);

    /* Check task is not active */
    if (curr_percent != 0) {
      return (ERR);
    }

    x_do_blink ();

    return (OK);
}

/* The real blinking primitive */
static void x_do_blink (void) {

    static boolean blinking;

    /* Check that the server is initialised */
    if (local_server.x_server == NULL) return;

    /* Here we know if it is colors for blink or standard colors */
    blinking = ! blinking;

    lin_blink_colors(blinking);
    XFlush (local_server.x_server);

    return;
}

/* Rings a bell on the display */
extern int x_bell (int nbre_bell) {

    XKeyboardControl keyboard_state;
    int i;

    if (local_server.x_server == NULL) return (ERR);

    /* Volume maxi, 400 Hz, and 100 ms */
    keyboard_state.bell_percent = 100;
    keyboard_state.bell_pitch = 400;
    keyboard_state.bell_duration = 100;
    XChangeKeyboardControl (local_server.x_server,
      KBBellPercent | KBBellPitch | KBBellDuration, &keyboard_state);

    /* Set at maximum number of bells if more that maxi is requested */
    if (nbre_bell > NBRE_MAX_BELL) nbre_bell = NBRE_MAX_BELL;

    for (i=1; i<=nbre_bell; i++) {
        /* Bell at 100% of bell_percent (in keyboard_state) */
        XBell (local_server.x_server, 100);
    }

    return (OK);

}

