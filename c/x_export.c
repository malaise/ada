#include <sys/types.h>
#include <sys/time.h>
#include <sys/select.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <X11/cursorfont.h>

#include "x_export.h"
#include "x_screen.h"
#include "x_line.h"
#include "timeval.h"


/* The maximum of sucessive bells */
#define NBRE_MAX_BELL 5

/* The delay in milli seconds after receiving 1st expose */
/* before looking for any further expose (and filter) */
#define DELAY_EXPOSE_MS 25

void print_date (void) {
  timeout_t cur_time;
  get_time (&cur_time);
  printf ("    >> %06d %06d << ", (int)cur_time.tv_sec,
                                  (int)cur_time.tv_usec);
}

/***** Blinking management *****/
/* The percentage of blinking when colors are seeable */
#define PERCENT_ON  75
#define PERCENT_OFF (100 - PERCENT_ON)
int curr_percent = 0; 

timeout_t next_blink;

/* The real blinking routine */
void x_do_blink (void);

/* The blinking task */
void x_do_blinking (void) {

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
  (void) incr_time (&next_blink, curr_percent * 10);
}

int x_stop_blinking (void) {
  
  /* Set to the non_blink state */
  if (curr_percent == PERCENT_OFF) {
    x_do_blink();
  }
  curr_percent = 0;
  return (OK);
}

int x_start_blinking (void) {
  curr_percent = PERCENT_ON;
  get_time (&next_blink);
  (void) incr_time (&next_blink, curr_percent * 10);
  return (OK);
}

/***** Fds Management   *****/
static fd_set global_read_mask;
static fd_set global_write_mask;

static int last_fd = -1;

int x_add_fd (int fd, boolean read) {
  if (last_fd == -1) {
    FD_ZERO(&global_read_mask);
    FD_ZERO(&global_read_mask);
  }
  if (read) {
    FD_SET(fd, &global_read_mask);
  } else {
    FD_SET(fd, &global_write_mask);
  }
  if (fd > last_fd) {
    last_fd = fd;
  }
  return (OK);
}

int x_del_fd (int fd, boolean read) {
  int i;
  fd_set *mask;

  if (last_fd == -1) {
    return (ERR);
  }

  if (read) {
    mask = &global_read_mask;
  } else {
    mask = &global_write_mask;
  }

  if (!FD_ISSET(fd, mask)) {
    return (ERR);
  }
  FD_CLR (fd, mask);

  for (i = NFDBITS - 1; i >= 0; i--) {
    if ( FD_ISSET(i, &global_read_mask)
      || FD_ISSET(i, &global_write_mask) ) {
      last_fd = i;
      return (OK);
    }
  }
  last_fd = 0;
  return (OK);
}

extern boolean x_fd_set (int fd, boolean read) {
  if (last_fd == -1) {
    return (FALSE);
  }
  if (read) {
    return (FD_ISSET(fd, &global_read_mask));
  } else {
    return (FD_ISSET(fd, &global_write_mask));
  }
}


/***** Event management *****/

/* Makes a select between the sockets described in the global mask AND the  */
/*  the socket used by X for the events                                     */
/* The time out is in miliseconds (negative for blocking )                  */
/* If a X event is available, *p_fd is set to X_EVENT (-1) and read to true */
/* Else *p_fd is set to one valid fd on which there is an event and         */
/*  read is set accordingly                                                 */
/* Else *p_fd is set to NO_EVENT (-2) and read is meaningless               */
/* Failure if select fails                                                  */

extern int x_select (int *p_fd, boolean *p_read, int *timeout_ms) {
  fd_set select_read_mask, select_write_mask;
  int last_select_fd;
  int x_soc;
  int i, n;
  timeout_t cur_time, exp_time, exp_select, timeout, *timeout_ptr;
  boolean timeout_is_active, blink_is_active;


  if (p_fd == (int*) NULL) {
    return (ERR);
  }

  /* Compute exp_time = cur_time + timeout_ms */
  blink_is_active = (curr_percent != 0);
  timeout_is_active = *timeout_ms >= 0;
  if (timeout_is_active) {
    get_time (&exp_time);
    incr_time (&exp_time, *timeout_ms);
  }
  if (blink_is_active || timeout_is_active) {
    timeout_ptr = &timeout;
  } else {
    timeout_ptr = NULL;
  }
  

  /* Check X socket */
  if (local_server.x_server == NULL) {
    x_soc = -1;
  } else {
    x_soc = ConnectionNumber (local_server.x_server);
    XFlush (local_server.x_server);
  }

  for (;;) {
    /* Compute exp_select : smallest of exp_time(timeout) and next_blink */
    if (blink_is_active) {
      if (timeout_is_active) {
        if (comp_time (&exp_time, &next_blink) >= 0) {
          exp_select = next_blink;
        } else {
          exp_select = exp_time;
        }
      } else {
        exp_select = next_blink;
      }
    } else {
      if (timeout_is_active) {
        exp_select = exp_time;
      }
    }

    timeout = exp_select;
    get_time (&cur_time);
    if ( (timeout_ptr != NULL) && (sub_time (timeout_ptr, &cur_time) < 0) ) {
      /* Select timeout is reached */
      if ( (blink_is_active) && time_is_reached (&next_blink) ) {
        /* Blink and continue */
        x_do_blinking();
      }
      if ( (timeout_is_active) && time_is_reached (&exp_time) ) {
        /* Done on timeout */
        *p_fd = NO_EVENT;
        *timeout_ms = 0;
        return (OK);
      }
    } else {
      /* Compute mask for select */
      bcopy ((char*)&global_read_mask, (char*)&select_read_mask,
             sizeof(fd_set));
      bcopy ((char*)&global_write_mask, (char*)&select_write_mask,
             sizeof(fd_set));
      last_select_fd = last_fd;
      if (x_soc != -1) {
        FD_SET(x_soc, &select_read_mask);
        if (x_soc > last_select_fd) {
          last_select_fd = x_soc;
        }
      }
      /* Default result */
      *p_fd = NO_EVENT;
      *p_read = TRUE;

      n = select (last_select_fd + 1, &select_read_mask,
                                      &select_write_mask, NULL, timeout_ptr);

      if (n > 0) {
        /* An Event : Separate X events from others */
        if ( (x_soc != -1) && (FD_ISSET(x_soc, &select_read_mask)) ) {
          *p_fd = X_EVENT;
          *p_read = TRUE;
        } else {
          /* Check read events first */
          for (i = 0; i <= last_fd; i++) {
            if (FD_ISSET(i, &select_read_mask)) {
              *p_fd = i;
              *p_read = TRUE;
              break;
            }
          }
          if (*p_fd == NO_EVENT) {
            /* Check write events second */
            for (i = 0; i <= last_fd; i++) {
              if (FD_ISSET(i, &select_write_mask)) {
                *p_fd = i;
                *p_read = FALSE;
                break;
              }
            }
          }
        }
        if (*timeout_ms > 0) {
          get_time (&cur_time);
          if (sub_time (&exp_time, &cur_time) >= 0) {
            *timeout_ms = exp_time.tv_sec * 1000 + exp_time.tv_usec / 1000;
          } else {
            *timeout_ms = 0;
          }
        }
        return (OK);
      } else if (n < 0) {
        if (errno != EINTR) {
          /* Real error */
#ifdef DEBUG
            perror ("select");
#endif
          return (ERR);
        }
      }

    }
  }
}

/***** Line management *****/

/* Initialise communication with the 'local' X server */
/*  opening a little window */
/* Hangs handler for blinking */
int x_initialise (char *server_name) {

    int result;

    result = (lin_initialise (server_name) ? OK : ERR);
    if (!blink_bold()) {
      (void) x_start_blinking();
    }
    return (result);
}


/* Opens a line */
int x_open_line (int screen_id, int row, int column, int height, int width, 
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
int x_close_line (void *line_id) {
    int result;

    result = (lin_close( (t_window*) line_id) ? OK : ERR);

    return (result);
}

/* Set line name */
int x_set_line_name (void *line_id, char *line_name) {
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
int x_flush (void) {

    /* Check that display is init */
    if (local_server.x_server == NULL) {
        return (ERR);
    }

    /* Flush the outputs */
    XFlush (local_server.x_server); 

    return (OK);
}

/* Clears a line */
int x_clear_line (void *line_id) {
    int result;


    result = (lin_clear( (t_window*) line_id) ? OK : ERR);

    return(result);
}

/***** Put and attributes management *****/

/* Sets the attributes for a further put in the same window */
int x_set_attributes (void *line_id, int paper, int ink,
  boolean superbright, boolean underline, boolean blink, boolean reverse) {

    t_window *win_id = (t_window*) line_id;

    /* Check that window is open */
        if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Checks that the colors are valid */
    if ( (! col_check (paper)) || (! col_check (ink)) ) {
        return (ERR);
    }

    /* Store underline attribute */
    win_id->underline = underline;

    /* Update graphic context */
    scr_set_attrib (win_id->server->x_server, 
      win_id->x_graphic_context, win_id->server->x_font,
      win_id->no_font, win_id->screen->color_id,
      paper, ink, superbright, blink, reverse);

    return (OK);

}

/* Set further put on window in Xor or back to Copy mode */
int x_set_xor_mode (void *line_id, boolean xor_mode) {
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
int x_put_char (void *line_id, int car, int row, int column) {

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
      win_id->x_window, x, y, (char)car, win_id->xor_mode);

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
int x_overwrite_char (void *line_id, int car, int row, int column) {

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
      win_id->x_window, x, y, (char)car);

    return (OK);
}


/* Writes a string whith the attributes previously set */
/* The line_id is the token, previously given by open_line */
/* The str is the adress of the first character to write */
/* The length is the number of characters to write */
/* The output is not flushed */
int x_put_string (void *line_id, char *p_char, int number,
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
      win_id->x_graphic_context,
      win_id->x_window, x, y, p_char, number, win_id->xor_mode);

    /* Underline */
    if (win_id->underline) {
        scr_underline_string (win_id->server->x_server,
          win_id->x_graphic_context, win_id->x_window, x, y, number);
    }

    return (OK);
}

/* Writes a char on a line with specified characteristics */
/* The output is not flushed */
int x_put_char_attributes (void *line_id, int car, int row, int column,
  int paper, int ink,
  boolean superbright, boolean underline, boolean blink, boolean reverse) {

    if (x_set_attributes (line_id, paper, ink, 
      superbright, underline, blink, reverse) == ERR) {
        return (ERR);
    }

    return (x_put_char (line_id, car, row, column));
}

int x_draw_area (void *line_id, int width, int height, int row, int column) {

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
int x_get_graph_charact (void *line_id, int *p_w_width, int *p_w_height,
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
int x_put_char_pixels (void *line_id, int car, int x, int y) {

    t_window *win_id = (t_window*) line_id;

    /* Check that window is open */
    if (! lin_check(win_id)) {
        return (ERR);
    }

    /* Put char */
    scr_put_char (win_id->server->x_server, 
      win_id->x_graphic_context, 
      win_id->x_window, x, y, (char)car, win_id->xor_mode);

    /* Underline */
    if (win_id->underline) {
      scr_underline_char (win_id->server->x_server,
      win_id->x_graphic_context, 
      win_id->x_window, x, y);
    }

    return (OK);
}

/* Draw a point at x,y */
int x_draw_point (void *line_id, int x, int y) {
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
int x_draw_line (void *line_id, int x1, int y1, int x2, int y2) {
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
int x_draw_rectangle (void *line_id, int x1, int y1, int x2, int y2) {
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
int x_fill_rectangle (void *line_id, int x1, int y1, int x2, int y2) {
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


int x_set_graphic_pointer (void *line_id, boolean graphic) {
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
      XUndefineCursor(local_server.x_server, win_id->x_window);
    }
    return (OK);
}

/***** Event management *****/
/* Previous event stored if arrived just after an expose */
static XEvent prev_event;
static boolean prev_event_set = False;

/* To wait a bit after first expose */
void delay_ms (unsigned int msecs) {
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
int x_process_event (void **p_line_id, int *p_kind, boolean *p_next) {

    t_window *win_id;
    XEvent event;
    int n_events;
    int result;

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
        /* Find the window of event */
        win_id = lin_get_win (event.xany.window);
        if (win_id == NULL) {
          /* Window not found : Check next event */
          break;
        }
        if (key_chain (&event.xkey, win_id->key_buf, 
         &win_id->nbre_key)) {
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
           win_id->button = 1;
        } else if (event.xbutton.button == Button2) {
           win_id->button = 2;
        } else if (event.xbutton.button == Button3) {
           win_id->button = 3;
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
int x_read_tid (void *line_id, boolean row_col, 
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
int x_read_key (void *line_id, int *p_key, int *p_nbre) {

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


/* Special Blink primitive must be called twice a second to generate */
/* blink if blinking is stopped */
int x_blink(void) {

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
void x_do_blink (void) {

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
int x_bell (int nbre_bell) {

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

