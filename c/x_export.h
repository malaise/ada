#ifndef _X_EXPORT_H
#define _X_EXPORT_H

#include <X11/Xlib.h>
#include "boolean.h"

/* The result of each call is 0 if success, -1 if error */
#define OK        0
#define ERR      -1

/* The 4 kinds of events */
#define REFRESH     4
#define KEYBOARD    3
#define TID_PRESS   2
#define TID_RELEASE 1
#define DISCARD     0

extern int x_initialise (char *server_name);

extern int x_open_line (int screen_id,
                        int row, int column,
                        int height, int width,
                        int background, int border,
                        int no_font,
                        void **p_line_id);

extern int x_close_line (void *line_id);

extern int x_set_line_name (void *line_id, char *line_name);

extern int x_flush (void);

extern int x_clear_line (void *line_id);

extern int x_move (void *line_id,
                   int row, int column);

extern int x_set_attributes (void *line_id,
                             int paper, int ink,
                             boolean superbright, boolean underline,
                             boolean blink, boolean reverse);

extern int x_put_char (void *line_id, int car);

extern int x_overwrite_char (void *line_id, int car);

extern int x_put_string (void *line_id, char *p_char, int number);

extern int x_put_char_pixels (void *line_id, int car, int x, int y);

extern int x_put_char_attributes (void *line_id,
                                  int car,
                                  int row, int column,
                                  int paper, int ink,
                                  boolean superbright, boolean underline,
                                  boolean blink, boolean reverse);

extern int x_draw_area (void *line_id, int width, int height);
  
extern int x_select (fd_set *p_mask, boolean *p_x_event, int *timeout_ms);

extern int x_process_event (void **p_line_id, int *p_kind, boolean *p_next);

extern int x_read_tid (void *line_id, int *p_button, int *p_row, int *p_column);

extern int x_read_key (void *line_id, int *p_key, int *p_nbre);

extern int x_blink (void);

extern int x_stop_blinking (void);


extern int x_bell (int nbre_bell);


#endif /* _X_EXPORT_H */
