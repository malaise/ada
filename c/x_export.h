#ifndef _X_EXPORT_H
#define _X_EXPORT_H

#include <X11/Xlib.h>
#include "boolean.h"

/* The result of each call is 0 if success, -1 if error */
#define OK        0
#define ERR      -1

/* The 5 kinds of events */
#define TID_MOTION  5
#define REFRESH     4
#define KEYBOARD    3
#define TID_PRESS   2
#define TID_RELEASE 1
#define DISCARD     0

/* Basics */

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

/* Char display */

extern int x_set_attributes (void *line_id,
                             int paper, int ink,
                             boolean superbright, boolean underline,
                             boolean blink, boolean reverse);

extern int x_set_xor_mode (void *line_id, boolean xor_mode);

extern int x_put_char (void *line_id, int car, int row, int column);

extern int x_overwrite_char (void *line_id, int car, int row, int column);


extern int x_put_string (void *line_id,
                         char *p_char, int number,
                         int row, int column);

extern int x_put_char_attributes (void *line_id,
                                  int car,
                                  int row, int column,
                                  int paper, int ink,
                                  boolean superbright, boolean underline,
                                  boolean blink, boolean reverse);

extern int x_draw_area (void *line_id,
                        int width, int height,
                        int row, int column);

/* Graphics */

extern int x_get_graph_charact (void *line_id, int *p_w_width, int *p_w_height,
                      int *p_f_width, int *p_f_height, int *p_f_offset);

extern int x_put_char_pixels (void *line_id, int car, int x, int y);

extern int x_draw_point (void *line_id, int x, int y);

extern int x_draw_line (void *line_id, int x1, int y1, int x2, int y2);

extern int x_draw_rectangle (void *line_id, int x1, int y1, int x2, int y2);

extern int x_get_pointer_pos (void *line_id, int *p_x, int *p_y);

extern int x_set_graphic_pointer (void *line_id, boolean graphic);

/* Events */

extern int x_select (fd_set *p_mask, boolean *p_x_event, int *timeout_ms);

extern int x_process_event (void **p_line_id, int *p_kind, boolean *p_next);

extern int x_read_tid (void *line_id, boolean row_col,
                       int *p_button, int *p_row, int *p_column);

extern int x_read_key (void *line_id, int *p_key, int *p_nbre);

extern int x_enable_motion_events (void *line_id, boolean enable_motion);

/* Blink, bell */

extern int x_blink (void);

extern int x_stop_blinking (void);

extern int x_start_blinking (void);

extern int x_bell (int nbre_bell);


#endif /* _X_EXPORT_H */
