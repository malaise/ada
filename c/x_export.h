#ifndef _X_EXPORT_H
#define _X_EXPORT_H

#include <X11/Xlib.h>
#include "boolean.h"
#include "timeval.h"
#include "wait_evt.h"

/* OK/ERR status are those of wait_evt */

/* New kind of event (fd) added to the list */
/*  of wait_evt (NO_EVENT, SIG_EVENT, WAKE_EVENT) */
#define X_EVENT (-10)

/* The kinds of X events */
#define DISCARD     0
#define TID_RELEASE 1
#define TID_PRESS   2
#define KEYBOARD    3
#define REFRESH     4
#define TID_MOTION  5
#define EXIT_REQ    6
#define SELECTION   7
#define TID_ENTER   8
#define TID_LEAVE   9

/* The shapes of pointers */
#define POINTER_NONE   0
#define POINTER_ARROW  1
#define POINTER_CROSS  2
#define POINTER_HAND   3
#define POINTER_TARGET 4
#define NB_POINTERS   5

/* Basics */

extern int x_initialise (const char *server_name,
                         const char *color_names[]);

extern int x_suspend (void);
extern int x_resume (void);
extern int x_modified (void);

extern int x_get_font_geometry (int font_no, boolean bold,
                        int *p_f_width, int *p_f_height, int *p_f_offset);

extern int x_open_line (int screen_id,
                        int row, int column,
                        int height, int width,
                        int background, int border,
                        int no_font,
                        void **p_line_id);

extern int x_close_line (void *line_id);

extern int x_set_name (void *line_id, const char *name);

extern int x_set_icon (void *line_id, const char **pixmap);

extern int x_flush (void);

extern int x_clear_line (void *line_id);

/* Char display */

extern int x_set_attributes (void *line_id,
                             int paper, int ink,
                             boolean bold, boolean underline,
                             boolean reverse);

extern int x_set_xor_mode (void *line_id, boolean xor_mode);

extern int x_put_char (void *line_id, int car, int row, int column);

extern int x_overwrite_char (void *line_id, int car, int row, int column);


extern int x_put_string (void *line_id,
                         const char *p_char, int number,
                         int row, int column);

extern int x_put_char_attributes (void *line_id,
                                  int car,
                                  int row, int column,
                                  int paper, int ink,
                                  boolean bold, boolean underline,
                                  boolean reverse);

extern int x_draw_area (void *line_id,
                        int width, int height,
                        int row, int column);

/* Graphics */

extern int x_get_graph_charact (void *line_id, int *p_w_width, int *p_w_height,
                      int *p_f_width, int *p_f_height, int *p_f_offset);

extern int x_get_font_name (void *line_id, char *font_name, int font_len);
extern int x_get_bold_name (void *line_id, char *font_name, int font_len);

extern int x_put_char_pixels (void *line_id, int car, int x, int y);

extern int x_put_string_pixels (void *line_id, const char *p_char, int number,
                                int x, int y);

extern int x_draw_point (void *line_id, int x, int y);

extern int x_draw_line (void *line_id, int x1, int y1, int x2, int y2);

extern int x_draw_rectangle (void *line_id, int x1, int y1, int x2, int y2);

extern int x_fill_rectangle (void *line_id, int x1, int y1, int x2, int y2);

/* Angles in minutes of degree, trigo */
extern int x_draw_arc (void *line_id, int x1, int y1, int x2, int y2,
                       int a1, int a2);

extern int x_fill_arc (void *line_id, int x1, int y1, int x2, int y2,
                       int e1, int a2);

extern int x_draw_points (void *line_id, int x1, int y1, int width, int height,
                          unsigned char points[]);

extern int x_fill_area (void *line_id, int xys[], int nb_points);

extern int x_get_pointer_pos (void *line_id, int *p_x, int *p_y);

extern int x_set_pointer (void *line_id, int shape);

extern int x_grab_pointer (void *line_id, boolean grab);

/* Events */
extern int x_select (int *p_fd, boolean *p_read, timeout_t *timeout);

extern int x_process_event (void **p_line_id, void **p_ref,
                            int *p_kind, boolean *p_next);

extern int x_read_tid (void *line_id, boolean row_col,
                       int *p_button, int *p_row, int *p_column,
                       int *p_sub_row, int *p_sub_column);

extern int x_read_key (void *line_id, boolean *p_control, boolean *p_shift,
                       boolean *p_code, int *p_keys, int *p_nbre);

extern int x_enable_motion_events (void *line_id, boolean enable_motion);

/* Selection exchange */
extern int x_set_selection (void *line_id, const char *selection);

extern int x_request_selection (void *line_id);

extern int x_get_selection (void *line_id, char *p_selection, int len);

/* Bell */

extern int x_bell (int nbre_bell);

#endif /* _X_EXPORT_H */

