#ifndef _WAIT_EVT_H
#define _WAIT_EVT_H

#include "timeval.h"

/* The result of each call is 0 if success, -1 if error */
#define OK        0
#define ERR      -1

extern int evt_add_fd (int fd, boolean read);
extern int evt_del_fd (int fd, boolean read);
extern boolean evt_fd_set (int fd, boolean read);

extern void send_signal (int sig);

#define SIG_UNKNOWN  -2
#define SIG_NONE     -1
#define SIG_DUMMY     0
#define SIG_TERMINATE 1
#define SIG_CHILD     2
extern int get_signal (void);

extern void evt_wake_up (void);

#define WAKE_EVENT (-3)
#define SIG_EVENT  (-2)
#define NO_EVENT   (-1)
extern int evt_wait (int *p_fd, boolean *p_read, int *timeout_ms);

extern void evt_time_remaining (int *timeout_ms, timeout_t *exp_time);
#endif

