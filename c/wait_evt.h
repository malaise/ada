#ifndef _WAIT_EVT_H
#define _WAIT_EVT_H

#include "timeval.h"

/* The result of each call except get_signal is 0 if success, -1 if error */
#define OK        0
#define ERR      -1

/* Add/Del fd to the WaitSet, either for read or write*/
extern int evt_add_fd (int fd, boolean read);
extern int evt_del_fd (int fd, boolean read);
/* Check if fd set (for read or write) */
extern boolean evt_fd_set (int fd, boolean read);

/* Send Dummy, Terminate or Child */
extern void send_signal (int sig);

/* Get signal received */
/* Unknown signal was received */
#define SIG_UNKNOWN  -2
/* No signal was received */
#define SIG_NONE     -1
/* Dummy signal has been received */
#define SIG_DUMMY     0
/* Terminate signal has been received */
#define SIG_TERMINATE 1
/* Child signal has been received */
#define SIG_CHILD     2
extern int get_signal (void);

/* Send WakeUp event */
extern void evt_wake_up (void);

/* Wait for an event (on a Fd, a Signal or WakeUp) */
/*  at most for a given delay */

/* *p_fd set to WAKE_EVENT to indicate a WakeUp event */
#define WAKE_EVENT (-3)
/* *p_fd set to SIG_EVENT to indicate a Signal event */
#define SIG_EVENT  (-2)
/* *p_fd set to NO_EVENT to indicate that the timeout is reached */
#define NO_EVENT   (-1)

/* *p_read is set to indicate if the fd is for read or write */
/*  always set to read if not a fd event */

/* Timeout is in out. Set it to the delay (0 for poll) */
/*  it will be set to the remaining time (0 if NO_EVENT) */
/*  Set it to a negative value for infinite wait */
extern int evt_wait (int *p_fd, boolean *p_read, int *timeout_ms);

/* Set in timeout_ms the remaining time between now and exp_time */
/* Result is always >= 0 */
extern void evt_time_remaining (int *timeout_ms, timeout_t *exp_time);
#endif

