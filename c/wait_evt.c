#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/select.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>

#include "wait_evt.h"

/***** Fds Management   *****/
static fd_set global_read_mask;
static fd_set global_write_mask;

/* Highest fd even set */
static int highest_fd = -1;
/* Highest fd set in mask */
static int last_fd = -1;
/* Fd previously returned */
static int prev_fd = -1;

int evt_add_fd (int fd, boolean read) {
  if (fd < 0) {
    return (WAIT_ERR);
  }
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
  if (last_fd > highest_fd) {
    highest_fd = last_fd;
  }
#ifdef DEBUG
  fprintf (stderr, "wait_evt.c: Added fd %d, last %d, highest %d\n",
                   fd, last_fd, highest_fd);
#endif
  return (WAIT_OK);
}

int evt_del_fd (int fd, boolean read) {
  int i;
  fd_set *mask;

  if (fd < 0) {
    return (WAIT_ERR);
  }
  if (last_fd == -1) {
    return (WAIT_ERR);
  }

  if (read) {
    mask = &global_read_mask;
  } else {
    mask = &global_write_mask;
  }

  if (!FD_ISSET(fd, mask)) {
    return (WAIT_ERR);
  }
  FD_CLR (fd, mask);

  last_fd = 0;
  for (i = highest_fd; i >= 0; i--) {
    if ( FD_ISSET(i, &global_read_mask)
      || FD_ISSET(i, &global_write_mask) ) {
      last_fd = i;
      break;
    }
  }
#ifdef DEBUG
  fprintf (stderr, "wait_evt.c: Deleted fd %d, last %d, highest %d\n",
                   fd, last_fd, highest_fd);
#endif
  return (WAIT_OK);
}

extern boolean evt_fd_set (int fd, boolean read) {
  if (fd < 0) {
    return (WAIT_ERR);
  }
  if (last_fd == -1) {
    return (FALSE);
  }
  if (read) {
    return (FD_ISSET(fd, &global_read_mask));
  } else {
    return (FD_ISSET(fd, &global_write_mask));
  }
}


/***** Sig Management *****/
#define SIGDUMMY SIGUSR1
static int map_signal (int sig_num) {
  if (sig_num == SIGINT) {
    return (SIG_TERMINATE);
  } else if (sig_num == SIGTERM) {
    return (SIG_TERMINATE);
  } else if (sig_num == SIGCHLD) {
    return (SIG_CHILD);
  } else if (sig_num == SIGDUMMY) {
    return (SIG_DUMMY);
  } else if (sig_num == SIG_NONE) {
    return (SIG_NONE);
  } else {
    return (SIG_UNKNOWN);
  }
}

static int signal_received[3];
static boolean sig_received = FALSE;
static void signal_handler (int sig) {
  int new_sig = map_signal (sig);
  /* Discard NONE or UNKNOWN */
  if (new_sig < SIG_DUMMY) return;
  /* Store signal received */
  signal_received[new_sig] = TRUE;
  sig_received = TRUE;
}

extern void send_dummy_signal (void) {
  /* Activate signal handling if necessary*/
  activate_signal_handling ();
  (void) kill (getpid(), SIGDUMMY);
}

/* Return highest prio received and reset it */
extern int get_signal (void) {
  int i, res;
  res = SIG_NONE;

  /* Sig_receive is re-evaluated set to TRUE if another signal is pending */
  sig_received = FALSE;
  for (i = SIG_TERMINATE; i >= SIG_DUMMY; i--) {
    if (signal_received[i]) {
      if (res == SIG_NONE) {
        res = i;
        signal_received[i] = FALSE;
      } else {
        /* Another (lower prio) signal is pending */
        sig_received = TRUE;
      }
    }
  }
  return res;
}

/* Check if a signal has been received */
static boolean check_signal (void) {
  return sig_received;
}

/* Reset */
static void reset_signal (void) {
  int i;
  for (i = SIG_TERMINATE; i >= SIG_DUMMY; i--) {
    signal_received[i] = FALSE;
  }
  sig_received = FALSE;
}

/***** WakeUp Management   *****/
static int wake_up_fds[2] = {-1, -1};
extern void evt_wake_up (void) {
  char c;
  int r;

  if (wake_up_fds[1] == -1) {
    return;
  } else {
    c = 'w';
    for (;;) {
      r = write (wake_up_fds[1], &c, 1);
      if ( (r == -1) && (errno != EINTR) ) {
#ifdef DEBUG
        perror("write");
#endif
        return;
      }
      if (r == 1) {
        return;
      }
    }
  }
}


/***** Select Management   *****/
/* Init signal handling and wake-up pipe */
static void init_sig_and_wake_up (void) {
  (void) signal(SIGPIPE, SIG_IGN);
  if (wake_up_fds[0] == -1) {
    (void) pipe (wake_up_fds);
    (void) fcntl (wake_up_fds[0], F_SETFD, FD_CLOEXEC);
    (void) fcntl (wake_up_fds[1], F_SETFD, FD_CLOEXEC);
    (void) evt_add_fd (wake_up_fds[0], TRUE);
  }
}

/* Activate signal handling and reporting */
static boolean sig_handled = FALSE;
extern void activate_signal_handling (void) {
  /* Set handler if not set */
  if (! sig_handled) {
    (void) signal(SIGINT, signal_handler);
    (void) signal(SIGTERM, signal_handler);
    (void) signal(SIGCHLD, signal_handler);
    (void) signal(SIGDUMMY, signal_handler);
    reset_signal();
    sig_handled = TRUE;
  }
}

/* Reset default unix behaviour */
extern int reset_default_signals (void) {
  int res;
  res = get_signal();
  reset_signal();
  if (sig_handled) {
    (void) signal(SIGINT, SIG_DFL);
    (void) signal(SIGTERM, SIG_DFL);
    (void) signal(SIGCHLD, SIG_DFL);
    (void) signal(SIGDUMMY, SIG_DFL);
    sig_handled = FALSE;
  }
  return res;
}

/* Are signal handled */
extern boolean signal_handling_set (void) {
  return sig_handled;
}

/* Compute time remaining */
extern void evt_time_remaining (timeout_t *remaining, timeout_t *exp_time) {

  timeout_t cur_time;

  if ( (remaining->tv_sec > 0) && (remaining->tv_usec > 0) ) {
    get_time (&cur_time);
    *remaining = *exp_time;
    if (sub_time (remaining, &cur_time) < 0) {
      remaining->tv_sec = 0;
      remaining->tv_usec = 0;
    }
  }
}

/* Check if a fd is set in a mask, from start_fd included
 *  to start_fd excluded, modulo last_fd + 1
 *  Return the fd, or NO_EVENT if no fd is set */
static int check_fd (int start_fd, fd_set *p_mask) {
  boolean started = FALSE;
  int i = start_fd;

  for (;;) {
    if (FD_ISSET(i, p_mask)) {
      return i;
    }
    if ( (i == start_fd) && (started) ) {
      /* All tried, not found */
      break;
    }
    started = TRUE;
    i = (i + 1) % (last_fd + 1);
  }
  return NO_EVENT;
}

extern int evt_wait (int *p_fd, boolean *p_read, timeout_t *timeout) {
  fd_set select_read_mask, select_write_mask;
  timeout_t exp_time, *timeout_ptr;
  boolean timeout_is_active;
  int start_fd;
  int n;
  ssize_t size;
  char c;

  /* Activate signal handling if necessary*/
  activate_signal_handling ();

  /* Compute exp_time = cur_time + timeout_ms */
  timeout_is_active = (timeout->tv_sec >= 0) && (timeout->tv_usec >= 0);
  if (timeout_is_active) {
    get_time (&exp_time);
    add_time (&exp_time, timeout);
    timeout_ptr = timeout;
  } else {
    timeout_ptr = NULL;
  }

  /* Init wake-up pipe */
  init_sig_and_wake_up ();

  for (;;) {

    /* Check for signal */
    if (check_signal() ) {
      *p_fd = SIG_EVENT;
      evt_time_remaining (timeout, &exp_time);
      return (WAIT_OK);
    }

    /* Copy select mask */
    memcpy (&select_read_mask,  &global_read_mask,  sizeof(fd_set));
    memcpy (&select_write_mask, &global_write_mask, sizeof(fd_set));

    /* Default result */
    *p_fd = NO_EVENT;
    *p_read = TRUE;

    /* Compute select timeout */
    if (timeout_is_active) {
      evt_time_remaining (timeout, &exp_time);
    }

    /* The select */
    n = select (last_fd + 1, &select_read_mask,
                             &select_write_mask, NULL, timeout_ptr);

    if (n > 0) {
      /* Start from 0 or from prev fd + 1 (round robin) */
      if (n == 1) {
        start_fd = 0;
      } else {
        start_fd = (prev_fd + 1) % (last_fd + 1);
      }

      /* Check read events first */
      *p_fd = check_fd (start_fd, &select_read_mask);
      if (*p_fd != NO_EVENT) {
         *p_read = TRUE;
      } else {
        /* Check write events second */
        *p_read = FALSE;
        *p_fd = check_fd (start_fd, &select_write_mask);
      }

      /* Check if p_fd is wake-up fd) */
      if (*p_fd == wake_up_fds[0] ) {
        for (;;) {
          size = read (wake_up_fds[0], &c, sizeof(c));
          if ( (size == -1) && (errno != EINTR) ) {
#ifdef DEBUG
            perror ("read");
#endif
            break;
          }
          if (size == 1) {
            break;
          }
        }
        *p_fd = WAKE_EVENT;
      } else if (*p_fd == NO_EVENT) {
#ifdef DEBUG
        fprintf (stderr, "No fd found\n");
#endif
        return (WAIT_ERR);
      }

      prev_fd = *p_fd;
      if (prev_fd < -1) {
        /* Not a fd => end of round robin */
        prev_fd = -1;
      }
      evt_time_remaining (timeout, &exp_time);
      return (WAIT_OK);

    } else if (n < 0) {
      if (errno != EINTR) {
        /* Real error */
#ifdef DEBUG
          perror ("select");
#endif
        return (WAIT_ERR);
      }
    }

    /* Check for timeout reached */
    if ( (!check_signal())
        && timeout_is_active
        && time_is_reached (&exp_time) ) {
      /* Done on timeout */
      *p_fd = NO_EVENT;
      timeout->tv_sec = 0;
      timeout->tv_usec = 0;
      return (WAIT_OK);
    }

  } /* for (;;) */
}

