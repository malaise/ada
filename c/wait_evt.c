#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/select.h>
#include <signal.h>
#include <errno.h>
#include <string.h>

#include "wait_evt.h"

/***** Fds Management   *****/
static fd_set global_read_mask;
static fd_set global_write_mask;

static int last_fd = -1;

int evt_add_fd (int fd, boolean read) {
  if (fd < 0) {
    return (ERR);
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
  return (OK);
}

int evt_del_fd (int fd, boolean read) {
  int i;
  fd_set *mask;

  if (fd < 0) {
    return (ERR);
  }
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

extern boolean evt_fd_set (int fd, boolean read) {
  if (fd < 0) {
    return (ERR);
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


/***** Sig Management   *****/
static int map_signal (int sig_num) {
  if (sig_num == SIGINT) {
    return (SIG_TERMINATE);
  } else if (sig_num == SIGTERM) {
    return (SIG_TERMINATE);
  } else if (sig_num == SIGCHLD) {
    return (SIG_CHILD);
  } else if (sig_num == SIG_DUMMY) {
    return (SIG_DUMMY);
  } else if (sig_num == SIG_NONE) {
    return (SIG_NONE);
  } else {
    return (SIG_UNKNOWN);
  }
}

static int sig_received = SIG_NONE;
static int last_sig = SIG_NONE;
static boolean sig_handled = FALSE;
static void signal_handler (int sig) {
  int new_sig = map_signal (sig);
  /* Discard NONE or UNKNOWN */
  if (sig < SIG_DUMMY) return;
  /* Discard new signal if a more urgent is pending */
  /* DUMMY < CHILD < TERM */
  if (new_sig <= sig_received) return;
  /* Store signal received */
  sig_received = new_sig;
}

extern void send_signal (int sig) {
  signal_handler (sig);
}

extern int get_signal (void) {
  return last_sig;
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
static void init_evt (void) {
  /* Set handler if not set */
  if (! sig_handled) {
    (void) signal(SIGINT, signal_handler);
    (void) signal(SIGTERM, signal_handler);
    (void) signal(SIGCHLD, signal_handler);
    (void) signal(SIGPIPE, SIG_IGN);
    sig_handled = TRUE;
  }
  if (wake_up_fds[0] == -1) {
    (void) pipe (wake_up_fds);
    (void) evt_add_fd (wake_up_fds[0], TRUE);
  }
}

extern int reset_default_signals (void) {
  int res;
  res = sig_received;
  sig_received = SIG_NONE;
  if (sig_handled) {
    (void) signal(SIGINT, SIG_DFL);
    (void) signal(SIGTERM, SIG_DFL);
    (void) signal(SIGCHLD, SIG_DFL);
    (void) signal(SIGPIPE, SIG_DFL);
    sig_handled = FALSE;
  }
  return res;
}

/* Compute time remaining */
extern void evt_time_remaining (int *timeout_ms, timeout_t *exp_time) {

  timeout_t cur_time;

  if (*timeout_ms > 0) {
    get_time (&cur_time);
    if (sub_time (exp_time, &cur_time) >= 0) {
      *timeout_ms = exp_time->tv_sec * 1000 + exp_time->tv_usec / 1000;
    } else {
      *timeout_ms = 0;
    }
  }
}


extern int evt_wait (int *p_fd, boolean *p_read, int *timeout_ms) {
  fd_set select_read_mask, select_write_mask;
  timeout_t exp_time, timeout, *timeout_ptr;
  boolean timeout_is_active;
  int i, n;
  ssize_t size;
  char c;

  /* Compute exp_time = cur_time + timeout_ms */
  timeout_is_active = *timeout_ms >= 0;
  if (timeout_is_active) {
    get_time (&exp_time);
    incr_time (&exp_time, (unsigned int) *timeout_ms);
    timeout_ptr = &timeout;
  } else {
    timeout_ptr = NULL;
  }

  /* Init signal handling and wake-up pipe */
  init_evt ();

  for (;;) {

    /* Check for signal */
    if (sig_received != SIG_NONE) {
      last_sig = sig_received;
      sig_received = SIG_NONE;
      *p_fd = SIG_EVENT;
      evt_time_remaining (timeout_ms, &exp_time);
      return (OK);
    }

    /* Copy select mask */
    memcpy (&select_read_mask,  &global_read_mask,  sizeof(fd_set));
    memcpy (&select_write_mask, &global_write_mask, sizeof(fd_set));

    /* Default result */
    *p_fd = NO_EVENT;
    *p_read = TRUE;

    /* Compute select timeout */
    if (timeout_is_active) {
      evt_time_remaining (timeout_ms, &exp_time);
      timeout.tv_sec =  (*timeout_ms / 1000);
      timeout.tv_usec = (*timeout_ms % 1000) * 1000;
    }

    /* The select */
    n = select (last_fd + 1, &select_read_mask,
                             &select_write_mask, NULL, timeout_ptr);

    if (n > 0) {
      /* Check read events first */
      for (i = 0; i <= last_fd; i++) {
        if (FD_ISSET(i, &select_read_mask)) {
          *p_fd = i;
          *p_read = TRUE;
          break;
        }
      }

      /* Check write events second */
      if (*p_fd == NO_EVENT) {
        for (i = 0; i <= last_fd; i++) {
          if (FD_ISSET(i, &select_write_mask)) {
            *p_fd = i;
            *p_read = FALSE;
            break;
          }
        }
      }

      /* Check i p_fd is wake-up fd) */
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
        return (ERR);
      }

      evt_time_remaining (timeout_ms, &exp_time);
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

    /* Check for timeout reached */
    if ( (timeout_is_active) && time_is_reached (&exp_time) ) {
      /* Done on timeout */
      *p_fd = NO_EVENT;
      *timeout_ms = 0;
      return (OK);
    }

  } /* for (;;) */
}

