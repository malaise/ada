#include <time.h>
#include <errno.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>

#include "sys_calls.h"

extern int time_to_tm (const time_t *the_time_p, my_tm_t *my_tm_p) {

  struct tm *tm_p;

  tm_p = gmtime (the_time_p);

  if (tm_p == (struct tm *) NULL) {
    return (-1);
  } else {
    my_tm_p->tm_sec  = tm_p->tm_sec;
    my_tm_p->tm_min  = tm_p->tm_min;
    my_tm_p->tm_hour = tm_p->tm_hour;
    my_tm_p->tm_mday = tm_p->tm_mday;
    my_tm_p->tm_mon  = tm_p->tm_mon + 1; /* mon is 0-11 in struct tm */
    my_tm_p->tm_year = tm_p->tm_year + 1900;
    return (0);
  }
}

extern int set_blocking (int fd, int blocking) {
  int flg;

  flg = fcntl (fd, F_GETFL, 0);
  if (flg < 0) {
    return (-1);
  }

  if (blocking) {
    flg &= ~O_NONBLOCK;
  } else {
    flg |= O_NONBLOCK;
  }

  if (fcntl (fd, F_SETFL, flg)  == -1) {
    return (-1);
  }

  return 0;

}

extern int set_tty_attr (int fd, int mode) {

  struct termios termattr;
  int blk;

  for (;;) {
    if (tcgetattr(fd, &termattr) == 0) {
      break;
    } else if (errno != EINTR) {
      return (-1);
    }
  }

  switch (mode) {
    case NORMAL:
      termattr.c_lflag |= ICANON;
      termattr.c_lflag |= ECHO;
      blk = 1;
    break;
    case NOECHO:
      termattr.c_lflag |= ICANON;
      termattr.c_lflag &= ~ECHO;
      blk = 1;
    break;
    case ASYNC:
      termattr.c_lflag &= ~ICANON;
      termattr.c_lflag |= ECHO;
      termattr.c_cc[VMIN] = 1;
      termattr.c_cc[VTIME]= 0;
      blk = 0;
    break;
    case TRANSP:
      termattr.c_lflag &= ~ICANON;
      termattr.c_lflag &= ~ECHO;
      termattr.c_cc[VMIN] = 1;
      termattr.c_cc[VTIME]= 0;
      blk = 0;
    break;
    default:
      errno = EINVAL;
      return (-1);
  }

  for (;;) {
    if (tcsetattr (fd, TCSANOW, &termattr) == 0) {
      break;
    } else if (errno != EINTR) {
      return (-1);
    }
  }

  return set_blocking (fd, blk);
}

extern int get_immediate (int fd) {

  ssize_t n;
  char c;

  for (;;) {
    n = read (fd, &c, 1);
    if (n < 0) {
      if (errno == EWOULDBLOCK) {
        return NONE;
      } else if (errno != EINTR) {
        perror ("get_immediate/read");
        return ERROR;
      }
    } else if (n > 0) {
      return ((unsigned char)c);
    } else {
      return CLOSED;
    }
  }
}

extern int read_dir (DIR *dir, char *name) {

  struct dirent * dir_ent;

  dir_ent = readdir (dir);
  if (dir_ent == NULL) {
    return (-1);
  }

  strcpy (name, dir_ent->d_name);
  return (strlen(name));
}

extern char **environ;
extern int env_len (void) {
  int i;

  i = 0;
  while (environ[i] != NULL) {
    i++;
  }
  return i;
}

extern char * env_val(int i) {

  if (i <= 0) {
    return NULL;
  } else {
    return environ[i - 1];
  }

}

extern int file_stat(const char *path, simple_stat *simple_stat_struct) {

  struct stat stat_struct;

  if (lstat (path, &stat_struct) != 0) {
    return (-1);
  }

  simple_stat_struct->mode = stat_struct.st_mode;
  simple_stat_struct->mtime = stat_struct.st_mtime;
  simple_stat_struct->size = (long)stat_struct.st_mtime;

  return (0);

}

extern int fd_stat(int fd, simple_stat *simple_stat_struct) {

  struct stat stat_struct;

  if (fstat (fd, &stat_struct) != 0) {
    return (-1);
  }

  simple_stat_struct->mode = stat_struct.st_mode;
  simple_stat_struct->mtime = stat_struct.st_mtime;
  simple_stat_struct->size = (long)stat_struct.st_mtime;

  return (0);

}

extern int fd_int_read (int fd, void *buffer, int nbytes) {
  ssize_t res;
  for (;;) {
    res = read (fd, buffer, (size_t) nbytes);
    if (res >= 0) {
      return (int)res;
    } else if (errno != EINTR) {
      return -1;
    }
  }
}

extern int fd_int_write (int fd, void *buffer, int nbytes) {
  ssize_t res;
  for (;;) {
    res = write (fd, buffer, (size_t) nbytes);
    if (res >= 0) {
      return (int)res;
    } else if (errno != EINTR) {
      return -1;
    }
  }
}

extern int fd_pipe (int *fd1, int *fd2) {
  int fds[2];

  for (;;) {
    if (pipe (fds) == 0) {
      *fd1 = fds[0];
      *fd2 = fds[1];
      return 0;
    } else if (errno != EINTR) {
      return -1;
    }
  }
}

extern int fd_close (int fd) {
  for (;;) {
    if (close (fd) == 0) {
      return 0;
    } else if (errno != EINTR) {
      return -1;
    }
  }
}

extern int procreate (void) {
  pid_t pid;

  pid = fork();
  if (pid == -1) {
    return 0;
  } else if (pid > 0) {
    return pid;
  } else {
    return (- getpid());
  }
}

extern void mutate (char * const program, int len) {
  int i, j, n;
  char * * argv;


  /* Check len of first string (prog name) */
  if ( (program == NULL) || (strlen (program) == 0) ) {
    return;
  }

  /* Count strings */
  n = 0;
  for (i = 0; i < len; i++) {
    if (program[i] == '\0') {
      n++;
    }
  }

  /* Allocate array of args */
  argv = (char**) malloc ((n + 1) * sizeof (char*));
  if (argv == NULL) {
    return;
  }

  /* Store pointers */
  i = 0;
  for (j = 0; j < n; j++) {
    argv[j] = &program[i];
    if (j == n - 1) {
      break;
    }
    i++;
    while (program[i] != '\0') {
      i++;
    }
    i++;
  }
  argv[n] = NULL;

  execv (argv[0], argv);
}

/* Waitpid (WNOHANG): pid is set to 0 if no more child */
#define NO_MORE  0
#define EXITED   1
#define SIGNALED 2
#define STOPPED  3
extern void next_dead (int *cause, int *pid, int *code) {

  int status;
  pid_t got_pid;

  for (;;) {
    got_pid = waitpid((pid_t)-1, &status, WNOHANG);
    if ( (got_pid != (pid_t)-1) || (errno != EINTR) ) break;
  }
  *pid = (int) got_pid;
  if ( (got_pid == (pid_t)-1) && (errno != ECHILD) ) {
    *pid = 0;
    *cause = ERROR;
    *code = 0;
  } else if ( (got_pid == 0)
         || ( (got_pid == (pid_t)-1) && (errno == ECHILD) ) ) {
    *pid = 0;
    *cause = NO_MORE;
    *code = 0;
  } else if (WIFEXITED(status)) {
    *cause = EXITED;
    *code = WEXITSTATUS(status);
  } else if (WIFSIGNALED(status)) {
    *cause = SIGNALED;
    *code = WTERMSIG(status);
  } else {
    *cause = STOPPED;
    *code = 0;
  }

}
