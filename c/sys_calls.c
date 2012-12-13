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
#include <pwd.h>
#include <grp.h>


#include "sys_calls.h"

extern int time_to_tm (const time_t *the_time_p, my_tm_t *my_tm_p) {

  struct tm *tm_p;

  tm_p = gmtime (the_time_p);

  if (tm_p == (struct tm *) NULL) {
    return (ERROR);
  } else {
    my_tm_p->tm_sec  = tm_p->tm_sec;
    my_tm_p->tm_min  = tm_p->tm_min;
    my_tm_p->tm_hour = tm_p->tm_hour;
    my_tm_p->tm_mday = tm_p->tm_mday;
    my_tm_p->tm_mon  = tm_p->tm_mon + 1; /* mon is 0-11 in struct tm */
    my_tm_p->tm_year = tm_p->tm_year + 1900;
    return OK;
  }
}

extern long gmt_offset (void) {

  time_t ttime;
  struct tm ltime;

  /* Get gmt offset of current local time */
  ttime = time (NULL);
  (void) localtime_r(&ttime, &ltime);
  return ltime.tm_gmtoff;
}

extern int get_blocking (int fd) {
   int flg;

  flg = fcntl (fd, F_GETFL, 0);
  if (flg < 0) {
    return ERROR;
  }

  return ((flg & O_NONBLOCK) == 0);
}

extern int set_blocking (int fd, boolean blocking) {
  int flg;

  flg = fcntl (fd, F_GETFL, 0);
  if (flg < 0) {
    return ERROR;
  }

  if (blocking) {
    flg &= ~O_NONBLOCK;
  } else {
    flg |= O_NONBLOCK;
  }

  if (fcntl (fd, F_SETFL, flg)  == -1) {
    return ERROR;
  }

  return OK;

}

extern int set_tty_attr (int fd, int mode) {

  struct termios termattr;
  int set_blk;

  for (;;) {
    if (tcgetattr(fd, &termattr) == 0) {
      break;
    } else if (errno != EINTR) {
      return ERROR;
    }
  }

  switch (mode) {
    case NORMAL:
      termattr.c_lflag |= ICANON;
      termattr.c_lflag |= ECHO;
      set_blk = TRUE;
    break;
    case NOECHO:
      termattr.c_lflag |= ICANON;
      termattr.c_lflag &= ~ECHO;
      set_blk = TRUE;
    break;
    case CHAR:
      termattr.c_lflag &= ~ICANON;
      termattr.c_lflag |= ECHO;
      termattr.c_cc[VMIN] = 1;
      termattr.c_cc[VTIME]= 0;
      set_blk = TRUE;
    break;
    case CHARNO:
      termattr.c_lflag &= ~ICANON;
      termattr.c_lflag &= ~ECHO;
      termattr.c_cc[VMIN] = 1;
      termattr.c_cc[VTIME]= 0;
      set_blk = TRUE;
    break;
    case ASYNC:
      termattr.c_lflag &= ~ICANON;
      termattr.c_lflag |= ECHO;
      termattr.c_cc[VMIN] = 1;
      termattr.c_cc[VTIME]= 0;
      set_blk = FALSE;
    break;
    case TRANSP:
      termattr.c_lflag &= ~ICANON;
      termattr.c_lflag &= ~ECHO;
      termattr.c_cc[VMIN] = 1;
      termattr.c_cc[VTIME]= 0;
      set_blk = FALSE;
    break;
    default:
      errno = EINVAL;
      return ERROR;
  }

  for (;;) {
    if (tcsetattr (fd, TCSANOW, &termattr) == 0) {
      break;
    } else if (errno != EINTR) {
      return ERROR;
    }
  }

  return set_blocking (fd, set_blk);
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
    return ERROR;
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

  simple_stat_struct->mode  = stat_struct.st_mode;
  simple_stat_struct->nlink = stat_struct.st_nlink;
  simple_stat_struct->uid   = stat_struct.st_uid;
  simple_stat_struct->gid   = stat_struct.st_gid;
  simple_stat_struct->mtime = stat_struct.st_mtime;
  simple_stat_struct->size = stat_struct.st_size;

  return OK;

}

extern int fd_stat(int fd, simple_stat *simple_stat_struct) {

  struct stat stat_struct;

  if (fstat (fd, &stat_struct) != 0) {
    return (-1);
  }

  simple_stat_struct->mode = stat_struct.st_mode;
  simple_stat_struct->mtime = stat_struct.st_mtime;
  simple_stat_struct->size = stat_struct.st_mtime;

  return OK;

}

/* Rights are -rw-r--r-- */
#define FD_ACCESS_RIGHTS (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
extern int fd_create (const char *path) {
  return creat(path, FD_ACCESS_RIGHTS);
}

/* Rights are -rwxr-xr-x */
#define DIR_ACCESS_RIGHTS (S_IRUSR | S_IWUSR | S_IXUSR \
                         | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH)
extern int dir_create (const char *path) {
  return mkdir(path, DIR_ACCESS_RIGHTS);
}

#define READ_ONLY  0
#define WRITE_ONLY 1
#define READ_WRITE 2
extern int fd_open (const char *path, int mode) {
  int flags;
  switch (mode) {
    case READ_ONLY:
      flags = O_RDONLY;
    break;
    case WRITE_ONLY:
      flags = O_WRONLY;
    break;
    case READ_WRITE:
      flags = O_RDWR;
    break;
    default:
      return (-1);
  }
  return open(path, flags);
}

extern int fd_int_read (int fd, void *buffer, int nbytes) {
  ssize_t res;
  for (;;) {
    res = read (fd, buffer, (size_t) nbytes);
    if (res >= 0) {
      return (int)res;
    } else if (errno != EINTR) {
      return ERROR;
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
      return ERROR;
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
      return ERROR;
    }
  }
}

extern int fd_close (int fd) {
  for (;;) {
    if (close (fd) == 0) {
      return 0;
    } else if (errno != EINTR) {
      return ERROR;
    }
  }
}

extern int procreate (void) {
  pid_t pid;

  pid = fork();
  if (pid == -1) {
    return OK;
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


/* Get user name from uid and get uid and gid from user name */
/* Return len on success and ERROR (-1) on error (not found) */
#define BUF_SIZE 1024
extern int get_user_name_of_uid (int uid, char *name) {
  int res;
  struct passwd pwbuf, *ppasswd;
  char buf[BUF_SIZE];

  /* Find entry matching uid */
  res = getpwuid_r((uid_t) uid, &pwbuf, buf, BUF_SIZE, &ppasswd);
  if (res != 0) return ERROR;
  strcpy (name, pwbuf.pw_name);
  return strlen (pwbuf.pw_name);
}

/* Return 0 on success and ERROR (-1) on error (not found) */
extern int get_ids_of_user_name (char *name, int *uid, int *gid) {
  int res;
  struct passwd pwbuf, *ppasswd;
  char buf[BUF_SIZE];

  /* Find entry matching name */
  res = getpwnam_r(name, &pwbuf, buf, BUF_SIZE, &ppasswd);
  if (res != 0) return ERROR;
  *uid = (int)pwbuf.pw_uid;
  *gid = (int)pwbuf.pw_gid;
  return OK;
}


/* Get group name from gid and get gid from group name */
/* Return len on success and ERROR (-1) on error (not found) */
extern int get_group_name_of_gid (int gid, char *name) {
  int res;
  struct group grpbuf, *pgroup;
  char buf[BUF_SIZE];

  /* Find entry matching uid */
  res = getgrgid_r((gid_t) gid, &grpbuf, buf, BUF_SIZE, &pgroup);
  if (res != 0) return ERROR;
  strcpy (name, grpbuf.gr_name);
  return strlen (grpbuf.gr_name);
}

/* Return 0 on success and ERROR (-1) on error (not found) */
extern int get_gid_of_group_name (char *name, int *gid) {
  int res;
  struct group grpbuf, *pgroup;
  char buf[BUF_SIZE];

  /* Find entry matching uid */
  res = getgrnam_r(name, &grpbuf, buf, BUF_SIZE, &pgroup);
  if (res != 0) return ERROR;
  *gid = grpbuf.gr_gid;
  return OK;
}

/* Get errno */
extern int get_errno (void) {
  return errno;
}

/* Long shift */
extern unsigned long shl_long (unsigned long l, int bits) {
  return l << bits;
}

extern unsigned long shr_long (unsigned long l, int bits) {
  return l >> bits;
}

