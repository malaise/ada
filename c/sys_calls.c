#include <time.h>
#include <errno.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>


typedef struct {
  int tm_sec;       /* Seconds after the minute [0-60]   */
  int tm_min;       /* Minutes after the hour [0-59]     */
  int tm_hour;      /* Hours since midnight [0-23]       */
  int tm_mday;      /* Day of the month [1-31]           */
  int tm_mon;       /* Months since January [1-12] *******/
  int tm_year;      /* Years since 1900                  */
} my_tm_t;

extern int time_to_tm (time_t *the_time_p, my_tm_t *my_tm_p) {

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

#define NORMAL 0
#define NOECHO 1
#define ASYNC  2
#define TRANSP 3

extern int set_stdin_attr (int mode) {

  struct termios termattr;
  int res;

  if (tcgetattr(0, &termattr) != 0) {
    return (-1);
  }

  switch (mode) {
    case NORMAL:
      termattr.c_lflag |= ICANON;
      termattr.c_lflag |= ECHO;
    break;
    case NOECHO:
      termattr.c_lflag |= ICANON;
      termattr.c_lflag &= ~ECHO;
    break;
    case ASYNC:
      termattr.c_lflag &= ~ICANON;
      termattr.c_lflag |= ECHO;
      termattr.c_cc[VMIN] = 1;
      termattr.c_cc[VTIME]= 0;
    break;
    case TRANSP:
      termattr.c_lflag &= ~ICANON;
      termattr.c_lflag &= ~ECHO;
      termattr.c_cc[VMIN] = 1;
      termattr.c_cc[VTIME]= 0;
    break;
    default:
      errno = EINVAL;
      return (-1);
    break;
  }


  if (tcsetattr (0, TCSANOW, &termattr) != 0) {
    return (-1);
  }

  res = fcntl (0, F_GETFL, 0);
  if (res < 0) {
    return (-1);
  }
  switch (mode) {
    case NORMAL:
    case NOECHO:
      res &= ~O_NONBLOCK;   
    break;
    case ASYNC:
    case TRANSP:
       res |= O_NONBLOCK;
    break;
    default:
      errno = EINVAL;
      return (-1);
    break;
  }

  if (fcntl (0, F_SETFL, res)  == -1) {
    return (-1);
  }

  return 0;

}

extern int get_immediate_stdin (void) {

  ssize_t n;
  char c;

  n = read (0, &c, 1);

  if ( (n < 0) && (errno != EWOULDBLOCK) ) {
    perror ("get_immediate_stdin/read");
    return (-1);
  } else if (n > 0) {
    return ((char)c);
  } else {
    return (-1);
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

typedef struct {
  unsigned int mode;
  int mtime;
} simple_stat;

extern int file_stat(const char *path, simple_stat *simple_stat_struct) {

  struct stat stat_struct;

  if (lstat (path, &stat_struct) != 0) {
    return (-1);
  }

  simple_stat_struct->mode = stat_struct.st_mode;
  simple_stat_struct->mtime = stat_struct.st_mtime;

  return (0);

}

