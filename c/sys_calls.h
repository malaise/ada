#ifndef _SYS_CALLS_H_
#define _SYS_CALLS_H_


typedef struct {
  int tm_sec;       /* Seconds after the minute [0-60]   */
  int tm_min;       /* Minutes after the hour [0-59]     */
  int tm_hour;      /* Hours since midnight [0-23]       */
  int tm_mday;      /* Day of the month [1-31]           */
  int tm_mon;       /* Months since January [1-12] *******/
  int tm_year;      /* Years since 1900                  */
} my_tm_t;

extern int time_to_tm (const time_t *the_time_p, my_tm_t *my_tm_p);


extern int set_blocking (int fd, int blocking);


#define NORMAL 0
#define NOECHO 1
#define ASYNC  2
#define TRANSP 3

extern int set_tty_attr (int fd, int mode);


#define ERROR  (-1)
#define NONE   (-2)
#define CLOSED (-3)

extern int get_immediate (int fd);


extern int read_dir (DIR *dir, char *name);


extern int env_len (void);

extern char * env_val(int i);


typedef struct {
  unsigned int mode;
  int mtime;
  long size;
} simple_stat;

extern int file_stat(const char *path, simple_stat *simple_stat_struct);

extern int fd_stat(int fd, simple_stat *simple_stat_struct);

extern int fd_int_read (int fd, void *buffer, int nbytes);

extern int fd_int_write (int fd, void *buffer, int nbytes);

extern int fd_pipe (int *fd1, int *fd2);

extern int fd_close (int fd);

/* Fork. >0 : father, pid of child, <0 : child, -pid, 0 : error */
extern int procreate (void);

/* Execv */
extern void mutate (char * const program, int len);

/* Waitpid (WNOHANG): pid is set to 0 if no more child */
/* Cause may be ERROR, or */
#define NO_MORE  0
#define EXITED   1
#define SIGNALED 2
#define STOPPED  3
/* Pid is set if not ERROR nor NO_MORE, Code if EXITED or SIGNALED */
extern void next_dead (int *cause, int *pid, int *code);

#endif

