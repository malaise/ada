#include <time.h>

typedef struct {
  int tm_sec;       /* Seconds after the minute [0-60]   */
  int tm_min;       /* Minutes after the hour [0-59]     */
  int tm_hour;      /* Hours since midnight [0-23]       */
  int tm_mday;      /* Day of the month [1-31]           */
  int tm_mon;       /* Months since January [1-12] *******/
  int tm_year;      /* Years since 1900                  */
} my_tm_t;

int time_to_tm (time_t *the_time_p, my_tm_t *my_tm_p) {

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
