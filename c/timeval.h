#ifndef __TIMEVAL_H_
#define __TIMEVAL_H_

#include "boolean.h"
#include <sys/time.h>
typedef struct timeval timeout_t;

/* Normalises a time */
/* returns 1, 0, -1 reflecting the sign of the result */
extern int normalize_time (timeout_t *p_time);

/* Adds *p_val to *p_to and returns the sign of *p_to after addition */
/* p_val and p_to are normalized */
extern int add_time (timeout_t *p_to, timeout_t *p_val);

/* Subs *p_val to *p_to and returns the sign of *p_to after addition */
/* p_val and p_to are normalized */
extern int sub_time (timeout_t *p_to, timeout_t *p_val);

/* Adds delay in ms to *p_to */
extern int incr_time (timeout_t *p_to, unsigned int delay_ms);


/* Init a time with current time */
extern void get_time (timeout_t *p_time);


/* Compares 2 times (-1 if t1 < t2, 0 if t1 = t2, 1 if t1 > t2) */
extern int comp_time (timeout_t *p_time_1, timeout_t *p_time_2);


/* True is time is reached (current time >= time) */
extern boolean time_is_reached (timeout_t *p_time);

/* Waits for some time */
extern void delay (timeout_t *p_timeout);

/* Convert time to double (sec.usec) and reverse */
extern double time_to_double (timeout_t *p_time );
extern void double_to_time (double from, timeout_t *p_to);

#endif

