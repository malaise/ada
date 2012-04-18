#include <errno.h>
#include <stdio.h>
#include <limits.h>
#include <math.h>
extern double trunc(double x);
extern double round(double x);

#include "timeval.h"

#define MILLION  1000000
#define THOUSAND    1000

#define DMILLION  (double) 1000000

/*
 ******************************************************************************
 * Function : normalize_time
 *
 * Abstract : Set time at normalized format:
 *                 0 < abs(usec) < MILLION
 *                 sec and usec have same sign
 *
 * Decisions : None
 *
 * Input  : p_time  address of the time structure to normalize
 *
 * Output : p_time  address of the normalized time structure
 *
 * Return : -1 if the time is negative, 0 if it 0, 1 if it is positive
 *
 * Errors : None
 *
 * History :
 *
 *******************************************************************************
*/
/* Normalises a time */
/* returns 1, 0, -1 reflecting the sign of the result */
int normalize_time (timeout_t *p_time)
{
  /* abs usec < MILLION */
  while (p_time->tv_usec <= - MILLION) {
    (p_time->tv_sec) --;
    (p_time->tv_usec) += MILLION;
  }

  while (p_time->tv_usec >= MILLION) {
    (p_time->tv_sec) ++;
    (p_time->tv_usec) -= MILLION;
  }

  /* sec and usec have same sign */
  if ( (p_time->tv_sec > 0) && (p_time->tv_usec < 0) ) {
    (p_time->tv_sec) --;
    (p_time->tv_usec) += MILLION;
  } else if ( (p_time->tv_sec < 0) && (p_time->tv_usec > 0) ) {
    (p_time->tv_sec) ++;
    (p_time->tv_usec) -= MILLION;
  }


  /* Ok if time is > 0 */
  if ( (p_time->tv_sec > 0) ||
       ( (p_time->tv_sec == 0) && (p_time->tv_usec > 0) ) ) {
    return (1);
  } else if ( (p_time->tv_sec == 0)  && (p_time->tv_usec == 0) ) {
    return (0);
  } else {
    return (-1);
  }
}



/*
 ******************************************************************************
 * Function : add_time
 *
 * Abstract : Adds a time p_val to a time p_to
 *
 * Decisions : Signs are taken into account and the result is normalized
 *
 * Input  : p_to   time value a
 *          p_val  time value b
 *
 * Output : p_to   time value of a+b
 *
 * Return : -1 if the resulting time is negative, 0 if it 0, 1 if it is positive
 *
 * Errors : None
 *
 * History :
 *
 *******************************************************************************
*/
/* Adds *p_val to *p_to and returns the sign of *p_to after addition */
/* p_val and p_to are normalized */
int add_time (timeout_t *p_to, timeout_t *p_val)
{

  /* normalize the values */
  (void) normalize_time (p_val);
  (void) normalize_time (p_to);

  /* Addition */
  (p_to->tv_sec)  += p_val->tv_sec;
  (p_to->tv_usec) += p_val->tv_usec;

  /* normalize the result */
  return (normalize_time (p_to));
}


/*
 ******************************************************************************
 * Function : sub_time
 *
 * Abstract : Substract a time p_val to a time p_to
 *
 * Decisions : Signs are taken into account and the result is normalized
 *
 * Input  : p_to   time value a
 *          p_val  time value b
 *
 * Output : p_to   time value of a-b
 *
 * Return : -1 if the resulting time is negative, 0 if it 0, 1 if it is positive
 *
 * Errors : None
 *
 * History :
 *
 *******************************************************************************
*/
/* Subs *p_val to *p_to and returns the sign of *p_to after addition */
/* p_val and p_to are normalized */
int sub_time ( timeout_t *p_to,  timeout_t *p_val)
{
    timeout_t mval;

    /* mval = - val */
    mval.tv_sec  = - p_val->tv_sec;
    mval.tv_usec = - p_val->tv_usec;
    /* Add mval to to */
    return ( add_time (p_to, &mval));
}


/*
 ******************************************************************************
 * Function : add_time
 *
 * Abstract : Adds a delay in milliseconds to a time p_to
 *
 * Decisions : Signs are taken into account and the result is normalized
 *
 * Input  : p_to      time value a
 *          delay_ms  value b in milliseconds
 *
 * Output : p_to   value of a+b
 *
 * Return : -1 if the resulting time is negative, 0 if it 0, 1 if it is positive
 *
 * Errors : None
 *
 * History :
 *
 *******************************************************************************
*/
/* Adds delay in ms to *p_to */
int incr_time (timeout_t *p_to, unsigned int delay_ms)
{

    timeout_t val;

    val.tv_sec  = (time_t)delay_ms / (time_t)THOUSAND;
    val.tv_usec = ( (long)delay_ms % (long)THOUSAND ) * (long)THOUSAND;

    return (add_time(p_to, &val));
}


/*
 ******************************************************************************
 * Function : get_time
 *
 * Abstract : Get the current time
 *
 * Decisions : Calls UNIX gettimeofday function
 *
 * Input  : None
 *
 * Output : p_time  Adress where the current time has to be put
 *
 * Return : None
 *
 * Errors : None
 *
 * History :
 *
 *******************************************************************************
*/
/* init a time with current time */
void get_time (timeout_t *p_time)
{
  struct timezone zone;

  gettimeofday(p_time, &zone);
}


/*
 ******************************************************************************
 * Function : comp_time
 *
 * Abstract : Compares 2 times
 *
 * Decisions : Signs are taken into account
 *
 * Input  : p_time_1   value a
 *          p_time_2   value b
 *
 * Output :
 *
 * Return : -1 if a<b,  0 if a=b,  1 if a>b
 *
 * Errors : None
 *
 * History :
 *
 *******************************************************************************
*/
/* Compares 2 times (-1 if t1 < t2, 0 if t1 = t2, 1 if t1 > t2) */
int comp_time (timeout_t *p_time_1, timeout_t *p_time_2)
{
    normalize_time (p_time_1);
    normalize_time (p_time_2);

    if (p_time_1->tv_sec > p_time_2->tv_sec) {
        return (1);
    } else if (p_time_1->tv_sec < p_time_2->tv_sec) {
        return (-1);
    } else if (p_time_1->tv_usec > p_time_2->tv_usec) {
        return (1);
    } else if (p_time_1->tv_usec < p_time_2->tv_usec) {
        return (-1);
    } else {
        return (0);
    }
}


/*
 ******************************************************************************
 * Function : time_is_reached
 *
 * Abstract : Check if given time is reached (current_time >= given_time)
 *
 * Decisions : None
 *
 * Input  : p_time  The expiry date
 *
 * Output : None
 *
 * Return : True  the date is reached,
 *          False the date is not reached yet.
 *
 * Errors : None
 *
 * History :
 *
 *******************************************************************************
*/
boolean time_is_reached (timeout_t *p_time)
{
    timeout_t current_time;

    /* Get current time */
    get_time (&current_time);

    /* reached if time_current >= time */
    return (comp_time (&current_time, p_time) >= 1);
}


/*
 ******************************************************************************
 * Function : delay
 *
 * Abstract : Wait for some time (during some delay)
 *
 * Decisions : Loop while EINTR
 *
 * Input  : p_timeout  The delay to wait
 *
 * Output : None
 *
 * Return : None
 *
 * Errors : None
 *
 * History :
 *
 *******************************************************************************
*/
extern void delay ( timeout_t *p_timeout ) {
    timeout_t cur_time, exp_time, delta_time;
    int cr;

  /* Compute expiration time */
  get_time (&exp_time);
  (void) add_time (&exp_time, p_timeout);


  for (;;) {

    /* Compute delay until expiration */
    delta_time = exp_time;
    get_time (&cur_time);
    if (sub_time (&delta_time, &cur_time) > 0) {
      cr = select (0, NULL, NULL, NULL, &delta_time);
    } else {
      cr = 0;
    }

    /* Done or interrupted */
    if (cr == 0) {
      return;
    } else if (errno != EINTR) {
      perror ("select");
    }
  }
}

/*
 ******************************************************************************
 * Function : wait_until
 *
 * Abstract : Wait until the given time is reached
 *
 * Decisions : None
 *
 * Input  : p_time  The time to wait until it is reached
 *
 * Output : None
 *
 * Return : None
 *
 * Errors : None
 *
 * History :
 *
 *******************************************************************************
*/
extern void wait_until ( timeout_t *p_time ) {
  timeout_t current_time, timeout;

  get_time (&current_time);
  timeout = *p_time;
  if (sub_time (&timeout, &current_time) <= 0) {
    /* Time is reached */
    return;
  }
  delay (&timeout);
}

/*
 ******************************************************************************
 * Function : time_to_double
 *
 * Abstract : Convert time to double (seconds dot microseconds)
 *
 * Decisions : None
 *
 * Input  : p_time  The time to convert
 *
 * Output : None
 *
 * Return : The double value of conversion
 *
 * Errors : None
 *
 * History :
 *
 *******************************************************************************
*/
extern double time_to_double (timeout_t *p_time ) {
  normalize_time (p_time);
  return (double) p_time->tv_sec
       + ((double) p_time->tv_usec) / DMILLION;
}

/*
 ******************************************************************************
 * Function : double_to_time
 *
 * Abstract : Convert a double (seconds dot microseconds) to time
 *
 * Decisions : Normalize the result
 *
 * Input  : from  The double value to convert
 *
 * Output : p_to  address of the converted time structure
 *
 * Return : The double value of conversion
 *
 * Errors : None
 *
 * History :
 *
 *******************************************************************************
*/
extern void double_to_time (double from, timeout_t *p_to) {
  double s, u;

  /* Try to round usecs */
  if (from < HUGE / DMILLION) {
    /* Cannot round usecs: trunc them */
    s = trunc (from);
    u = trunc ((from - s) * MILLION);
  } else {
    /* Round usecs */
    u = from * DMILLION;
    u = (rint (u)) / DMILLION;
    /* Split */
    s = trunc (u);
    u = (u - s) * DMILLION;
  }

  /* Convert to ints, checking overflow */
  p_to->tv_sec = (sizeof(p_to->tv_sec) == sizeof(long) ? LONG_MAX : INT_MAX);
  if (s <= (double) p_to->tv_sec) {
    p_to->tv_sec = (time_t)s;
  }
  p_to->tv_usec = (long)u;
  normalize_time (p_to);

}

