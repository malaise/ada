#ifndef _X_KEY_H
#define _X_KEY_H
/* May 29, 1996 : Increase NBRE_MAX_KEY from 4 to 6 to generate Shift preffix   */

#include "x_stdinc.h"

#define NBRE_MAX_KEY 4

void key_chain(XKeyEvent *p_x_key, int *p_control, int *p_shift,
               int *p_code, int key_buf[], int *p_nbre_key);

#endif
/* _X_KEY_H */

