#ifndef _X_KEY_H
#define _X_KEY_H
/*  @(#) TAAATS PROGRAM FILE %M% Release %I% %H% %T% ~  */
static char H_YEK_X []=" @(#) TAAATS PROGRAM FILE %M% Release %I% %H% %T% ~";
/* May 29, 1996 : Increase NBRE_MAX_KEY from 4 to 6 to generate Shift preffix   */

#include "x_stdinc.h"

#define NBRE_MAX_KEY	6

boolean key_chain(XKeyEvent *p_x_key, int key_buf[], int *p_nbre_key);

#endif
/* _X_KEY_H */
