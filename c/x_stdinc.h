#ifndef _X_STDINC_H
#define _X_STDINC_H
/*  @(#) TAAATS PROGRAM FILE %M% Release %I% %H% %T% ~  */
static char H_CNIDTS_X []=" @(#) TAAATS PROGRAM FILE %M% Release %I% %H% %T% ~";
#ifndef STD_INC
#define STD_INC

#include <stdio.h>
#include <X11/Xlib.h>

#define error(str) (printf("%s\n",str), exit(1))

#define boolean int
#define byte unsigned char

#endif

#endif
/* _X_STDINC_H */

