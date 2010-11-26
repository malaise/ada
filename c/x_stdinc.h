#ifndef _X_STDINC_H
#define _X_STDINC_H

#include <string.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xft/Xft.h>

#define error(str) (printf("%s\n",str), exit(1))

#define boolean int
#define byte unsigned char

#endif

/* _X_STDINC_H */


