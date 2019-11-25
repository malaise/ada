#ifndef _X_BITMAPS_H
#define _X_BITMAPS_H

#include "x_cards.h"

/* Build the bitmap of a card */
extern void makeBitmap (suitList s, int v, char * bitmap);

/* Build the bitmap of a suite */
extern void makeOneSymbolBitmap (suitList s, char* bitmap);

#endif
