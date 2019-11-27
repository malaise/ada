#ifndef _X_CARDS_H
#define _X_CARDS_H

#include "x_stdinc.h"
#include "x_line.h"

#define CARDWIDTH    79
#define CARDHEIGHT  123
#define BMWIDTH     ((CARDWIDTH - 2 + 7) / 8)


typedef enum {Heart = 0, Diamond = 1, Club = 2, Spade = 3} suitList;

typedef struct {
  Window xWindow;
  boolean faceUp;
  Pixmap usualPixmap, hilightedPixmap;
} card;

/* Global initialization */
extern boolean initDesk (void *line_id, const boolean enable_motion);

/* Card creation */
extern card* createEmpty (const boolean squared, void *ref);
extern card* createSymbol (const suitList suit, void *ref);
extern card* createCard (const suitList suit, const int value, void *ref);

/* Card display and move */
extern void map (card* aCard);
extern void unmap (card* aCard);
extern void move (card* aCard, const int x, const int y);
extern void doSelect (card* aCard);
extern void unSelect (card* aCard);
/* Turns over and unselects */
extern void turnOver (card* aCard, const boolean faceUp);

#endif
