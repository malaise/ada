#include <stdlib.h>
#include <stdio.h>

#include <X11/extensions/shape.h>

#include "x_line.h"
#include "x_cards.h"
#include "x_bitmaps.h"

#include "bitmaps/boundingMask.bm"
#include "bitmaps/clipMask.bm"
#include "bitmaps/cursor.bm"
#include "bitmaps/cursor_back.bm"

#define WHITE       "white"
#define BLACK       "black"
#define RED         "red"
#define SELECTED    "lightskyblue4"
#define BACK        "darkblue"
#define EMPTY       "forestgreen"
#define EMPTYBORDER "green"

/* Common static data */
static t_window *win_id = NULL;
static Display* display = NULL;
static int      screen;
static Drawable rootWindow = None;
static Window   parentWindow = None;
static Colormap colorMap;
static unsigned long backgroundColor;
static unsigned long whiteColor;
static unsigned long blackColor;
static unsigned long redColor;
static unsigned long selectedColor;
static unsigned long backColor;
static unsigned long emptyColor;
static unsigned long emptyBorderColor;
static Pixmap boundingMask;
static Pixmap clipMask;
static Cursor cursor;
static char bitmap[BMWIDTH * (CARDHEIGHT - 2)];


/* Common utilities */

/* Display error message */
static void errorMsg (const char* message) {
  fprintf (stderr, "XCards error: %s\n", message);
}

/* Allocate a color */
static unsigned long getColor(const char* name) {
  XColor c0, c1;

  XAllocNamedColor(display, colorMap, name, &c1, &c0);
  return c1.pixel;
}

/* Set the background color */
static void setBackground (const card* aCard, const unsigned long color) {
  XSetWindowBackground (display, aCard->xWindow, color);
}

/* Set the background pixmap */
static void setBackgroundPixmap (const card* aCard, const Pixmap pixmap) {
  XSetWindowBackgroundPixmap (display, aCard->xWindow, pixmap);
}

/* Set border color */
static void setBorder (const card* aCard, const unsigned long color,
                       const int width) {
  XSetWindowBorder (display, aCard->xWindow, color);
  XSetWindowBorderWidth (display, aCard->xWindow, width);
}

/* Color of a suit */
typedef enum {RedSuit, BlackSuit, Error} suitColor;
static suitColor suitColorOf (suitList suit) {
  switch (suit) {
    case Diamond: case Heart:
      return RedSuit;
    case Spade: case Club:
      return BlackSuit;
  }
  return Error;
}
static unsigned long colorOf (const suitList suit) {
  if (suitColorOf (suit) == RedSuit) {
    return redColor;
  } else {
    return blackColor;
  }
}


/* Global initialization: Get local copy of line info, other global init */
extern boolean initDesk (void *line_id) {
  XColor cursFore, cursBack, xColor;
  Pixmap bitMap, mask;

  /* Copy data from line */
  win_id = (t_window*) line_id;
  display = win_id->server->x_server;
  screen = win_id->screen->x_screen;
  rootWindow = win_id->screen->x_root_win;
  parentWindow = win_id->x_window;
  colorMap = win_id->screen->colormap;
  backgroundColor = win_id->screen->color_id[win_id->background_color];

  /* Colors */
  whiteColor = getColor (WHITE);
  blackColor = getColor (BLACK);
  redColor = getColor (RED);
  selectedColor = getColor (SELECTED);
  backColor = getColor (BACK);
  emptyColor = getColor (EMPTY);
  emptyBorderColor = getColor (EMPTYBORDER);

  /* Card shape pixmaps */
  boundingMask = XCreateBitmapFromData (display, rootWindow,
      (char*)boundingMask_bits,
      boundingMask_width, boundingMask_height);
  clipMask = XCreateBitmapFromData (display, rootWindow,
      (char*)clipMask_bits,
      clipMask_width, clipMask_height);

  /* Cursor for valid target */
  bitMap = XCreateBitmapFromData (display, parentWindow,
      (char*) cursor_bits, cursor_width, cursor_height);
  mask = XCreateBitmapFromData (display, parentWindow,
      (char*) cursor_back_bits, cursor_back_width, cursor_back_height);
  XAllocNamedColor (display, colorMap, WHITE, &cursFore, &xColor);
  XAllocNamedColor (display, colorMap, BLACK, &cursBack, &xColor);
  cursor = XCreatePixmapCursor (display, bitMap, mask,
      &cursFore, &cursBack, 0, 0);

  return True;
}

/* Common operations for creating a card */
static card* createCommon (void *ref) {
  card* aCard;
  XWindowAttributes attributes;

  /* Alloc */
  aCard = malloc (sizeof (card));
  if (aCard == NULL) {
    perror ("malloc card");
    return NULL;
  }

  /* Create window */
  aCard->xWindow = XCreateSimpleWindow (display, parentWindow, 0, 0,
      CARDWIDTH, CARDHEIGHT, 1,
      BlackPixel(display, screen),
      WhitePixel(display, screen));
  if (aCard == None) {
     errorMsg ("Cannot create window for card");
     return NULL;
  }

  aCard->faceUp = True;

  /* Select input */
  XGetWindowAttributes (display, aCard->xWindow, &attributes);
  XSelectInput(display, aCard->xWindow,
      attributes.your_event_mask | ButtonPressMask | ButtonReleaseMask
                                 | EnterWindowMask | LeaveWindowMask
                                 | PointerMotionMask);

  /*  Register */
  if (!lin_register ((t_window *) win_id, aCard->xWindow, ref, True) ) {
      return NULL;
  }
  return aCard;
}

/* Card creation */
extern card* createEmpty (const boolean squared, void *ref) {
  card* aCard;

  if ((aCard = createCommon(ref)) == NULL) return NULL;

  if (squared) {
    setBackground (aCard, emptyColor);
    setBorder (aCard, emptyBorderColor, 1);
  } else {
    setBorder (aCard, backgroundColor, 1);
    setBackground (aCard, backgroundColor);
  }
  return aCard;
}

extern card* createSymbol (const suitList suit, void *ref) {
  card* aCard;
  unsigned long fore, back;
  Pixmap bgpixmap;

  if ((aCard = createCommon(ref)) == NULL) return NULL;

  fore = colorOf (suit);
  back = whiteColor;
  makeOneSymbolBitmap(suit, bitmap);
  bgpixmap = XCreatePixmapFromBitmapData (
      display, parentWindow, bitmap, CARDWIDTH - 2, CARDHEIGHT - 2, fore, back,
      DefaultDepth (display, screen));
  setBackgroundPixmap (aCard, bgpixmap);
  setBorder (aCard, blackColor, 1);

  return aCard;
}

extern card* createCard (const suitList suit, const int value, void *ref) {
  card* aCard;
  unsigned long fore, back, hilight;

  if ((aCard = createCommon(ref)) == NULL) return NULL;
  fore = colorOf (suit);
  back = whiteColor;
  hilight = selectedColor;
  makeBitmap(suit, value, bitmap);

  aCard->usualPixmap = XCreatePixmapFromBitmapData (
      display, parentWindow, bitmap, CARDWIDTH - 2, CARDHEIGHT - 2, fore,
      back, DefaultDepth (display, screen));
  aCard->hilightedPixmap = XCreatePixmapFromBitmapData (
      display, parentWindow, bitmap, CARDWIDTH - 2, CARDHEIGHT - 2, fore,
      hilight, DefaultDepth (display, screen));

  XShapeCombineMask(display, aCard->xWindow, ShapeBounding, 0, 0,
      boundingMask, ShapeSet);
  XShapeCombineMask(display, aCard->xWindow, ShapeClip, 0, 0,
      clipMask, ShapeSet);

  setBorder (aCard, blackColor, 1);
  unSelect (aCard);
  return aCard;
}

/* Card display and move */
extern void map (card* aCard) {
  XMapWindow (display, aCard->xWindow);
  XRaiseWindow (display, aCard->xWindow);
}

extern void unmap (card* aCard) {
  XUnmapWindow (display, aCard->xWindow);
}

extern void move (card* aCard, const int x, const int y) {
  XMoveWindow (display, aCard->xWindow, x, y);
}

extern void doSelect (card* aCard) {
  if (aCard->faceUp) {
    setBackgroundPixmap (aCard, aCard->hilightedPixmap);
  } else {
    setBackground (aCard, selectedColor);
  }
  XClearWindow (display, aCard->xWindow);
}

extern void unSelect (card* aCard) {
  if (aCard->faceUp) {
    setBackgroundPixmap (aCard, aCard->usualPixmap);
  } else {
    setBackground (aCard, backColor);
  }
  XClearWindow (display, aCard->xWindow);
}

extern void turnOver (card* aCard, const boolean faceUp) {
  aCard->faceUp = faceUp;
  if (faceUp) {
    setBorder (aCard, blackColor, 1);
  } else {
    setBorder (aCard, whiteColor, 5);
  }
  unSelect (aCard);
}

