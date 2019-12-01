#include <strings.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "timeval.h"
#include "x_export.h"
#include "x_cards.h"

typedef struct {
  card *theCard;
  char ident[80];
} uCard;
uCard cards[4*13];

int main(int argc, char *argv[]) {

void *line, *line_event;
char name[150];
int fwidth, fheight, foffset;
int rows, cols;
int i, k, l, m;
int alt;
timeout_t delta;
boolean read;
void *ref;


  if (argc == 1) {
    if (getenv("DISPLAY") == NULL) {
      printf ("DISPLAY not set.\n");
      exit(1);
    }
    strcpy (name, getenv("DISPLAY"));
  } else if (argc == 2) {
    strcpy (name, argv[1]);
  } else {
    printf ("SYNTAX ERROR. Usage: %s [ <display> ]\n", argv[0]);
    exit(1);
  }

  /* Init */
  if (x_initialise (name, NULL) != 0) {
    printf ("ERROR initialise\n");
    exit(1);
  }
  /* Compute size in row/col */
  if (x_get_font_geometry (0, &fwidth, &fheight, &foffset) != 0) {
    printf ("ERROR initialise\n");
    exit(1);
  }
  /* 4 pixels between each stack + 4 on each side */
  cols = (float)((CARDWIDTH + 4) * 13 + 4) / (float)fwidth + 1;
  rows = 54;
  printf ("Cols = %d\n", cols);

  /* Open line and init desk */
  i = x_open_line (0, 1, 1, rows, cols, 2, 1, 0, &line);
  if (i != WAIT_OK) {
    printf ("ERROR open line\n");
    exit (1);
  }
  if (! initDesk (line, True) ) {
    printf ("ERROR init desk\n");
    exit (1);
  }
  if (x_get_font_name (line, name, sizeof(name)) != WAIT_OK) {
    printf ("ERROR getting font name\n");
    exit (1);
  }
  printf ("Font: %s\n", name);

  /* Create cards */
  strcpy (cards[0].ident, "8 Spade");
  cards[0].theCard = createCard (Spade, 7, &cards[0].ident);
  if (cards[0].theCard == NULL) {
    printf ("ERROR create card\n");
    exit (1);
  }
  move (cards[0].theCard, 4, 10);
  map (cards[0].theCard);

  strcpy (cards[1].ident, "Empty");
  cards[1].theCard = createEmpty (False, &cards[1].ident);
  move (cards[1].theCard, 104, 10);
  map (cards[1].theCard);

  strcpy (cards[2].ident, "Empty Squared");
  cards[2].theCard = createEmpty (True, &cards[2].ident);
  move (cards[2].theCard, 204, 10);
  map (cards[2].theCard);

  strcpy (cards[3].ident, "Symbol Heart");
  cards[3].theCard = createSymbol (Heart, &cards[3].ident);
  move (cards[3].theCard, 304, 10);
  map (cards[3].theCard);

  strcpy (cards[4].ident, "King Diamond face down");
  cards[4].theCard = createCard (Diamond, 12, &cards[4].ident);
  move (cards[4].theCard, 404, 10);
  turnOver (cards[4].theCard, False);
  map (cards[4].theCard);

  strcpy (cards[5].ident, "Ace Club selected");
  cards[5].theCard = createCard (Club, 0, &cards[5].ident);
  move (cards[5].theCard, 504, 10);
  doSelect (cards[5].theCard);
  map (cards[5].theCard);

  strcpy (cards[6].ident, "Two Club face down selected");
  cards[6].theCard = createCard (Club, 1, &cards[6].ident);
  move (cards[6].theCard, 604, 10);
  turnOver (cards[6].theCard, False);
  doSelect (cards[6].theCard);
  map (cards[6].theCard);

  k = REFRESH;
  l = 0;
  alt = 0;
  for (;;) {

    if (k != DISCARD) {
      /* Not timeout of select */

      if (k == REFRESH) {
        /* Redisplay */
        /* @@@ */
      }
    }

    /* Wait for events */
    delta.tv_sec = -1;
    delta.tv_usec = -1;

    if (l == 0) {
      /* no event pending */
      i = x_select (&m, &read, &delta);
    }

    if (m == X_EVENT) {
      x_process_event (&line_event, &ref, &k, &l);
      if (k == EXIT_REQ) {
        /* Request to exit by Window manager */
        printf ("Exit request\n");
        break;
      } else if (k == DISCARD) {
        /* Wrong X event i.e. expose... */
        continue;
      }
    } else if (m == NO_EVENT) {
      /* Timeout */
      printf ("Timeout\n");
      x_set_selection (line, NULL);
      k = DISCARD;
      continue;
    } else if (m == SIG_EVENT) {
      printf ("Signal\n");
      break;
    } else {
      printf ("ERROR: other event %d\n", m);
    }

    /* Keybord or TID */
    if (k == TID_PRESS) {
      printf ("Mouse press\n");
    } if (k == TID_RELEASE) {
      printf ("Mouse release\n");
      alt++;
      if (alt == 4) alt = 0;
      move (cards[0].theCard, alt * 100 + 04, 10);
      raise (cards[0].theCard);
    } else if (k == TID_ENTER) {
      printf ("Mouse enter\n");
    } else if (k == TID_LEAVE) {
      printf ("Mouse leave\n");
    }  else if (k == REFRESH) {
      printf ("Refresh\n");
    }
    if (ref != NULL) {
      printf ("Ref: %s\n", (char*)ref);
    }


  }/* end for(;;) */

  /* Close */
  if (x_close_line (line) != 0) {
    printf ("ERROR CLOSE LINE\n");
    exit (1);
  }

  exit (0);

}

