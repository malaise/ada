#include <strings.h>
#include <stdio.h> 
#include <stdlib.h>
#include <sys/time.h>


#include "x_export.h"

#define CLL "                                                      "

#define NBRE_COLOR 14

#define T1 "Usage is: test_x display [back border font] (0-9 0-9 0-1)."
#define T2 "Use any mouse button to simulate TID."
#define T3 "Use s, u, b and r keys to change attributes."
#define T4 "Use + - keys to shift inks and * / keys to shift backgrounds."
#define T5 "To exit, touch TID (or click/release) in row 1 col 1."
#define YES_STR " Yes. "
#define NO_STR  " No.  "
#define STD_SFX_NAME ":0.0"

#define TITLE_LNE   2
#define COLOURS_LNE TITLE_LNE + 11
#define STATUS_LNE  COLOURS_LNE + 2 * NBRE_COLOR + 1 
#define EVENT_LNE   STATUS_LNE + 3

int back;


void put(void *line, char *string) {

  (void) x_put_string (line, string, strlen(string));
}

void title (void *line) {

  (void) x_move (line, TITLE_LNE + 0, 2);
  (void) x_set_attributes (line, back, 13, 0, 0, 0, 0);
  (void) put (line, T1);
  (void) x_move (line, TITLE_LNE + 2, 2);
  (void) put (line, T2);
  (void) x_move (line, TITLE_LNE + 3, 2);
  (void) put (line, T3);
  (void) x_move (line, TITLE_LNE + 4, 2);
  (void) put (line, T4);
  (void) x_move (line, TITLE_LNE + 6, 2);
  (void) put (line, T5);
}

void main(int argc, char *argv[]) 
{

void *line, *line_event;
char name[50];
char stra[81], stre[81];
char digits[5];
int i, j, k, l;
fd_set m;
int kk[6];
int bord, font;
unsigned int b1;
unsigned int s, u, b, r;
int delta;
int b_off, c_off, bv, cv;

  /* Parameters */ 
  name[0] = '\0';
  font = 0;
  s = 0; u = 0; b = 0; r = 0;

  if (argc == 2) {
    strcpy (name, argv[1]); 
    back = 0;
    bord = 1;
    font = 0;
  } else if (argc == 5) {
    strcpy (name, argv[1]); 
    back = atoi (argv[2]);
    bord = atoi (argv[3]);
    font = atoi (argv[4]);
  } else {
    printf ("SYNTAX ERROR: %s\n", T1);
    exit(1);
  }

  if ( (back < 0) || (back > 9) || (bord < 0) || (bord > 9)
    || (font < 0) || (font > 1) ) {
    printf ("SYNTAX ERROR: %s\n", T1);
    exit(1);
  }


  /* Init */
  strcat (name, ":0.0");
  if (x_initialise (name) != 0) {
    printf ("ERROR INITIALISE\n");
    exit(1);
  }

  /* Open */

  if (font == 0) {
    i = x_open_line (0, 10, 20, 50, 80, back, bord, font, &line);
  } else {
    i = x_open_line (0, 5, 1, 50, 80, back, bord, font, &line);
  }
  if (i != 0) {
    printf ("ERROR OPEN LINE\n");
    exit (1);
  }

  delta = 0;
  strcpy (stre, "");
  b_off = 0; c_off = 1;

  k = REFRESH;
  l = 0;
  for (;;) {

    if (k != DISCARD) {
      /* Not timeout of select */

      if (k == REFRESH) {
        /* Title */
        title(line);
      }

      /* Text display  : show colors */
      for (i = 0; i < NBRE_COLOR; i++) {
        cv = (i + c_off) % NBRE_COLOR;
        if (cv < 0) cv += NBRE_COLOR;
        bv = (i + b_off) % NBRE_COLOR;
        if (bv < 0) bv += NBRE_COLOR;
        x_move (line, COLOURS_LNE + (2*i), 30);
        x_set_attributes (line, bv, cv, s, u, b, r);
        strcpy (stra, "Ink ");
        sprintf (digits, "%2d", cv);
        strcat (stra, digits);
        strcat (stra, " on Background ");
        sprintf (digits, "%2d", bv);
        strcat (stra, digits);
        put (line, stra);
      }
      x_move (line, COLOURS_LNE, 10);
      x_set_attributes (line, bv, 2, 0, 0, 0, 0);
      x_draw_area (line, 5, 2*NBRE_COLOR);

      /* Text display  : show attributes */
      strcpy (stra, "(s)uperbright:");
      if (s) strcat (stra, YES_STR); else strcat (stra, NO_STR);
      strcat (stra, "(u)nderscore:");
      if (u) strcat (stra, YES_STR); else strcat (stra, NO_STR);
      strcat (stra, "(b)link:");
      if (b) strcat (stra, YES_STR); else strcat (stra, NO_STR);
      strcat (stra, "(r)everse:");
      if (r) strcat (stra, YES_STR); else strcat (stra, NO_STR);
      x_move (line, STATUS_LNE, 7);
      x_set_attributes (line, back, 13, 0, 0, 0, 0);
      put (line, stra);

      /* Text display  : show event */
      x_set_attributes (line, back, 13, 0, 0, 0, 0);
      x_move (line, EVENT_LNE, 10);
      put (line, CLL);
      x_move (line, EVENT_LNE, 10);
      put (line, stre);
      
    }


    /* Wait for events */
    FD_ZERO(&m);
    delta = 2000;

    if (l == 0) {
      /* no event pending */
      i = x_select (&m, &l, &delta);
    }

    if (l != 0) {
      x_process_event (&line_event, &k, &l);
      if (k == DISCARD) {
        /* Wrong X event i.e. expose... */
        continue;
      }
    } else {
      /* Timeout */
      k = DISCARD;
      continue;
    }

    /* Keybord or TID */
    if (k == KEYBOARD) {

      /* Keyboard */
      strcpy (stre, "Keys: ");
      
      /* read the codes */
      x_read_key (line_event, kk, &j); 
      for (i = 0; i < j; i++) {
        /* Build string */
        strcat (stre, "<");
        sprintf (digits, "%02X", kk[i]);
        strcat (stre, digits);
        strcat (stre, "> ");
     }

     if (j == 1) {
       b1 = (unsigned char) kk[0];
       /* Single key : Modify attributes if key is s, u, b, or r */
       /* Shift colors if + - / *                                */
       if (b1 == 's') s = ! s;
       if (b1 == 'u') u = ! u;
       if (b1 == 'b') b = ! b;
       if (b1 == 'r') r = ! r;
       if (b1 == '+') c_off = (c_off - 1) % NBRE_COLOR;
       if (b1 == '-') c_off = (c_off + 1) % NBRE_COLOR;
       if (b1 == '*') b_off = (b_off - 1) % NBRE_COLOR;
       if (b1 == '/') b_off = (b_off + 1) % NBRE_COLOR;
     }

    } else if (k == TID_PRESS) {

      /* TID press */
      strcpy (stre, "TID press buttton: ");
      x_read_tid (line_event, &l, &i, &j);
      sprintf (digits, "%03d", l);
      strcat (stre, digits);
      strcat (stre, "    row: ");
      sprintf (digits, "%03d", i);
      strcat (stre, digits);
      strcat (stre, "    col: ");
      sprintf (digits, "%03d", j);
      strcat (stre, digits);

    } else if (k == TID_RELEASE) {

      /* TID release */
      strcpy (stre, "TID release row: ");
      x_read_tid (line_event, &l, &i, &j);
      sprintf (digits, "%03d", l);
      strcat (stre, digits);
      strcat (stre, "    row: ");
      sprintf (digits, "%03d", i);
      strcat (stre, digits);
      strcat (stre, "    col: ");
      sprintf (digits, "%03d", j);
      strcat (stre, digits);
      (void) x_bell(1);

    }  else if (k == REFRESH) {
       /* Redraw to be done */
       strcpy (stre, "Refresh");
    }

    printf ("%s\n", stre);

    /* Exit when TID release in upper left corner */
    if ( (k == TID_RELEASE) && (i == 1) && (j == 1) ) {
      printf ("Exit\n");
      break;
    }


  }/* end for(;;;) */

  /* Close */
  if (x_close_line (line) != 0) {
    printf ("ERROR CLOSE LINE\n");
    exit (1);
  }

  exit (0);

} 

