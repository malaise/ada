#include <strings.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "timeval.h"
#include "x_export.h"

#define CLL "                                                      "

#define NBRE_COLOR 14

#define T1 "Usage is: test_x [display [back border font]] (0-9 0-9 0-3)."
#define T2 "Use any mouse button to simulate TID. m to enable motion."
#define T3 "Use s, u and r keys to change attributes."
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

/* 7 * 8 */
unsigned char draw[] = {0, 0, 0, 1, 0, 0, 0, \
                        0, 0, 1, 1, 1, 0, 0, \
                        0, 0, 1, 1, 1, 0, 0, \
                        0, 0, 1, 1, 1, 0, 0, \
                        0, 1, 1, 1, 1, 1, 0, \
                        0, 1, 1, 1, 1, 1, 0, \
                        1, 1, 1, 1, 1, 1, 1, \
                        1, 1, 1, 1, 1, 1, 1 };


static void put(void *line, const char *string, int row, int column) {

  (void) x_put_string (line, string, (int)strlen(string), row, column);
}

static void title (void *line) {
  unsigned char str[80];
  char tit[80];
  int ww, wh, fw, fh, fo;

  (void) x_clear_line (line);
  (void) x_set_attributes (line, back, 13, 0, 0, 0);
  put (line, T1, TITLE_LNE + 0, 2);
  put (line, T2, TITLE_LNE + 2, 2);
  put (line, T3, TITLE_LNE + 3, 2);
  put (line, T4, TITLE_LNE + 4, 2);
  put (line, T5, TITLE_LNE + 6, 2);
  /* € and œ */
  str[0]=0xE2; str[1]=0x82; str[2]=0xAC;
  str[3]=0xC5; str[4]=0x93; str[5]='\0';
  /* Other french letters */
  strcpy (tit, "àâéêëèîïôùûüç ");
  strcat (tit, (char*)str);
  put (line, (char*)tit, 0, 30);
  /* Font names and geo */
  x_get_font_name (line, tit, sizeof(str));
  put (line, tit, TITLE_LNE + 7, 2);
  x_get_bold_name (line, tit, sizeof(str));
  put (line, tit, TITLE_LNE + 8, 2);
  x_get_graph_charact (line, &ww, &wh, &fw, &fh, &fo);
  sprintf (tit, "%dx%d+%d", fw, fh, fo);
  put (line, tit, TITLE_LNE + 9, 2);

}

int main(int argc, char *argv[])
{

void *line, *line_event;
char name[50];
char stra[81], stre[81], strs[81];
char selection[81];
char digits[5];
int i, j, k, l, m;
int kk[4];
boolean control, shift, code;
int bord, font;
unsigned int b1;
unsigned int s, u, r;
timeout_t delta;
int b_off, c_off, bv, cv;
int motion;
boolean read;

  /* Parameters */
  name[0] = '\0';
  font = 0;
  s = 0; u = 0; r = 0;

  if (argc == 1) {
    if (getenv("DISPLAY") == NULL) {
      printf ("DISPLAY not set.\n");
      exit(1);
    }
    strcpy (name, getenv("DISPLAY"));
    back = 0;
    bord = 1;
    font = 0;
  } else if (argc == 2) {
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
    || (font < 0) || (font > 3) ) {
    printf ("SYNTAX ERROR: %s\n", T1);
    exit(1);
  }


  /* Init */
  if (x_initialise (name, NULL) != 0) {
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

  strcpy (stre, "");
  b_off = 0; c_off = 1;
  motion = False;

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
        strcpy (stra, "Ink ");
        sprintf (digits, "%2d", cv);
        strcat (stra, digits);
        strcat (stra, " on Background ");
        sprintf (digits, "%2d", bv);
        strcat (stra, digits);
        x_set_attributes (line, bv, cv, (int)s, (int)u, (int)r);
        put (line, stra, COLOURS_LNE + (2*i), 30);
      }
      x_set_attributes (line, bv, 2, 0, 0, 0);
      x_draw_area (line, 5, 2*NBRE_COLOR, COLOURS_LNE, 10);
      x_fill_rectangle (line, 0, 0, 9, 14);
      x_draw_rectangle (line, 10, 0, 19, 14);
      x_draw_points (line, 20, 1, 7, 8, draw);

      /* Text display  : show attributes */
      strcpy (stra, "(s)uperbright:");
      if (s) strcat (stra, YES_STR); else strcat (stra, NO_STR);
      strcat (stra, "(u)nderscore:");
      if (u) strcat (stra, YES_STR); else strcat (stra, NO_STR);
      strcat (stra, "(r)everse:");
      if (r) strcat (stra, YES_STR); else strcat (stra, NO_STR);
      x_set_attributes (line, back, 13, 0, 0, 0);
      put (line, stra, STATUS_LNE, 7);

      /* Text display  : show event */
      x_set_attributes (line, back, 13, 0, 0, 0);
      put (line, CLL, EVENT_LNE, 10);
      put (line, stre, EVENT_LNE, 10);

    }


    /* Wait for events */
    delta.tv_sec = 2;
    delta.tv_usec = 0;

    if (l == 0) {
      /* no event pending */
      i = x_select (&m, &read, &delta);
    }

    if (m == X_EVENT) {
      x_process_event (&line_event, &k, &l);
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
    if (k == KEYBOARD) {

      /* Keyboard */
      strcpy (stre, "Keys: ");

      /* Read the key codes */
      x_read_key (line_event, &control, &shift, &code, kk, &j);
      if (control) strcat (stre, "Ctrl ");
      if (shift)   strcat (stre, "Shift ");
      if (code)    strcat (stre, "Code ");
      for (i = 0; i < j; i++) {
        /* Build the codes */
        strcat (stre, "<");
        sprintf (digits, "%02X", kk[i]);
        strcat (stre, digits);
        strcat (stre, "> ");
     }
     /* Read the string if any */
     memset (strs, 0, sizeof(strs));
     if (! code) {
       strcat (stre, " -->");
       for (i = 0; i < j; i++) {
         strs[i] = (unsigned char) kk[i];
       }
       strcat (stre, strs);
       strcat (stre, "<");
       x_set_selection (line, stre);
     }

     if (j == 1) {
       b1 = (unsigned char) kk[0];
       /* Single key : Modify attributes if key is s, u, b, or r */
       /* Shift colors if + - / *                                */
       if (b1 == 's') s = ! s;
       if (b1 == 'u') u = ! u;
       if (b1 == 'r') r = ! r;
       if (b1 == '+') c_off = (c_off - 1) % NBRE_COLOR;
       if (b1 == '-') c_off = (c_off + 1) % NBRE_COLOR;
       if (b1 == '*') b_off = (b_off - 1) % NBRE_COLOR;
       if (b1 == '/') b_off = (b_off + 1) % NBRE_COLOR;
       if (b1 == 'm') {
         motion = ! motion;
         x_enable_motion_events (line, motion);
       }
     }

    } else if (k == TID_PRESS) {

      /* TID press */
      strcpy (stre, "TID press buttton: ");
      x_read_tid (line_event, TRUE, &l, &i, &j);
      sprintf (digits, "%03d", l);
      strcat (stre, digits);
      strcat (stre, "    row: ");
      sprintf (digits, "%03d", i);
      strcat (stre, digits);
      strcat (stre, "    col: ");
      sprintf (digits, "%03d", j);
      strcat (stre, digits);
      if (l == 2) {
        x_request_selection (line);
      }

    } else if (k == TID_RELEASE) {

      /* TID release */
      strcpy (stre, "TID release buttton: ");
      x_read_tid (line_event, TRUE, &l, &i, &j);
      sprintf (digits, "%03d", l);
      strcat (stre, digits);
      strcat (stre, "    row: ");
      sprintf (digits, "%03d", i);
      strcat (stre, digits);
      strcat (stre, "    col: ");
      sprintf (digits, "%03d", j);
      strcat (stre, digits);
      (void) x_bell(1);
    } else if (k == TID_MOTION) {
      strcpy (stre, "TID motion buttton: ");
      x_read_tid (line_event, TRUE, &l, &i, &j);
      sprintf (digits, "%03d", l);
      strcat (stre, digits);
      strcat (stre, "    x: ");
      sprintf (digits, "%03d", i);
      strcat (stre, digits);
      strcat (stre, "    y: ");
      sprintf (digits, "%03d", j);
      strcat (stre, digits);

    } else if (k == SELECTION) {
      if (x_get_selection (line, selection, sizeof(selection)) != WAIT_OK) {
        printf ("Reading selection failed\n");
      } else {
        printf ("Selection -> %s\n", selection);
      }
    }  else if (k == REFRESH) {
      x_read_tid (line_event, FALSE, &l, &i, &j);
      /* Redraw to be done */
      strcpy (stre, "Refresh");
    }

    printf ("%s\n", stre);

    /* Exit when TID release in upper left corner */
    if ( (k == TID_RELEASE) && (i == 1) && (j == 1) ) {
      printf ("Exit button\n");
      break;
    }


  }/* end for(;;) */

  /* Close */
  if (x_close_line (line) != 0) {
    printf ("ERROR CLOSE LINE\n");
    exit (1);
  }

  exit (0);

}

