/*  @(#) TAAATS PROGRAM FILE %M% Release %I% %H% %T% ~  */
static char C_YEK_X []=" @(#) TAAATS PROGRAM FILE %M% Release %I% %H% %T% ~";
/* May 29, 1996 : Increase generate Shift preffix then Control preffix          */
/* Jan 14, 1997 : Allow 4 bytes of key code from X. Treat last 2 bytes          */

#include "x_key.h"
#include <X11/keysym.h>


#define HIG_BYTE 0xFF00
#define LOW_BYTE 0x00FF

#define KEY_PREFIX 0xFF
#define KEY_SHIFT   0xE1
#define KEY_CONTROL 0xE3

boolean key_chain (XKeyEvent *p_x_key, int key_buf[], int *p_nbre_key) {

    KeySym key_sym;
    char dummy_str[4];

      XLookupString(p_x_key, dummy_str, 4, &key_sym, NULL);
      key_sym = key_sym & 0x0000FFFF;

      /* Skip any event about a modifier managed later on for generating keys */
      /* Left or right SHIFT key, Left or right CONTROL key, CAPS_LOCK or SHIFT_LOCK key */
      if ( (key_sym == XK_Shift_L)   || (key_sym == XK_Shift_R)
        || (key_sym == XK_Control_L) || (key_sym == XK_Control_R) 
        || (key_sym == XK_Caps_Lock) || (key_sym == XK_Shift_Lock) ) {
        *p_nbre_key = 0;
        return (False);
      }

      *p_nbre_key = 0;
      if ( (p_x_key->state & ControlMask) != 0) {
          /* Case of Ctrl active */
          key_buf[*p_nbre_key]     = KEY_PREFIX;
          key_buf[(*p_nbre_key)+1] = KEY_CONTROL;
          *p_nbre_key += 2;
      }

      /* Add preffifx (eventually preceeded by Shift preffix */
      if ( (key_sym & HIG_BYTE) == HIG_BYTE ) {
          /* Add Shift preffix only before KEY_PREFIX */
          if ( (p_x_key->state & ShiftMask) != 0) {
              /* Case of Shift active */
              key_buf[*p_nbre_key]     = KEY_PREFIX;
              key_buf[(*p_nbre_key)+1] = KEY_SHIFT;
              *p_nbre_key += 2;
          }
          key_buf[*p_nbre_key] = KEY_PREFIX;
          (*p_nbre_key) ++;
      }

      /* Add code */
      key_buf[*p_nbre_key] = (int) (key_sym & LOW_BYTE);
      (*p_nbre_key) ++;
/*
printf ("READ KEY %02X %02X %02X %02X %d\n",
(int)key_buff[0], (int)key_buff[1], (int)key_buff[2], (int)key_buff[3], 
*pp_nbre_key);
*/
        return (True);
}
 

