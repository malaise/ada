/* May 29, 1996 : Increase generate Shift preffix then Control preffix          */
/* Jan 14, 1997 : Allow 4 bytes of key code from X. Treat last 2 bytes          */

#include "x_key.h"
#include <X11/keysym.h>
#include <X11/Xutil.h>


#define HIG_BYTE 0xFF00
#define LOW_BYTE 0x00FF

#define KEY_PREFIX 0xFF
#define KEY_SHIFT   0xE1
#define KEY_CONTROL 0xE3

void key_chain(XKeyEvent *p_x_key, int *p_control, int *p_shift,
                  int *p_code, int key_buf[], int *p_nbre_key) {

    KeySym key_sym;
    char str[4];
    int nb_char;
    int i;

    /* Init out parameters */
    *p_control = 0;
    *p_shift = 0;
    *p_code = 0;
    memset (key_buf, 0, NBRE_MAX_KEY);
    *p_nbre_key = 0;

    /* Decode keybord event */
    nb_char = XLookupString(p_x_key, str, sizeof(str), &key_sym, NULL);
    key_sym = key_sym & 0x0000FFFF;


    /* Control modifier checked by default */
    if ( (p_x_key->state & ControlMask) != 0) {
        *p_control = 1;
    }

    /* Skip any event about a modifier managed later on for generating keys */
    /* Left or right SHIFT key, Left or right CONTROL key, CAPS_LOCK or SHIFT_LOCK key */
    /* Alt Gr (XK_ISO_Level3_Shift) */
    if ( (key_sym == XK_Shift_L)   || (key_sym == XK_Shift_R)
      || (key_sym == XK_Control_L) || (key_sym == XK_Control_R)
      || (key_sym == XK_Caps_Lock) || (key_sym == XK_Shift_Lock)
      || (key_sym == XK_ISO_Level3_Shift) ) {
        *p_control = 0;
    } else if ( (key_sym == 0) && (nb_char == 0) ) {
        *p_control = 0;
    } else if (key_sym == XK_ISO_Left_Tab) {
        /* Handle Shift+Tab as Shift + Tab */
        *p_shift = 1;
        *p_code = 1;
        key_buf[(*p_nbre_key)+0] = KEY_PREFIX;
        key_buf[(*p_nbre_key)+1] = 0x09;
        *p_nbre_key += 2;
    } else if ( ( (key_sym & HIG_BYTE) == HIG_BYTE) 
             || (nb_char == 0) ) {
        /* Shift is set only for function keys or if no translation */
        /* Add Shift prefix if needed */
        if ( (p_x_key->state & ShiftMask) != 0) {
            /* Case of Shift active */
            *p_shift = 1;
        }
        /* Add Codes */
        *p_code = 1;
        key_buf[(*p_nbre_key)+0] = (key_sym & HIG_BYTE)>>8;
        key_buf[(*p_nbre_key)+1] = key_sym & LOW_BYTE;
        *p_nbre_key +=2;
    } else {
        /* Normal character translated into a sequence of "bytes": add codes */
        for (i = 0; i < nb_char; i++) {
            key_buf[*p_nbre_key] = (unsigned char) str[i];
            (*p_nbre_key) ++;
        }
    }

}

