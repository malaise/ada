with Interfaces.C.Strings;
package body Icon is
  function "+" (Str : String) return Interfaces.C.Strings.Chars_Ptr is
  begin
    return Interfaces.C.Strings.New_String (Str);
  end "+";

  -- Xpm definition of icon
  Xpm : constant Interfaces.C.Strings.Chars_Ptr_Array := (
    +("128 128 3 1 "),
    +("  c None"),
    +("0 c black"),
    +("* c red"),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                            *                                                                   "),
    +("                                                            *                                                                   "),
    +("                                                            **                                                                  "),
    +("                                                           ***                                                                  "),
    +("                                                           ***                                                                  "),
    +("                                                           ***                                                                  "),
    +("                                                           ****                                                                 "),
    +("                                                          *****                                                                 "),
    +("                                                          *****                                                                 "),
    +("                                                          *****                                                                 "),
    +("                                                          ******                                                                "),
    +("                                                         *******                                                                "),
    +("                                                         *******                                           *                    "),
    +("                                                         *******                                         ***                    "),
    +("                                                         ********                                       ***                     "),
    +("                                                        *********                                     *****                     "),
    +("                                                        *********                                    *****                      "),
    +("                                                        *********                                  *******                      "),
    +("                        **                              **********                               ********                       "),
    +("                         **                            ***********                              *********                       "),
    +("                         ****                          ***********                            **********                        "),
    +("                         *****                         ***********                           ***********                        "),
    +("                         *******                       ************                        ************                         "),
    +("                          *******                     *************                      **************                         "),
    +("                          *********                   ******  *****                     **************                          "),
    +("                          **********                  ******  *****                   ****************                          "),
    +("                          ************                ******  ******                *****************                           "),
    +("                           ************              *******  ******               *****************                            "),
    +("                           **************            ******    *****             *******************                            "),
    +("                           ***************           ******    *****            *******************                             "),
    +("                            ****************         ******    ******         ************  *******                             "),
    +("                            *****************       *******    ******       *************  *******                              "),
    +("                            *******************     ******      *****      *************   *******                              "),
    +("                            ********************    ******      *****    *************    *******                               "),
    +("                             *********************  ******      ****** **************     *******                               "),
    +("                             *****************************      ********************     *******                                "),
    +("                             *******  ********************       *****************       *******                                "),
    +("                             *******   ******************        ****************       *******                                 "),
    +("                              ******     ****************        ***************        *******                                 "),
    +("                              *******     ***************        *************         *******                                  "),
    +("                              *******       ************          ***********          *******                                  "),
    +("                              *******        ***********          **********          *******                                   "),
    +("                               *******         *********          ********            *******                                   "),
    +("                               *******          ********          *******            *******                                    "),
    +("                               *******            ******           *****             *******                                    "),
    +("                                ******             ****            ***              *******                                     "),
    +("                                ******               **            **               *******                                     "),
    +("                                *******                                            *******                                      "),
    +("                                *******                                            *******                                      "),
    +("                                 ******                                           ********                                      "),
    +("                                 *******                                          *********************************             "),
    +("                                 *******                                         **********************************             "),
    +("                                 *******                                         ********************************               "),
    +("                                  ******                                         *******************************                "),
    +("                                  ******                                                                ******                  "),
    +("                                  *******                                                              ******                   "),
    +("                                  *******                                                            *******                    "),
    +("                                *********                                                          *******                      "),
    +("                              ************              000000000000                             ********                       "),
    +("                            **************            00000000000000000                        ********                         "),
    +("                          ***************            0000000000000000000                     *********                          "),
    +("                       ****************             00000000000000000000                   *********                            "),
    +("                     ***************                 0000000000000000000                 **********                             "),
    +("                   **************                00    000000000000000    0            **********                               "),
    +("                 *************              000000000      0000000      000000000     **********                                "),
    +("              *************             00000000000000                000000000000000    *****                                  "),
    +("            ************             000000000000000000000        0000000000000000000000   **                                   "),
    +("           ****************        0000000000000000000000000000000000000000000000000000000                                      "),
    +("              ****************   000000000000000000000000000000000000000 0000000000000000000                                    "),
    +("                 *************  00000000000000000000 000000000000000000  00000000000000000000                                   "),
    +("                     ********* 0000000000000000000000  00000000000000  00000000000000000000000                                  "),
    +("                        ***** 00000000000000000000000000    00000    0000000000000000000000000                                  "),
    +("                              00000000000000000000000000000000000000000000000000000000000000000                                 "),
    +("                              00000000000000000000000000000000000000000000000000000000000000000                                 "),
    +("                              0000000000000000000000000000000000000000000000000000000000000000                                  "),
    +("                               000000000000000000000000000000000000000000000000000000000000000                                  "),
    +("                                000000000000000000000000000000000000000000000000000000000000                                    "),
    +("                            00    000000000000000000000000000000000000000000000000000000000    0                                "),
    +("                            000     00000000000000000000000000000000000000000000000000000     000                               "),
    +("                           00000      0000000000000000000000000000000000000000000000000     00000                               "),
    +("                           0000000       000000000000000000000000000000000000000000        0000000                              "),
    +("                          0000000000          000000000000000000000000000000000         0000000000                              "),
    +("                          0000000000000              000000000000000000               0000000000000                             "),
    +("                         000000000000000000                                       00000000000000000                             "),
    +("                         00000000000000000000000                            000000000000000000000000                            "),
    +("                        00000000000000000000000000000000000       00000000000000000000000000000000000                           "),
    +("                       000000000000000000000000000000000000000000000000000000000000000000000000000000                           "),
    +("                       0000000000000000000000000000000000000000000000000000000000000000000000000000000                          "),
    +("                      00000000000000000000000000000000000000000000000000000000000000000000000000000000                          "),
    +("                      000000000000000000000000000000000000000000000000000000000000000000000000000000000                         "),
    +("                      000000000000000000000000000000000000000000000000000000000000000000000000000000000                         "),
    +("                      000000000000000000000000000000000000000000000000000000000000000000000000000000000                         "),
    +("                      000000000000000000000000000000000000000000000000000000000000000000000000000000000                         "),
    +("                       0000000000000000000000000000000000000000000000000000000000000000000000000000000                          "),
    +("                       000000000000000000000000000000000000000000000000000000000000000000000000000000                           "),
    +("                        0000000000000000000000000000000000000000000000000000000000000000000000000000                            "),
    +("                          0000000000000000000000000000000000000000000000000000000000000000000000000                             "),
    +("                           0000000000000000000000000000000000000000000000000000000000000000000000                               "),
    +("                             0000000000000000000000000000000000000000000000000000000000000000000                                "),
    +("                               000000000000000000000000000000000000000000000000000000000000000                                  "),
    +("                                 0000000000000000000000000000000000000000000000000000000000                                     "),
    +("                                    0000000000000000000000000000000000000000000000000000                                        "),
    +("                                        000000000000000000000000000000000000000000000                                           "),
    +("                                            0000000000000000000000000000000000000                                               "),
    +("                                                  000000000000000000000000                                                      "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                "),
    +("                                                                                                                                ") );

  function Mine_Icon return Gdk.Pixbuf.Gdk_Pixbuf is
  begin
    -- Create Pixbuf from Xpm
    return Gdk.Pixbuf.Gdk_New_From_Xpm_Data (Xpm);
  end Mine_Icon;

end Icon;
