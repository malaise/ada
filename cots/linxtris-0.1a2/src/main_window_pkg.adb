-----------------------------------------------------------------------
--
--     Copyright (C) 2003 Dúlio Matos Leite de Carvalho e Silva
--
--     This file is part of LinXtris.
--
--     LinXtris is free software; you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation; either version 2 of the License, or
--     (at your option) any later version.
--
--     This program is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.
--
--     You should have received a copy of the GNU General Public License
--     along with this program; if not, write to the Free Software
--     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
-----------------------------------------------------------------------

with Gtk.Window; use Gtk.Window;
with Gtk.Widget;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Enums;
with Main_Window_Pkg.Callbacks; use Main_Window_Pkg.Callbacks;
with Preferences_Window_Pkg;
with Gtk.Table; use Gtk.Table;
with Double_Buffer; use Double_Buffer;
with Gdk.Pixmap; use Gdk.Pixmap;
with Gdk;
with Gdk.Color;
with Gdk.Bitmap;
with Gdk.GC;
with Gdk.Drawable;
with Block_Engine;
with Game_Engine;
with Gtk.Label;
with Gtk.Misc;
with Gtk.Container;
with Gtk.Frame;
with Gtk.Alignment;
with Gtk.Accel_Group;
with Gdk.Types;
with Gdk.Types.Keysyms;
--with GLib.Properties;
--with GLib.Object;
with Scores_Window_Pkg;
with Glib; use Glib;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;

package body Main_Window_Pkg is

   procedure Gtk_New( Win: out Main_Window_Type ) is
   begin
      Win := new Main_Window_Record;
      Win.Engine := new Block_Engine.Object(10, 23);
      Block_Engine.Init( Win.Engine.All );
      Main_Window_Pkg.Initialize( Win );
   end Gtk_New;

   procedure Initialize( Win: access Main_Window_Record'Class ) is
      Gdkw: Gdk.Gdk_Window;
      lbl: Gtk.Label.Gtk_Label;
      Frame: Gtk.Frame.Gtk_Frame;
      Alg: Gtk.Alignment.Gtk_Alignment;
      AGroup: Gtk.Accel_Group.Gtk_Accel_Group;
   begin
      Gtk.Window.Initialize( Win, Gtk.Enums.Window_Toplevel );
      Gtk.Menu_Bar.Gtk_New( Win.Menu_Bar );
      Gtk.Menu_Item.Gtk_New_With_Mnemonic( Win.Item_Game, "_Game" );
      Gtk.Menu_Bar.Add( Win.Menu_Bar, Win.Item_Game );
      Gtk.Menu.Gtk_New( Win.Item_Game_Submenu );
      Gtk.Menu_Item.Set_Submenu( Win.Item_Game, Win.Item_Game_Submenu );
      Gtk.Menu_Item.Gtk_New_With_Mnemonic( Win.Item_Game_New, "_New" );
      Gtk.Menu.Add( Win.Item_Game_Submenu, Win.Item_Game_New);
      Gtk.Menu_Item.Gtk_New_With_Mnemonic( Win.Item_Game_Pause, "_Pause" );
      Gtk.Menu.Add( Win.Item_Game_Submenu, Win.Item_Game_Pause);
      Gtk.Menu_Item.Set_Sensitive( Win.Item_Game_Pause, False );
      Gtk.Menu_Item.Gtk_New( Win.Item_Game_Sep1 );
      Gtk.Menu.Add( Win.Item_Game_Submenu, Win.Item_Game_Sep1);
      Gtk.Menu_Item.Gtk_New_With_Mnemonic( Win.Item_Game_Scores, "_Scores..." );
      Gtk.Menu.Add( Win.Item_Game_Submenu, Win.Item_Game_Scores);
      Gtk.Menu_Item.Gtk_New( Win.Item_Game_Sep2 );
      Gtk.Menu.Add( Win.Item_Game_Submenu, Win.Item_Game_Sep2);
      Gtk.Menu_Item.Gtk_New_With_Mnemonic( Win.Item_Game_Quit, "_Quit" );
      Gtk.Menu.Add( Win.Item_Game_Submenu, Win.Item_Game_Quit);

      Gtk.Menu_Item.Gtk_New_With_Mnemonic( Win.Item_Settings, "_Settings" );
      Gtk.Menu_Bar.Add( Win.Menu_Bar, Win.Item_Settings );
      Gtk.Menu.Gtk_New( Win.Item_Settings_Submenu );
      Gtk.Menu_Item.Set_Submenu( Win.Item_Settings, Win.Item_Settings_Submenu );
      Gtk.Menu_Item.Gtk_New_With_Mnemonic( Win.Item_Settings_Preferences, "_Preferences" );
      Gtk.Menu.Add( Win.Item_Settings_Submenu, Win.Item_Settings_Preferences);

      Gtk.Menu_Item.Gtk_New_With_Mnemonic( Win.Item_Help, "_Help" );
      Gtk.Menu_Bar.Add( Win.Menu_Bar, Win.Item_Help );
      Gtk.Menu.Gtk_New( Win.Item_Help_Submenu );
      Gtk.Menu_Item.Set_Submenu( Win.Item_Help, Win.Item_Help_Submenu );
      Gtk.Menu_Item.Gtk_New_With_Mnemonic( Win.Item_Help_About, "_About" );
      Gtk.Menu.Add( Win.Item_Help_Submenu, Win.Item_Help_About);

      Gtk.Accel_Group.Gtk_New( AGroup );
      Add_Accel_Group( Win, AGroup );
      Gtk.Menu_Item.Add_Accelerator( Win.Item_Game_New, "activate", AGroup,
                                     Gdk.Types.Keysyms.GDK_LC_n, Gdk.Types.Control_Mask, Gtk.Accel_Group.Accel_Visible );

      Gtk.Menu_Item.Add_Accelerator( Win.Item_Game_Quit, "activate", AGroup,
                                     Gdk.Types.Keysyms.GDK_LC_q, Gdk.Types.Control_Mask, Gtk.Accel_Group.Accel_Visible );

      Gtk.Menu_Item.Add_Accelerator( Win.Item_Game_Pause, "activate", AGroup,
                                     Gdk.Types.Keysyms.GDK_Pause, Gdk.Types.Release_Mask, Gtk.Accel_Group.Accel_Visible );


      Gtk.Table.Gtk_New( Widget => Win.Table,
                         Rows => 2, Columns => 2,
                         Homogeneous => False );
      Add( Win, Win.Table );
      Gtk.Table.Set_Row_Spacing( Win.Table, 1, 100 );

      Gtk.Table.Attach( Win.Table, Win.Menu_Bar, 0, 2, 0, 1 );

      Gtk.Frame.Gtk_New( Frame );
      Gtk.Frame.Set_Shadow_Type( Frame, Gtk.Enums.Shadow_In );
      Gtk.Frame.Set_Border_Width( Frame, 10 );

      Double_Buffer.Gtk_New( Win.Game_Screen );
      Gtk.Frame.Add( Frame, Win.Game_Screen );
      Size( Win.Game_Screen, 240, 480 );
      Gtk.Table.Attach( Win.Table, Frame, 0, 1, 1, 2 );

      Gtk.Box.Gtk_New_VBox( Win.Box );
      Gtk.Table.Attach( Win.Table, Win.Box, 1, 2, 1, 2, XPadding => 10);

      Double_Buffer.Gtk_New( Win.Prev_Screen );
      Size( Win.Prev_Screen, 85, 85 );
      Gtk.Frame.Gtk_New( Frame );
      Gtk.Frame.Set_Shadow_Type( Frame, Gtk.Enums.Shadow_In );
      Gtk.Frame.Add( Frame, Win.Prev_Screen );
      Gtk.Alignment.Gtk_New( Alg, 0.5, 0.5, 1.0, 0.0 );
      Gtk.Alignment.Add( Alg, Frame );
      Gtk.Box.Pack_Start( Win.Box, Alg );

      Gtk.Label.Gtk_New( lbl, "Score" );
      Gtk.Label.Set_Alignment( lbl, 0.5, 1.0 );
      Gtk.Box.Pack_Start( Win.Box, lbl );
      Gtk.Label.Set_Size_Request( lbl, 85);
      Gtk.Label.Gtk_New( Win.Score_Label, "0" );
      Gtk.Label.Set_Alignment( Win.Score_Label, 0.5, 0.0 );
      Gtk.Frame.Gtk_New( Frame );
      Gtk.Frame.Set_Shadow_Type( Frame, Gtk.Enums.Shadow_In );
      Gtk.Frame.Add( Frame, Win.Score_Label );
      Gtk.Alignment.Gtk_New( Alg, 0.5, 0.0, 1.0, 0.0 );
      Gtk.Alignment.Add( Alg, Frame );
      Gtk.Box.Pack_Start( Win.Box, Alg );

      Gtk.Label.Gtk_New( lbl, "Level" );
      Gtk.Box.Pack_Start( Win.Box, lbl );
      Gtk.Label.Set_Alignment( lbl, 0.5, 1.0 );
      Gtk.Label.Set_USize( Lbl, 85, -1 );
      Gtk.Label.Gtk_New( Win.Level_Label, "1" );
      Gtk.Label.Set_Alignment( Win.Level_Label, 0.5, 0.0 );
      Gtk.Frame.Gtk_New( Frame );
      Gtk.Frame.Set_Shadow_Type( Frame, Gtk.Enums.Shadow_In );
      Gtk.Frame.Add( Frame, Win.Level_Label );
      Gtk.Alignment.Gtk_New( Alg, 0.5, 0.0, 1.0, 0.0 );
      Gtk.Alignment.Add( Alg, Frame );
      Gtk.Box.Pack_Start( Win.Box, Alg );

      Gtk.Label.Gtk_New( lbl, "Lines" );
      Gtk.Box.Pack_Start( Win.Box, lbl );
      Gtk.Label.Set_Alignment( lbl, 0.5, 1.0 );
      Gtk.Label.Set_Size_Request( lbl, 85);
      Gtk.Label.Gtk_New( Win.Lines_Label, "0" );
      Gtk.Label.Set_Alignment( Win.Lines_Label, 0.5, 0.0 );
      Gtk.Frame.Gtk_New( Frame );
      Gtk.Frame.Set_Shadow_Type( Frame, Gtk.Enums.Shadow_In );
      Gtk.Frame.Add( Frame, Win.Lines_Label );
      Gtk.Alignment.Gtk_New( Alg, 0.5, 0.0, 1.0, 0.0 );
      Gtk.Alignment.Add( Alg, Frame );
      Gtk.Box.Pack_Start( Win.Box, Alg );


      Gtk.Label.Gtk_New( lbl, "Pieces" );
      Gtk.Box.Pack_Start( Win.Box, lbl );
      Gtk.Label.Set_Alignment( lbl, 0.5, 1.0 );
      Gtk.Label.Set_USize( Lbl, 85, Gint(Gtk.Label.Get_Allocation_Height( Lbl )) );
      Gtk.Label.Gtk_New( Win.Pieces_Label, "0" );
      Gtk.Label.Set_Alignment( Win.Pieces_Label, 0.5, 0.0 );
      Gtk.Frame.Gtk_New( Frame );
      Gtk.Frame.Set_Shadow_Type( Frame, Gtk.Enums.Shadow_In );
      Gtk.Frame.Add( Frame, Win.Pieces_Label );
      Gtk.Alignment.Gtk_New( Alg, 0.5, 0.0, 1.0, 0.0 );
      Gtk.Alignment.Add( Alg, Frame );
      Gtk.Box.Pack_Start( Win.Box, Alg );

      Gtk.Label.Gtk_New( lbl, "Record" );
      Gtk.Box.Pack_Start( Win.Box, lbl );
      Gtk.Label.Set_Size_Request( lbl, 85);
      Gtk.Label.Set_USize( Lbl, 85, -1 );
      Gtk.Label.Gtk_New( Win.Record_Label,
                         Integer'Image( Scores_Window_Pkg.Get_Maximum_Score ) );
      Gtk.Label.Set_Alignment( Win.Record_Label, 0.5, 0.0 );
      Gtk.Frame.Gtk_New( Frame );
      Gtk.Frame.Set_Shadow_Type( Frame, Gtk.Enums.Shadow_In );
      Gtk.Frame.Add( Frame, Win.Record_Label );
      Gtk.Alignment.Gtk_New( Alg, 0.5, 0.0, 1.0, 0.0 );
      Gtk.Alignment.Add( Alg, Frame );
      Gtk.Box.Pack_Start( Win.Box, Alg );

      Set_Title( Win, "LinXtris" );
      Set_Resizable( Win, False );
      Set_Position( Win, Gtk.Enums.Win_Pos_Center_Always );

      Main_Window_Return_Callback.Connect( Win, "delete_event",
                                           Main_Window_Return_Callback.To_Marshaller (On_Main_Window_Delete'Access) );

      Main_Window_Callback.Connect( Win, "destroy",
                                    Main_Window_Callback.To_Marshaller (On_Main_Window_Destroy'Access) );

      Main_Window_Return_Callback.Connect( Win, "key_press_event",
                                           Main_Window_Return_Callback.To_Marshaller( On_Main_Window_Key_Pressed'Access ) );

      Menu_Item_Callback.Connect( Win.Item_Game_Quit, "activate",
                                  Menu_Item_Callback.To_Marshaller( On_Item_Game_Quit_Pressed'Access ) );

      Menu_Item_Callback.Connect( Win.Item_Game_New, "activate",
                                  Menu_Item_Callback.To_Marshaller( On_Item_Game_New_Pressed'Access ) );

      Menu_Item_Callback.Connect( Win.Item_Game_Pause, "activate",
                                  Menu_Item_Callback.To_Marshaller( On_Item_Game_Pause_Pressed'Access ) );

      Menu_Item_Callback.Connect( Win.Item_Game_Scores, "activate",
                                  Menu_Item_Callback.To_Marshaller( On_Item_Game_Scores_Pressed'Access ) );

      Menu_Item_Callback.Connect( Win.Item_Settings_Preferences, "activate",
                                  Menu_Item_Callback.To_Marshaller( On_Item_Settings_Preferences_Pressed'Access ) );

      Menu_Item_Callback.Connect( Win.Item_Help_About, "activate",
                                  Menu_Item_Callback.To_Marshaller( On_Item_Help_About_Pressed'Access ) );

      Initialize_Pixmaps( Win );

      Gdkw := Get_Window( Win );
      Gdk.GC.Gdk_New( Win.GC, Gdkw );
   end Initialize;

   procedure Initialize_Pixmaps( Win: access Main_Window_Record'Class ) is
      Gdkw: Gdk.Gdk_Window;
      MaskBmp: Gdk.Bitmap.Gdk_Bitmap := Gdk.Bitmap.Null_Bitmap;
      Xpm_Name: Unbounded_String;
   begin
      Realize( Win );
      Gdkw := Get_Window( Win );

      if Argument_Count = 2 then
         if Argument( 1 ) = "-data_dir" then
            Data_Dir := To_Unbounded_String( Argument( 2 ) );
         end if;
      end if;

      Xpm_Name := Data_Dir & "pixmaps/blue.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Blue_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/green.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Green_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/red.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Red_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/yellow.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Yellow_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/grey.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Grey_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/blank.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Blank_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/cyan.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Cyan_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/magenta.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Magenta_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/ghost.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Ghost_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/white.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.White_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );

      Xpm_Name := Data_Dir & "pixmaps/blue_prev.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Blue_Prev_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/green_prev.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Green_Prev_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/red_prev.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Red_Prev_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/yellow_prev.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Yellow_Prev_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/grey_prev.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Grey_Prev_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/cyan_prev.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Cyan_Prev_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/magenta_prev.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Magenta_Prev_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
      Xpm_Name := Data_Dir & "pixmaps/blank_prev.xpm";
      Gdk.Pixmap.Create_From_Xpm( Win.Blank_Prev_Pix, Gdkw,
                                  MaskBmp,
                                  Gdk.Color.Null_Color, To_String(Xpm_Name) );
   end Initialize_Pixmaps;

end Main_Window_Pkg;
