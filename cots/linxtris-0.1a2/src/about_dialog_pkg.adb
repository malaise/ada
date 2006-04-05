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
with Gtk.Button; use Gtk.Button;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Label; use Gtk.Label;
with Gtk.Box; use Gtk.Box;
with Main_Window_Pkg; use Main_Window_Pkg;
with Gtk.Alignment; use Gtk.Alignment;
with Gtk.Stock;
with Gtk.Enums;
with Gtk.Handlers;
with Gdk.Event; use Gdk.Event;
with Pango.Font; use Pango.Font;

package body About_Dialog_Pkg is

   package Callbacks is

      package About_Dialog_Callback is new Gtk.Handlers.Callback
        (Widget_Type => About_Dialog_Record);
      package About_Dialog_Return_Callback is new Gtk.Handlers.Return_Callback
        (Widget_Type => About_Dialog_Record,
         Return_Type => Boolean);
      package Button_Callback is new Gtk.Handlers.Callback
        ( Widget_Type => Gtk_Button_Record );
      function On_About_Dialog_Delete (Win: access About_Dialog_Record'Class;
                                       Event: Gdk_Event) return Boolean;
      procedure On_OK_Button_Clicked( Button: access Gtk_Button_Record'Class );
      procedure On_About_Dialog_Show( Win: access About_Dialog_Record'Class );
      procedure On_About_Dialog_Hide( Win: access About_Dialog_Record'Class );

   end Callbacks;

   package body Callbacks is

      function On_About_Dialog_Delete (Win: access About_Dialog_Record'Class;
                                       Event: Gdk_Event) return Boolean is
      begin
         Hide( Win );
         return False;
      end;

      procedure On_OK_Button_Clicked( Button: access Gtk_Button_Record'Class ) is
      begin
         Hide( About_Dialog );
      end;

      procedure On_About_Dialog_Show( Win: access About_Dialog_Record'Class ) is
      begin
         Win.pause_status := Main_Window.Game_Paused;
         Main_Window.Game_Paused := True;
      end;

      procedure On_About_Dialog_Hide( Win: access About_Dialog_Record'Class ) is
      begin
         Main_Window.Game_Paused := Win.Pause_Status;
      end;

   end Callbacks;


   procedure Gtk_New( Win: out About_Dialog_Type ) is
   begin
      Win := new About_Dialog_Record;
      Initialize( Win );
   end;

   procedure Initialize( Win: access About_Dialog_Record'Class ) is
      Sep: Gtk_Separator;
      Lbl: Gtk_Label;
      Box: Gtk_Box;
      Alg: Gtk_Alignment;
      use Callbacks;
   begin
      Gtk.Window.Initialize( Win, Gtk.Enums.Window_Toplevel );
      Set_Title( Win, "About LinXtris" );
      Set_Transient_For( Win, Main_Window_Pkg.Main_Window );
      Set_Resizable( Win, False );
      Set_Position( Win, Gtk.Enums.Win_Pos_Center_Always );
      Set_Border_Width( Win, 10 );

      Gtk_New_VBox( Box );
      Set_Spacing( Box, 10 );
      Add( Win, Box );

      Gtk_New( Alg, 0.5, 0.5, 0.0, 0.0 );
      Gtk_New( Lbl, "LinXtris 0.1a2" );
      Modify_Font( Lbl, From_String("Bold 18") );
      Add( Alg, Lbl );
      Pack_Start( Box, Alg );

      Gtk_New( Alg, 0.5, 0.5, 0.0, 0.0 );
      Gtk_New( Lbl, "Send comments to: dulio@users.sourceforge.net" );
      Add( Alg, Lbl );
      Pack_Start( Box, Alg );

      Gtk_New( Alg, 0.5, 0.5, 0.0, 0.0 );
      Gtk_New( Lbl, "Copyright (C) 2003 Dulio Matos Leite de C. e Silva" );
      Modify_Font( Lbl, From_String("8") );
      Add( Alg, Lbl );
      Pack_Start( Box, Alg );

      Gtk_New_HSeparator( Sep );
      Pack_Start( Box, Sep );

      Gtk_New_From_Stock( Win.OK_Button, Gtk.Stock.Stock_OK );
      Set_Size_Request( Win.OK_Button, 80 );
      Gtk_New( Alg, 1.0, 0.0, 0.0, 0.0 );
      Add( Alg, Win.OK_Button );
      Pack_Start( Box, Alg );

      Button_Callback.Connect( Win.OK_Button, "clicked",
                               Button_Callback.To_Marshaller( On_OK_Button_Clicked'Access ) );
      About_Dialog_Callback.Connect( Win, "show",
                                     About_Dialog_Callback.To_Marshaller( On_About_Dialog_Show'Access ) );
      About_Dialog_Callback.Connect( Win, "hide",
                                     About_Dialog_Callback.To_Marshaller( On_About_Dialog_Hide'Access ) );
      About_Dialog_Return_Callback.Connect( Win, "delete_event",
                                            About_Dialog_Return_Callback.To_Marshaller( On_About_Dialog_Delete'Access ) );
   end;

end About_Dialog_Pkg;
