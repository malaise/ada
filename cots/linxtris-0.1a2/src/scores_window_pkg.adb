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
with Gtk.Separator; use Gtk.Separator;
with Gtk.Stock;
with Gtk.Alignment; use Gtk.Alignment;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Table; use Gtk.Table;
with Gtk.Enums;
with Main_Window_Pkg;
with Gtk.Frame; use Gtk.Frame;
with Glib; use Glib;
with Main_Window_Pkg; use Main_Window_Pkg;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Button; use Gtk.Button;
with Gdk.Event; use Gdk.Event;
With Gtk.Widget; Use Gtk.Widget;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.IO_Exceptions;

package body Scores_Window_Pkg is

   package Callbacks is

      package Scores_Window_Callback is new Gtk.Handlers.Callback
        (Widget_Type => Scores_Window_Record);
      package Scores_Window_Return_Callback is new Gtk.Handlers.Return_Callback
        (Widget_Type => Scores_Window_Record,
         Return_Type => Boolean);
      package Button_Callback is new Gtk.Handlers.Callback
        ( Widget_Type => Gtk_Button_Record );
      function On_Scores_Window_Delete (Win: access Scores_Window_Record'Class;
                                        Event: Gdk_Event) return Boolean;
      procedure On_OK_Button_Clicked( Button: access Gtk_Button_Record'Class );
      procedure On_Scores_Window_Show( Win: access Scores_Window_Record'Class );
      procedure On_Scores_Window_Hide( Win: access Scores_Window_Record'Class );

   end Callbacks;

   package body Callbacks is

      function On_Scores_Window_Delete (Win: access Scores_Window_Record'Class;
                                        Event: Gdk_Event) return Boolean is
      begin
         Hide( Win );
         return False;
      end;

      procedure On_OK_Button_Clicked( Button: access Gtk_Button_Record'Class ) is
      begin
         Hide( Scores_Win );
      end;

      procedure On_Scores_Window_Show( Win: access Scores_Window_Record'Class ) is
      begin
         Win.pause_status := Main_Window.Game_Paused;
         Main_Window.Game_Paused := True;
         Get_Scores;
      end;

      procedure On_Scores_Window_Hide( Win: access Scores_Window_Record'Class ) is
      begin
         Main_Window.Game_Paused := Win.Pause_Status;
      end;

   end Callbacks;

   procedure Gtk_New( Win: out Scores_Window ) is
   begin
      Win := new Scores_Window_Record;
      Initialize( Win );
   end Gtk_New;

   procedure Initialize( Win: access Scores_Window_Record'Class ) is
      use Callbacks;
      lbl: Gtk_Label;
      Table: Gtk_Table;
      alg: Gtk_Alignment;
      frame: Gtk_Frame;
   begin
      Gtk.Window.Initialize( Win, Gtk.Enums.Window_Toplevel );
      Set_Title( Win, "Scores" );
      Set_Transient_For( Win, Main_Window_Pkg.Main_Window );
      Set_Resizable( Win, False );
      Set_Position( Win, Gtk.Enums.Win_Pos_Center_Always );

      Gtk_New( Table, 13, 2, False );
      Set_Border_Width( Table, 10 );
      Set_Row_Spacings( Table, 7 );
      Set_Col_Spacings( Table, 15 );

      Gtk_New( lbl, "Name" );
      Attach( Table, lbl, 0, 1, 0, 1 );

      Gtk_New( lbl, "Score" );
      Attach( Table, lbl, 1, 2, 0, 1 );

      Add( Win, Table );
      for i in Win.Scores_Label'Range loop
         Gtk_New( Win.Scores_Label(i), "0" );
         Set_Size_Request( Win.Scores_Label(i), 80 );
         Gtk_New( Frame );
         Gtk.Frame.Set_Shadow_Type( Frame, Gtk.Enums.Shadow_In );
         Add( Frame, Win.Scores_Label(i) );
         Attach( Table, Frame, 1, 2, Guint(i), Guint(i+1) );
      end loop;
      for i in Win.Names_Label'Range loop
         Gtk_New( Win.Names_Label(i), "no user" );
         Set_Justify( Win.Names_Label(i), Gtk.Enums.Justify_Left );
         Set_Size_Request( Win.Names_Label(i), 120 );
         Gtk_New( Frame );
         Gtk.Frame.Set_Shadow_Type( Frame, Gtk.Enums.Shadow_In );
         Add( Frame, Win.Names_Label(i) );
         Attach( Table, Frame, 0, 1, Guint(i), Guint(i+1) );
      end loop;

      Gtk_New_From_Stock( Win.OK_Button, Gtk.Stock.Stock_OK );
      Set_Size_Request( Win.OK_Button, 80 );
      Gtk_New( Alg, 1.0, 0.5, 0.0, 0.0 );
      Add( Alg, Win.OK_Button );
      Attach( Table, Alg, 0, 2, 12, 13, Ypadding => 5 );

      Button_Callback.Connect( Win.OK_Button, "clicked",
                               Button_Callback.To_Marshaller( On_OK_Button_Clicked'access ) );
      Scores_Window_Callback.Connect( Win, "show",
                                      Scores_Window_Callback.To_Marshaller( On_Scores_Window_Show'access ) );

      Scores_Window_Callback.Connect( Win, "hide",
                                      Scores_Window_Callback.To_Marshaller( On_Scores_Window_Hide'access ) );

   end Initialize;

   function Get_Maximum_Score return Natural is
      use Scores_IO;
      Filename: String := To_String( Main_Window_Pkg.Data_Dir & "scores" );
      File: File_Type;
      scr: Score_Type;
   begin
      Open( File, In_File, Filename );
      Read( File, scr );
      Close( File );
      return scr.Score;
   exception
      when Name_Error => return 0;
   end Get_Maximum_Score;

   function Get_Minimum_Score return Natural is
   begin
      Get_Scores;
      return Minimum_Score;
   end Get_Minimum_Score;

   procedure Set_Score( Name: String; Score: Natural ) is
      use Scores_IO;
      Filename: String := To_String( Main_Window_Pkg.Data_Dir & "scores" );
      File: File_Type;
      scr: Score_Type;
      I: Scores_IO.Count := 10;
      J: Scores_IO.Count;
   begin
      Open( File, InOut_File, Filename );
      loop
         Set_Index( File, Scores_IO.Count(I) );
         Read( File, scr );
         if (scr.Score > Score) or I = 1 then
            if I > 1 then
               I := I + 1;
            end if;
            if I < 10 then
               J := 10;
               while J > I loop
                  Set_Index( File, Scores_IO.Count(J-1) );
                  Read( File, Scr );
                  Set_Index( File, Scores_IO.Count(J) );
                  Write( File, Scr );
                  J := J - 1;
               end loop;
            end if;
            scr.Score := Score;
            if Name'Length > scr.Name'Length then
               for i in scr.Name'Range loop
                  scr.Name(i) := Name(i);
               end loop;
            else
               for i in Name'Range loop
                  scr.Name(i) := Name(i);
               end loop;
               for i in Name'Last+1..scr.Name'Last loop
                  scr.Name(i) := ' ';
               end loop;
            end if;
            Set_Index( File, Scores_IO.Count(I) );
            Write( File, scr );
            exit;
         end if;
         I := I - 1;
         exit when I = 0;
      end loop;
      Close( File );
      Get_Scores;
   end Set_Score;

   procedure Get_Scores is
      use Scores_IO;
      Filename: String := To_String( Main_Window_Pkg.Data_Dir & "scores" );
      File: File_Type;
      scr: Score_Type;
      procedure Create_File is
      begin
         Create( File, Out_File, Filename );
         scr.Name := "no user             ";
         scr.Score := 0;
         for i in 1..10 loop
            Write( File, scr );
         end loop;
         Close( File );
      end Create_File;
   begin
      Open( File, In_File, Filename );
      for i in 1..10 loop
         Read( File, scr );
         Set_Text( Scores_Win.Names_Label(i), scr.Name );
         Set_Text( Scores_Win.Scores_Label(i), Integer'Image(scr.Score) );
      end loop;
      Minimum_Score := scr.Score;
      Close( File );
   exception
      when Name_Error => Create_File;

   end Get_Scores;

end Scores_Window_pkg;
