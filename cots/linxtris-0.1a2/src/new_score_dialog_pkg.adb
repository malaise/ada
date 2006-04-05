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
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Enums;
with Main_Window_Pkg; use Main_Window_Pkg;
with Gtk.Handlers;
with Gtk.Button; use Gtk.Button;
with Gdk.Event; use Gdk.Event;
with Gtk.Stock;
with Gtk.Alignment; use Gtk.Alignment;
with Gtk.Label; use Gtk.Label;
with Scores_Window_Pkg;
with Block_Engine;
with Gtk.Table; use Gtk.Table;

package body New_Score_Dialog_Pkg is

   package Callbacks is

      package New_Score_Dlg_Callback is new Gtk.Handlers.Callback
        (Widget_Type => New_Score_Dialog_Record);
      package New_Score_Dlg_Return_Callback is new Gtk.Handlers.Return_Callback
        (Widget_Type => New_Score_Dialog_Record,
         Return_Type => Boolean);
      package Button_Callback is new Gtk.Handlers.Callback
        ( Widget_Type => Gtk_Button_Record );
      function On_New_Score_Dialog_Delete (Win: access New_Score_Dialog_Record'Class;
                                           Event: Gdk_Event) return Boolean;
      procedure On_OK_Button_Clicked( Button: access Gtk_Button_Record'Class );
      procedure On_New_Score_Dialog_Show( Win: access New_Score_Dialog_Record'Class );
      procedure On_New_Score_Dialog_Hide( Win: access New_Score_Dialog_Record'Class );

   end Callbacks;

   package body Callbacks is

      function On_New_Score_Dialog_Delete (Win: access New_Score_Dialog_Record'Class;
                                           Event: Gdk_Event) return Boolean is
         use Scores_Window_Pkg;
      begin
         Hide( Win );
         Set_Score( Get_Text( Win.Name_Entry ),
                    Block_Engine.Get_Score( Main_Window.Engine.All ) );
         return False;
      end;

      procedure On_OK_Button_Clicked( Button: access Gtk_Button_Record'Class ) is
         use Scores_Window_Pkg;
      begin
         Hide( New_Score_Dialog );
         Set_Score( Get_Text( New_Score_Dialog.Name_Entry ),
                    Block_Engine.Get_Score( Main_Window.Engine.All ) );
         Show_All( Scores_Win );
      end;

      procedure On_New_Score_Dialog_Show( Win: access New_Score_Dialog_Record'Class ) is
      begin
         Main_Window.Game_Paused := True;
      end;

      procedure On_New_Score_Dialog_Hide( Win: access New_Score_Dialog_Record'Class ) is
      begin
         Main_Window.Game_Paused := False;
      end;

   end Callbacks;

   procedure Gtk_New( Win: out New_Score_Dialog_Type )is
   begin
      Win := new New_Score_Dialog_Record;
      Initialize( Win );
   end;

   procedure Initialize( Win: access New_Score_Dialog_Record'Class ) is
      use Callbacks;
      Alg: Gtk_Alignment;
      Table: Gtk_Table;
      Lbl: Gtk_Label;
   begin
      Gtk.Window.Initialize( Win, Gtk.Enums.Window_Toplevel );
      Set_Title( Win, "Top Ten Score" );
      Set_Transient_For( Win, Main_Window_Pkg.Main_Window );
      Set_Resizable( Win, False );
      Set_Position( Win, Gtk.Enums.Win_Pos_Center_Always );

      Gtk_New( Table, 2, 2, False );
      Set_Col_Spacings( Table, 5 );
      Set_Row_Spacings( Table, 7 );
      Add( Win, Table );
      Set_Border_Width( Win, 7 );

      Gtk_New( lbl, "Name:" );
      Attach( Table, lbl, 0, 1, 0, 1 );

      Gtk_New( Win.Name_Entry );
      Set_Max_Length( Win.Name_Entry, 20 );
      Attach( Table, Win.Name_Entry, 1, 2, 0, 1  );

      Gtk_New( Alg, 1.0, 0.5, 0.0, 0.0 );
      Gtk_New_From_Stock( Win.OK_Button, Gtk.Stock.Stock_OK );
      Set_Size_Request( Win.OK_Button, 80 );
      Add( Alg, Win.OK_Button );
      Attach( Table, Alg, 0, 2, 1, 2 );

      Button_Callback.Connect( Win.OK_Button, "clicked",
                               Button_Callback.To_Marshaller( On_OK_Button_Clicked'Access ) );
      New_Score_Dlg_Callback.Connect( Win, "show",
                                      New_Score_Dlg_Callback.To_Marshaller( On_New_Score_Dialog_Show'Access ) );
      New_Score_Dlg_Callback.Connect( Win, "hide",
                                      New_Score_Dlg_Callback.To_Marshaller( On_New_Score_Dialog_Hide'Access ) );
      New_Score_Dlg_Return_Callback.Connect( Win, "delete_event",
                                             New_Score_Dlg_Return_Callback.To_Marshaller( On_New_Score_Dialog_Delete'Access ) );
   end;



end New_Score_Dialog_Pkg;
