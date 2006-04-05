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

with Ada.Text_IO; use Ada.Text_IO;
with Gtk.Main; use Gtk.Main;
with Main_Window_Pkg; use Main_Window_Pkg;
with Gtk.Widget;
with Gtk;
with Game_Engine;
with Block_Engine;
with Double_Buffer;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gdk.Types; use Gdk.Types;
with Gdk.Event;
with Glib; use Glib;
with Preferences_Window_pkg; use Preferences_Window_pkg;
with Scores_Window_Pkg; use Scores_Window_Pkg;
with About_Dialog_Pkg; use About_Dialog_Pkg;


package body Main_Window_Pkg.Callbacks is

   function On_Main_Window_Delete (Win: access Main_Window_Record'Class;
                                   Event: Gdk_Event) return Boolean is
   begin
      return False;
   end;

   procedure On_Main_Window_Destroy (Win: access Main_Window_Record'Class) is
   begin
      Gtk.Main.Main_Quit;
   end;

   procedure On_Item_Game_Quit_Pressed (Item: access Gtk_Menu_Item_Record'Class) is
   begin
      Gtk.Main.Main_Quit;
   end;

   function On_Main_Window_Key_Pressed( Win: access Main_Window_Record'Class;
                                        Event: Gdk.Event.Gdk_Event_Key ) return Boolean is
   begin
      return Game_Engine.Process_Key_Press( Win, Event );
   end;

   procedure On_Item_Game_New_Pressed( Item: access Gtk_Menu_Item_Record'Class ) is
   begin
      Game_Engine.New_Game;
   end;

   procedure On_Item_Game_Pause_Pressed( Item: access Gtk_Menu_Item_Record'Class ) is
   begin
      if Main_Window.Exists_Game then
         Main_Window.Game_Paused := not Main_Window.Game_Paused;
      end if;
   end;

   procedure On_Item_Settings_Preferences_Pressed( Item: access Gtk_Menu_Item_Record'Class ) is
   begin
      Show_All( Pref_Win );
   end;

   procedure On_Item_Help_About_Pressed( Item: access Gtk_Menu_Item_Record'Class ) is
   begin
      Show_All( About_Dialog );
   end;

   procedure On_Item_Game_Scores_Pressed( Item: access Gtk_Menu_Item_Record'Class ) is
      pause_status: Boolean;
   begin
      Show_All( Scores_Win );
   end;

end Main_Window_Pkg.Callbacks;
