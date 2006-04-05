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
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Table; use Gtk.Table;
with Double_Buffer; use Double_Buffer;
with Gdk.Pixmap; use Gdk.Pixmap;
with Gdk;
with Gtk.Main;
with Gdk.GC;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Frame;
with Block_Engine;
with Glib;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Main_Window_Pkg is

   type Engine_Access is access Block_Engine.Object;
   type Animation_Type is (Line_Completed_Animation, Game_Over_Animation);

   type Main_Window_Record is new Gtk_Window_Record with
      record
         Menu_Bar: Gtk_Menu_Bar;
         Game_Screen: Gtk_Double_Buffer;
         Prev_Screen: Gtk_Double_Buffer;
         Table: Gtk_Table;
         Item_Game: Gtk_Menu_Item;
         Item_Game_Submenu: Gtk_Menu;
         Item_Game_New: Gtk_Menu_Item;
         Item_Game_Pause: Gtk_Menu_Item;
         Item_Game_Sep1: Gtk_Menu_Item;
         Item_Game_Scores: Gtk_Menu_Item;
         Item_Game_Sep2: Gtk_Menu_Item;
         Item_Game_Quit: Gtk_Menu_Item;
         Item_Settings: Gtk_Menu_Item;
         Item_Settings_Submenu: Gtk_Menu;
         Item_Settings_Preferences: Gtk_Menu_Item;
         Item_Help: Gtk_Menu_Item;
         Item_Help_Submenu: Gtk_Menu;
         Item_Help_About: Gtk_Menu_Item;
         Box: Gtk.Box.Gtk_Box;
         Blue_Pix: Gdk_Pixmap;
         Green_Pix: Gdk_Pixmap;
         Red_Pix: Gdk_Pixmap;
         Grey_Pix: Gdk_Pixmap;
         Blank_Pix: Gdk_Pixmap;
         Yellow_Pix: Gdk_Pixmap;
         Magenta_Pix: Gdk_Pixmap;
         Cyan_Pix: Gdk_Pixmap;
         Ghost_Pix: Gdk_Pixmap;
         White_Pix: Gdk_Pixmap;
         Blue_Prev_Pix: Gdk_Pixmap;
         Green_Prev_Pix: Gdk_Pixmap;
         Red_Prev_Pix: Gdk_Pixmap;
         Grey_Prev_Pix: Gdk_Pixmap;
         Yellow_Prev_Pix: Gdk_Pixmap;
         Magenta_Prev_Pix: Gdk_Pixmap;
         Cyan_Prev_Pix: Gdk_Pixmap;
         Blank_Prev_Pix: Gdk_Pixmap;
         Score_Label: Gtk_Label;
         Level_Label: Gtk_Label;
         Pieces_Label: Gtk_Label;
         Lines_Label: Gtk_Label;
         Record_Label: Gtk_Label;
         Engine: Engine_Access;
         Game_Paused: Boolean := False;
         Exists_Game: Boolean := False;
         Continuous_Movement: Boolean := True;
         On_Animation: Boolean := False;
         Animation: Boolean := True;
         Animation_Count: Natural := 0;
         Animation_Kind: Animation_Type;
         Piece_Preview: Boolean := True;
         Down_Timeout_ID: Gtk.Main.Timeout_Handler_ID := 0;
         Down_Timeout_Interval: Glib.Guint32;
         Down_Count: Natural := 0;
         Delta_Y: Integer := 0;
         Max_Down_Count: Positive := 40;
         GC: Gdk.GC.Gdk_GC;
      end record;

   type Main_Window_Type is access all Main_Window_Record'Class;

   Main_Window: Main_Window_Type;
   Data_Dir: Unbounded_String := To_Unbounded_String("");

   procedure Gtk_New( Win: out Main_Window_Type );
   procedure Initialize( Win: access Main_Window_Record'Class );
private
   procedure Initialize_Pixmaps( Win: access Main_Window_Record'Class );
end Main_Window_Pkg;
