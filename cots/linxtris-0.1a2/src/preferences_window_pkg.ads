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
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Button; use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Ada.Direct_IO;

package Preferences_Window_Pkg is

   type Preferences_Type is record
      Initial_Level: Positive := 1;
      Initial_Lines: Natural := 0;
      Piece_Preview: Boolean := True;
      Continuous_Movement: Boolean := True;
      Animation: Boolean := True;
      Ghost: Boolean := False;
   end record;

   package Preferences_IO is new Ada.Direct_IO( Preferences_Type );

   type Preferences_Window_Record is new Gtk_Window_Record with
      record
         Level_Spin: Gtk_Spin_Button;
         Lines_Spin: Gtk_Spin_Button;
         OK_Button: Gtk_Button;
         Preview_Check: Gtk_Check_Button;
         Cont_Move_Check: Gtk_Check_Button;
         Animation_Check: Gtk_Check_Button;
         Ghost_Check: Gtk_Check_Button;
         Pause_Status: Boolean;
      end record;

   type Preferences_Window is access all Preferences_Window_Record'Class;

   Pref_Win: Preferences_Window;

   procedure Gtk_new( Win: out Preferences_Window );
   procedure Initialize( Win: access Preferences_Window_Record'Class );
   procedure Set_Preferences;
   procedure Get_Preferences;
end Preferences_Window_Pkg;
