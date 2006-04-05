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

with Block_Engine;
with Gdk.Pixmap;
with Main_Window_Pkg; use Main_Window_Pkg;
with GLib;
with Gdk.Event;

package Game_Engine is
   procedure Clear_Screen;
   procedure Clear_Prev;
   procedure Paint_Prev;
   procedure New_Game;
   function Process_Key_Press( Win: access Main_Window_Record'Class;
                                Event: Gdk.Event.Gdk_Event_Key ) return Boolean;
private
   procedure Paint_Screen;
   procedure Paint_Block( X: Integer; Y: Integer; Color: Block_Engine.Color );
   function X_Drawing_Coordinate( X: Integer ) return GLib.Gint;
   function Y_Drawing_Coordinate( Y: Integer ) return GLib.Gint;
   function Color_To_Pix( Color: Block_Engine.Color ) return Gdk.Pixmap.Gdk_Pixmap;
   procedure Process_And_Draw;
   procedure Do_Animation;
   procedure Do_Line_Completed_Animation;
   procedure Do_Game_Over_Animation;
   function Auto_Down_Timeout return Boolean;
end Game_Engine;
