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
with Main_Window_Pkg.Callbacks; use Main_Window_Pkg.Callbacks;
with Gdk;
with Gdk.GC;
with Gdk.Drawable;
with Double_Buffer; use Double_Buffer;
with GLib;
with Gtk.Label;
with Ada.Strings; use Ada.Strings;
with Gtk.Main; use Gtk.Main;
with Gtk.Menu_Item;
with Scores_Window_Pkg; use Scores_Window_Pkg;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gdk.Types; use Gdk.Types;
with Gdk.Event; use Gdk.Event;
with Gtk.Menu_Item;
with Glib; use Glib;
with New_Score_Dialog_Pkg;
with Preferences_Window_Pkg;

package body Game_Engine is

   procedure Clear_Screen is
   begin
      for i in 1..10 loop
         for j in 1..20 loop
            Paint_Block(i, j, Block_Engine.Blank );
         end loop;
      end loop;

   end;

   procedure Clear_Prev is
      Gdkw: Gdk.Gdk_Window;
   begin
      Gdkw := Get_Window( Main_Window );
      Gdk.Drawable.Draw_Drawable( Get_Pixmap( Main_Window.Prev_Screen ),
                                  Main_Window.GC, Main_Window.Blank_Prev_Pix,
                                  0, 0,
                                  0, 0,
                                  85, 85
                                );
      Double_Buffer.Draw( Main_Window.Prev_Screen );
   end;

   procedure Paint_Piece_Block(X: Integer; Y: Integer; Color: Block_Engine.Color ) is
      Gdkw: Gdk.Gdk_Window;
   begin
      if Y > 20 then
         return;
      end if;
      Gdkw := Get_Window( Main_Window );
      Gdk.Drawable.Draw_Drawable( Get_Pixmap( Main_Window.Game_Screen ),
                                  Main_Window.GC, Color_To_Pix(Color),
                                  0, 0,
                                  X_Drawing_Coordinate(X),
                                  Y_Drawing_Coordinate(Y) + Gint(Main_Window.Delta_Y),
                                  24, 24
                                );
   end Paint_Piece_Block;

   procedure Paint_Piece is
      use Block_Engine;
   begin
      if Main_Window.Continuous_Movement then
         Main_Window.Delta_Y := (24*Main_Window.Down_Count)/Main_Window.Max_Down_Count - 24;
         for I in 1..4 loop
            if Block_Engine.Piece_Y( Main_Window.Engine.all, i ) < 20 then
               if Block_Engine.Get_Position_Color( Main_Window.Engine.all, Block_Engine.Piece_X( Main_Window.Engine.all, i ),
                                                   Block_Engine.Piece_Y( Main_Window.Engine.all, i ) + 1) /= Block_Engine.Blank then
                  Main_Window.Delta_Y := 0;
                  exit;
               end if;
            end if;
         end loop;
      else
         Main_Window.Delta_Y := 0;
      end if;
      for i in 1..4 loop
         Paint_Piece_Block( Block_Engine.Piece_X( Main_Window.Engine.All, i ),
                            Block_Engine.Piece_Y( Main_Window.Engine.All, i ),
                            Block_Engine.Get_Piece_Color( Main_Window.Engine.All ) );
      end loop;
   end;

   procedure Paint_Screen is
   begin
      for i in 1..10 loop
         for j in 1..20 loop
            Paint_Block(i, j,
                        Block_Engine.Get_Position_Color(Main_Window.Engine.All, i, j) );
         end loop;
      end loop;
      if Block_Engine.Get_Has_Ghost( Main_Window.Engine.all ) then
         for i in  1..4 loop
            Paint_Block( Block_Engine.Ghost_X( Main_Window.Engine.All, i ),
                         Block_Engine.Ghost_Y( Main_Window.Engine.All, i ),
                         Block_Engine.Ghost );
         end loop;
      end if;
      Paint_Piece;
   end;

   procedure Paint_Block( X: Integer; Y: Integer; Color: Block_Engine.Color ) is
      Gdkw: Gdk.Gdk_Window;
   begin
      Gdkw := Get_Window( Main_Window );
      Gdk.Drawable.Draw_Pixmap( Get_Pixmap( Main_Window.Game_Screen ),
                                Main_Window.GC, Color_To_Pix(Color),
                                0, 0,
                                X_Drawing_Coordinate(X),
                                Y_Drawing_Coordinate(Y),
                                24, 24
                              );
   end Paint_Block;

   procedure Paint_White_Block( X: Integer; Y: Integer) is
      Gdkw: Gdk.Gdk_Window;
   begin
      Gdkw := Get_Window( Main_Window );
      Gdk.Drawable.Draw_Drawable( Get_Pixmap( Main_Window.Game_Screen ),
                                  Main_Window.GC, Main_Window.White_Pix,
                                  0, 0,
                                  X_Drawing_Coordinate(X),
                                  Y_Drawing_Coordinate(Y),
                                  24, 24
                                );
   end Paint_White_Block;


   function X_Drawing_Coordinate( X: Integer ) return GLib.Gint is
   begin
      return GLib.Gint( 24*(X-1) );
   end;

   function Y_Drawing_Coordinate( Y: Integer ) return GLib.Gint is
   begin
      return GLib.Gint( 480 - 24*Y );
   end;

   function Color_To_Pix( Color: Block_Engine.Color ) return Gdk.Pixmap.Gdk_Pixmap is
   begin
      case Color is
         when Block_Engine.Blue => return Main_Window.Blue_Pix;
         when Block_Engine.Red => return Main_Window.Red_Pix;
         when Block_Engine.Yellow => return Main_Window.Yellow_Pix;
         when Block_Engine.Grey => return Main_Window.Grey_Pix;
         when Block_Engine.Blank => return Main_Window.Blank_Pix;
         when Block_Engine.Green => return Main_Window.Green_Pix;
         when Block_Engine.Cyan => return Main_Window.Cyan_Pix;
         when Block_Engine.Magenta => return Main_Window.Magenta_Pix;
         when Block_Engine.Ghost => return Main_Window.Ghost_Pix;
      end case;
   end;

   function Color_To_Prev_Pix( Color: Block_Engine.Color ) return Gdk.Pixmap.Gdk_Pixmap is
   begin
      case Color is
         when Block_Engine.Blue => return Main_Window.Blue_Prev_Pix;
         when Block_Engine.Red => return Main_Window.Red_Prev_Pix;
         when Block_Engine.Yellow => return Main_Window.Yellow_Prev_Pix;
         when Block_Engine.Grey => return Main_Window.Grey_Prev_Pix;
         when Block_Engine.Blank => return Main_Window.Blank_Prev_Pix;
         when Block_Engine.Green => return Main_Window.Green_Prev_Pix;
         when Block_Engine.Cyan => return Main_Window.Cyan_Prev_Pix;
         when Block_Engine.Magenta => return Main_Window.Magenta_Prev_Pix;
         when Block_Engine.Ghost => return Main_Window.Blank_Prev_Pix;
      end case;
   end;

   procedure Paint_Prev is
      Gdkw: Gdk.Gdk_Window;
   begin
      if not Main_Window.Piece_Preview then
         return;
      end if;
      Gdkw := Get_Window( Main_Window );
      Gdk.Drawable.Draw_Drawable( Get_Pixmap( Main_Window.Prev_Screen ),
                                  Main_Window.GC,
                                  Color_To_Prev_Pix( Block_Engine.Get_Next_Piece_Color(Main_Window.Engine.all) ),
                                  0, 0,
                                  0, 0,
                                  85, 85
                                );
      Double_Buffer.Draw( Main_Window.Prev_Screen );
   end;

   procedure Terminate_Game is
      use New_Score_Dialog_Pkg;
   begin
      Gtk.Main.Timeout_Remove( Main_Window.Down_Timeout_ID );
      Main_Window.Exists_Game := False;
      Gtk.Menu_Item.Set_Sensitive( Main_Window.Item_Game_Pause, False );
      if Block_Engine.Get_Score( Main_Window.Engine.All ) > Get_Minimum_Score then
         Show_All( New_Score_Dialog );
      end if;
   end;

   procedure Process_And_Draw is
      Score, Level, Pieces, Lines: Integer;
   begin
      Block_Engine.Process_Move( Main_Window.Engine.All );
      Score := Block_Engine.Get_Score( Main_Window.Engine.All );
      Level := Block_Engine.Get_Level( Main_Window.Engine.All );
      Pieces := Block_Engine.Get_Pieces_Number( Main_Window.Engine.All );
      Lines := Block_Engine.Get_Lines_Number( Main_Window.Engine.All );
      Gtk.Label.Set_Text( Main_Window.Score_Label, Integer'Image(Score) );
      Gtk.Label.Set_Text( Main_Window.Level_Label, Integer'Image(Level) );
      Gtk.Label.Set_Text( Main_Window.Pieces_Label, Integer'Image(Pieces) );
      Gtk.Label.Set_Text( Main_Window.Lines_Label, Integer'Image(Lines) );
      if (Block_Engine.Get_Lines_Completed( Main_Window.Engine.all ) > 0) and MAin_Window.Animation then
         Main_Window.On_Animation := True;
         Main_Window.Animation_Kind := Line_Completed_Animation;
      end if;
      if Block_Engine.Level_Changed( Main_Window.Engine.All ) then
         Main_Window.Down_Count := 0;
         declare
            Tmp: Integer := Main_Window.Max_Down_Count;
         begin
            Main_Window.Max_Down_Count := Integer(Float(Main_Window.Max_Down_Count)/1.35);
            if Main_Window.Max_Down_Count = Tmp then
               Main_Window.Max_Down_Count := Main_Window.Max_Down_Count/2;
            end if;
         end;
      end if;
      Paint_Prev;
      Game_Engine.Paint_Screen;
      Double_Buffer.Draw( Main_Window.Game_Screen );
      if Block_Engine.Game_Over( Main_Window.Engine.All ) then
         Clear_Prev;
         if Main_Window.Animation then
            Main_Window.On_Animation := True;
            Main_Window.Animation_Kind := Game_Over_Animation;
         else
            Terminate_Game;
         end if;
      end if;
   end;

   function Auto_Down_Timeout return Boolean is
   begin
      if Main_Window.On_Animation then
         Do_Animation;
         return True;
      end if;
      if Main_Window.Game_Paused Then
         return True;
      end if;
      if Main_Window.Down_Count /= Main_Window.Max_Down_Count then
         Main_Window.Down_Count := Main_Window.Down_Count + 1;
         if Main_Window.Continuous_Movement then
            Game_Engine.Paint_Screen;
            Double_Buffer.Draw( Main_Window.Game_Screen );
         end if;
         return True;
      end if;
      Main_Window.Down_Count := 0;
      Block_Engine.Set_Auto_Down( Main_Window.Engine.All );
      Game_Engine.Process_And_Draw;
      return True;
   end;

   procedure Do_Animation is
   begin
      if Main_Window.Animation_Kind = Line_Completed_Animation then
         Do_Line_Completed_Animation;
      elsif Main_Window.Animation_Kind = Game_Over_Animation then
         Do_Game_Over_Animation;
      end if;
   end;

   procedure Do_Line_Completed_Animation is
      Base_Line, Lines: Natural;
   begin
      Base_Line := Block_Engine.Get_Lowest_Line_Completed( Main_Window.Engine.all );
      Lines := Block_Engine.Get_Lines_Completed( Main_Window.Engine.all );
      Main_Window.Animation_Count := Main_Window.Animation_Count + 1;
      if Main_Window.Animation_Count = 1 then
         for J in Base_Line..(Base_Line+Lines-1) loop
            for I in 1..10 loop
               Paint_White_Block( I, J );
            end loop;
         end loop;
         for j in Base_Line..20 loop
            for i in 1..10 loop
               Paint_Block(i, j + Lines,
                           Block_Engine.Get_Position_Color(Main_Window.Engine.All, i, j) );
            end loop;
         end loop;
         Double_Buffer.Draw( Main_Window.Game_Screen );
      elsif Main_Window.Animation_Count = 5 then
         for J in Base_Line..(Base_Line+Lines-1) loop
            for I in 1..10 loop
               Paint_Block( I, J, Block_Engine.Blank );
            end loop;
         end loop;
         Double_Buffer.Draw( Main_Window.Game_Screen );
       elsif Main_Window.Animation_Count = 10 then
          for J in Base_Line..(Base_Line+Lines-1) loop
             for I in 1..10 loop
                Paint_White_Block( I, J );
             end loop;
          end loop;
          Double_Buffer.Draw( Main_Window.Game_Screen );
      elsif Main_Window.Animation_Count = 15 then
         Main_Window.On_Animation := False;
         Main_Window.Animation_Count := 0;
         Paint_Screen;
         Double_Buffer.Draw( Main_Window.Game_Screen );
         return;
      end if;
   end;

   procedure Do_Game_Over_Animation is
   begin
      Main_Window.Animation_Count := Main_Window.Animation_Count + 1;
      if (Main_Window.Animation_Count rem 2) = 0 then
        return;
      end if;
      for I in 1..10 loop
         Paint_White_Block( I, (Main_Window.Animation_Count+1)/2 );
      end loop;
      if Main_Window.Animation_Count = 39 then
         Main_Window.On_Animation := False;
         Main_Window.Animation_Count := 0;
         Terminate_Game;
      end if;
      Double_Buffer.Draw( Main_Window.Game_Screen );
   end;


   procedure New_Game is
      Initial_Level: integer;
      use Preferences_Window_Pkg;
   begin
      Get_Preferences;
      Gtk.Menu_Item.Set_Sensitive( Main_Window.Item_Game_Pause );
      Main_Window.Exists_Game := True;
      Main_Window.Game_Paused := False;
      Game_Engine.Clear_Screen;
      Double_Buffer.Draw( Main_Window.Game_Screen );
      Block_Engine.Init( Main_Window.Engine.All );
      Game_Engine.Paint_Prev;
      Initial_Level := Block_Engine.Get_Initial_Level(Main_Window.Engine.all);

      Main_Window.Down_Count := 0;
      Main_Window.Max_Down_Count := 40;
      while Initial_Level > 1 loop
         Main_Window.Max_Down_Count := Integer( Float(Main_Window.Max_Down_Count)/1.35 );
         Initial_Level := Initial_Level - 1;
      end loop;
      Main_Window.Down_Timeout_Interval := 20;

      if Main_Window.Down_Timeout_ID /= 0 then
         Gtk.Main.Timeout_Remove( Main_Window.Down_Timeout_ID );
      end if;
      Main_Window.Down_Timeout_ID := Gtk.Main.Timeout_Add( Main_Window.Down_Timeout_Interval,
                                                           Auto_Down_Timeout'Access );
      Gtk.Label.Set_Text( Main_Window.Record_Label, Integer'Image(Scores_Window_Pkg.Get_Maximum_Score) );
   end;

   function Process_Key_Press( Win: access Main_Window_Record'Class;
                                Event: Gdk.Event.Gdk_Event_Key ) return Boolean is
      Key: Gdk.Types.Gdk_Key_type;
   begin
      if Main_Window.On_Animation then
         return True;
      end if;
      if Win.Game_Paused or (not Win.Exists_Game) Then
         return False;
      end if;
      Key := Gdk.Event.Get_Key_Val( Event );
      if Key = Gdk_Down then
            Block_Engine.Set_Next_Move( Win.Engine.All, Block_Engine.Down );
      elsif Key = Gdk_Up then
         Block_Engine.Set_Next_Move( Win.Engine.All, Block_Engine.Rotate );
      elsif Key = Gdk_Left then
         Block_Engine.Set_Next_Move( Win.Engine.All, Block_Engine.Left );
      elsif Key = Gdk_Right then
         Block_Engine.Set_Next_Move( Win.Engine.All, Block_Engine.Right );
      elsif Key = Gdk_Space then
            Block_Engine.Set_Next_Move( Win.Engine.All, Block_Engine.Drop );
      end if;
      Game_Engine.Process_And_Draw;
      return False;
   end;


end Game_Engine;
