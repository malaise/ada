-- Mine Detector Game
-- Copyright (C) 2015 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- V6.2 2017 Mar 01 Add icon
-- V6.1 2015 Dec 01 Corrected initial levels selection
-- V6.1 2012 Aug 01 GTKAda changed
-- V6.0 2009 Aug 01
--
with Glib;
with Gdk.Event;
with Gtk.Arguments;
with Gtk.Box;
with Gtk.Window;
with Gtk.Button;
with Gtk.Toggle_Button;
with Gtk.Check_Button;
with Gtk.Dialog;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Main;
with Gtk.Scrolled_Window;
with Gtk.Table;
with Gtk.Combo_Box_Text;
with Gtk.Text_View;
with Gtk.Text_Buffer;
with Gtk.Text_Iter;
with Gtk.Check_Menu_Item;
with Gtkada.Dialogs;


with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;

with System;

with Field.Operations;
with Icon;

use Ada;
use Ada.Characters;
use Ada.Strings.Unbounded;

use Gtk.Window;
use Gtk.Button;
use Gtk.Toggle_Button;
use Gtk.Check_Button;
use Gtk.Label;
use Gtk.Main;
use Gtk.Arguments;
use Gtk.Box;
use Gtk.Table;
use Gtk.Enums;
use Gtkada.Dialogs;
use Gtk.Dialog;
use Gtk.Scrolled_Window;
use Gtk.Check_Menu_Item;
use Gtk.Combo_Box_Text;
use Gtk.Text_View;
use Gtk.Text_Buffer;
use Gtk.Text_Iter;

with Gtk.Menu;
with Gtk.Menu_Item;
with Gtk.Radio_Menu_Item;
with Gtk.Widget;

use Gtk.Menu;
use Gtk.Menu_Item;
use Gtk.Radio_Menu_Item;
use Gtk.Widget;

package body User_If is
  package Window_Cb is new Gtk.Handlers.Return_Callback (Gtk_Window_Record, Boolean);
  package Button_Cb is new Gtk.Handlers.Callback (Gtk_Button_Record);
  package Toggle_Cb is new Gtk.Handlers.Callback (Gtk_Toggle_Button_Record);
  package Marker_Cb is new Gtk.Handlers.Return_Callback (Gtk_Toggle_Button_Record, Boolean);
  package Check_Cb  is new Gtk.Handlers.Callback (Gtk_Check_Button_Record);

  type Button_Set is
      array (Field.Valid_Row, Field.Valid_Column) of Gtk_Toggle_Button;

  Window         : Gtk_Window;
  Mines_Left     : Gtk_Label;
  Box            : Gtk_Hbox;
  Table          : Gtk_Table;
  Right_Side     : Gtk_Vbox;
  Button         : Button_Set;
  Restart_Button : Gtk_Button;
  Restarting     : Boolean := False;
  Mark_Check     : Gtk_Check_Button;
  Step_Check     : Gtk_Check_Button;
  Rules          : Gtk_Button;
  About          : Gtk_Button;
  Level          : Gtk_Combo_Box_Text;
  Rules_Dialog   : Gtk_Dialog;
  Game_Over      : Gtk_Label;

  You_Won_Message  : constant String := "You Won";
  You_Lost_Message : constant String := "BOOM!";

  type Level_Info is record
    Name  : String (1 .. 3);
    Mines : Natural;
  end record;

  type Level_List is array (Glib.Gint range <>) of Level_Info;

  Levels : constant Level_List := (0 => (Name => " 50", Mines =>  50),
                                   1 => (Name => "100", Mines => 100),
                                   2 => (Name => "150", Mines => 150),
                                   3 => (Name => "200", Mines => 200),
                                   4 => (Name => "250", Mines => 250) );

  Default_Level : constant Glib.Gint := 1;

  Auto_Marking_Desired      : Boolean := False;
  Extended_Stepping_Desired : Boolean := False;

  subtype Cell_String is String (1 .. 1);

  procedure Show_Game_Over is
  begin
    case Field.Operations.Game_State is
      when Field.Operations.Won =>
        Set_Text (Game_Over, You_Won_Message);
      when Field.Operations.Lost =>
        Set_Text (Game_Over, You_Lost_Message);
      when Field.Operations.In_Progress =>
        null;
    end case;
  end Show_Game_Over;
  pragma Inline (Show_Game_Over);

  use type Field.Operations.Game_State_Id;

  procedure Display (Cell   : in Field.Cell_Location;
                     Text   : in Cell_String;
                     Active : in Boolean) is
  begin
    Set_Label (Button (Cell.Row, Cell.Column), Text);
    Set_Active (Button (Cell.Row, Cell.Column), Active);

    if Field.Operations.Game_State /= Field.Operations.In_Progress then
      Show_Game_Over;
    end if;
  end Display;
  pragma Inline (Display);

  procedure Display_Blank (Cell : in Field.Cell_Location) is
  begin
    Display (Cell => Cell, Text => " ", Active => False);
  end Display_Blank;

  procedure Display_Count (Count   : in Field.Valid_Count;
                           Stepped : in Boolean;
                           Cell    : in Field.Cell_Location) is
    Zero_Pos : constant := Character'Pos ('0');
  begin
    Display (Cell   => Cell,
             Text   => Character'Val (Zero_Pos + Count) & "",
             Active => Stepped);
  end Display_Count;

  procedure Display_Mark (Cell : in Field.Cell_Location) is
  begin
     Display (Cell => Cell, Text => "M", Active => False);
  end Display_Mark;

  procedure Display_Mine (Cell : in Field.Cell_Location) is
  begin
    Display (Cell => Cell, Text => "X", Active => True);
  end Display_Mine;

  procedure Display_To_Go (To_Go : in Integer) is
     Image : constant String := Integer'Image (To_Go);
  begin
    Set_Text (Mines_Left, Image);
  end Display_To_Go;

  procedure Reset_Screen is
  begin
     Restarting := True; -- Turn off Toggle
     Set_Text (Mines_Left, "0");
     Set_Text (Game_Over,  "");

     Button_Row : for Row in Field.Valid_Row loop
       Button_Column : for Column in Field.Valid_Column loop
         Display_Blank ( (Row => Row, Column => Column) );
       end loop Button_Column;
     end loop Button_Row;

     Restarting := False;
  end Reset_Screen;

  function Auto_Marking return Boolean is
  begin
    return Auto_Marking_Desired;
  end Auto_Marking;

  function Extended_Stepping return Boolean is
  begin
    return Extended_Stepping_Desired;
  end Extended_Stepping;

  function When_Close (Object : access Gtk_Window_Record'Class;
                       Event  : Gdk.Event.Gdk_Event) return Boolean is
  begin
    Main_Quit;
    return True;
  end When_Close;

  procedure Toggle (Button_Access : access Gtk_Toggle_Button_Record'Class;
                    Params : Gtk_Args) is
    Name : constant String := Get_Name (Button_Access);

    Row    : constant Field.Valid_Row
           := Field.Valid_Row'Value (Name (Name'First    .. Name'First + 1));
    Column : constant Field.Valid_Column
           := Field.Valid_Column'Value (Name (Name'Last - 1 .. Name'Last));
  begin
    if not Restarting then
      if Field.Operations.Game_State /= Field.Operations.In_Progress then
        Show_Game_Over;
        Field.Operations.Restore (Cell => (Row => Row, Column => Column));
      else
        Field.Operations.Step (Cell => (Row => Row, Column => Column));
      end if;
    end if;
  end Toggle;

  procedure Mark_Toggle (Button : access Gtk_Check_Button_Record'Class;
                         Params : Gtk_Args) is
  begin
    Auto_Marking_Desired := Get_Active (Mark_Check);
  end Mark_Toggle;

  procedure Step_Toggle (Button : access Gtk_Check_Button_Record'Class;
                         Params : Gtk_Args) is
  begin
    Extended_Stepping_Desired := Get_Active (Step_Check);
  end Step_Toggle;

  function Button_Press (Button : access Gtk_Toggle_Button_Record'Class;
                         Event : Gdk.Event.Gdk_Event) return Boolean is
    Name : constant String := Get_Name (Button);

    Row    : constant Field.Valid_Row
           := Field.Valid_Row'Value    (Name (Name'First    .. Name'First + 1));
    Column : constant Field.Valid_Column
           := Field.Valid_Column'Value (Name (Name'Last - 1 .. Name'Last));

    Number : constant Glib.Guint := Gdk.Event.Get_Button (Event);

    use type Glib.Guint;
  begin
    case Number is
      when 1 =>
        return False;
      when 2=>
        null;
      when 3 =>
        if Field.Operations.Game_State /= Field.Operations.In_Progress then
          Show_Game_Over;
        else
          Field.Operations.Mark (Cell => (Row => Row, Column => Column) );
        end if;
      when others =>
        -- Many other mouse buttons are possible
        null;
    end case;
    return True;
  end Button_Press;

  procedure When_Restart_Button (Object : access Gtk_Button_Record'Class;
                                 Params : Gtk_Args) is
  begin
    Field.Operations.Set_Mine_Count (Levels (Get_Active (Level) ).Mines);
    Field.Operations.Reset;
  end When_Restart_Button;

  function Close_Rules (Object : access Gtk_Window_Record'Class;
                        Event  : Gdk.Event.Gdk_Event) return Boolean is
  begin
    Destroy (Rules_Dialog);
    return True;
  end Close_Rules;

  procedure Ok_Close_Rules (Object : access Gtk_Button_Record'Class;
                            Params : Gtk_Args) is
  begin
    Destroy (Rules_Dialog);
  end Ok_Close_Rules;

  procedure Rules_Pressed (Object : access Gtk_Button_Record'Class;
                           Params : Gtk_Args) is
    type Text_Set is array (Positive range <>) of Unbounded_String;

    Rules : constant Text_Set :=
      (To_Unbounded_String (
         "The object of the game is to mark all cells containing " &
         "mines and to step on all cells that do not contain a " &
         "mine." & Latin_1.Lf),
         Null_Unbounded_String & Latin_1.Lf,
       To_Unbounded_String (
         "The game is played on a rectangular field of 16 x 30 " &
         "cells. A number of mines are hidden within the field." & Latin_1.Lf),
         Null_Unbounded_String & Latin_1.Lf,
       To_Unbounded_String (
         "Some of the cells have numbers on them. The numbers represent " &
         "the total number of mines in that cell and its " &
         "immediate neighbors. As you play the game, additional cells " &
         "will become numbered." & Latin_1.Lf),
         Null_Unbounded_String & Latin_1.Lf,
       To_Unbounded_String (
         "You step on a cell by clicking on it with the primary " &
         "mouse button (normally the left button when configured " &
         "for a right-handed user). You mark a cell by clicking " &
         "on it with the secondary mouse button (normally the " &
         "right button). A marked cell has an M on it. Marking a " &
         "marked cell unmarks it. You can only mark or step " &
         "on a cell with a number on it." & Latin_1.Lf),
          Null_Unbounded_String & Latin_1.Lf,
       To_Unbounded_String (
         "When you step on a cell, an auto-stepping algorithm " &
         "automatically steps on any of its neighbors that " &
         "obviously do not contain mines. Since this is then " &
         "done for the neighbors of the stepped-on neighbors, " &
         "the auto-stepping algorithm will spread across areas " &
         "of the field that obviously do not contain mines. The " &
         "auto-stepping algorithm is invoked even if the cell is " &
         "already stepped on. This can be useful to clear around " &
         "a new mark." & Latin_1.Lf),
          Null_Unbounded_String & Latin_1.Lf,
       To_Unbounded_String (
         "If you step on a cell containing a mine, either " &
         "directly or indirectly through the auto-stepping " &
         "algorithm, the cell shows an X, and the game is over." & Latin_1.Lf),
         Null_Unbounded_String & Latin_1.Lf,
       To_Unbounded_String (
         "The game is over when you step on a mine, or when you " &
         "have marked all mines and stepped on all other cells. " &
         "If you win, '" & You_Won_Message &"' appears below the " &
         "'About' button. If you lose, '" & You_Lost_Message &
         "' appears there." & Latin_1.Lf),
         Null_Unbounded_String & Latin_1.Lf,
       To_Unbounded_String (
         "At the top right of the window is a number. At the " &
         "start of a game this is the number of mines in the " &
         "field. Each time you mark a cell, this number is " &
         "decreased by one. Each time you unmark a marked cell, " &
         "this number is increased by one. If you successfully " &
         "complete a game, this number will be zero." & Latin_1.Lf),
          Null_Unbounded_String & Latin_1.Lf,
       To_Unbounded_String (
         "The 'New Game' button starts a new game. Any game in " &
         "progress is abandoned." & Latin_1.Lf),
         Null_Unbounded_String & Latin_1.Lf,
       To_Unbounded_String (
         "The level drop-down allows you to choose how many mines " &
         "will be in the field at the start of the next game. You " &
         "can choose from" & Levels (Levels'First).Name & " to " &
         Levels (Levels'Last).Name & " mines. This goes into effect " &
         "the next time you start a new game. At higher numbers of " &
         "mines, it may not be able to win the game without luck." &
         Latin_1.Lf),
         Null_Unbounded_String & Latin_1.Lf,
       To_Unbounded_String (
         "The 'Auto Mark' check box enables an auto-marking " &
         "algorithm that marks any cells that obviously contain " &
         "a mine. At lower levels, the game does not present much " &
         "of an intellectual challenge with this option. At higher " &
         "levels, it's very difficult to play without this option." &
         Latin_1.Lf),
         Null_Unbounded_String & Latin_1.Lf,
       To_Unbounded_String (
         "The 'Auto Step after Mark' check box enables the auto-" &
         "stepping algorithm after a cell is marked, either " &
         "directly or indirectly through the auto-marking " &
         "algorithm.") );

    Scroller  : Gtk_Scrolled_Window;
    Text_View : Gtk_Text_View;
    Text_Buf  : Gtk_Text_Buffer;
    Text_Iter : Gtk_Text_Iter;
    Ok_Button : Gtk_Button;
    V_Box     : Gtk_Vbox;
    Action    : Gtk_Box;
  begin
    Gtk_New (Rules_Dialog);
    Set_Transient_For (Rules_Dialog, Window);
    Set_Title (Rules_Dialog, "Rules for Mine Detector");
    Set_Default_Size (Rules_Dialog, 500, 400);
    Rules_Dialog.Set_Resizable (True);
    Window_Cb.Connect (Rules_Dialog, "delete_event",
                       Window_Cb.To_Marshaller (Close_Rules'Access) );
    V_Box := Get_Content_Area (Rules_Dialog);
    Action := Get_Action_Area (Rules_Dialog);

    Gtk_New (Scroller);
    Pack_Start (V_Box, Scroller);
    Set_Policy (Scroller, Policy_Automatic, Policy_Automatic);

    Gtk_New (Text_Buf);
    Get_Start_Iter (Text_Buf, Text_Iter);
    Insert_Text : for I in Rules'Range loop
      Insert (Buffer => Text_Buf,
              Iter => Text_Iter,
              Text => To_String (Rules (I) ) );
    end loop Insert_Text;

    Gtk_New (Text_View, Text_Buf);
    Set_Wrap_Mode (Text_View, Wrap_Word);
    Add (Scroller, Text_View);
    Set_Editable (View => Text_View, Setting => False);

    Gtk_New (Ok_Button, "OK");
    Button_Cb.Connect (Ok_Button, "clicked", Ok_Close_Rules'Access);
    Pack_Start (Action, Ok_Button, False);

    Show_All (Rules_Dialog);
  end Rules_Pressed;

  procedure About_Pressed (Object : access Gtk_Button_Record'Class;
                           Params : Gtk_Args) is
    Result : Message_Dialog_Buttons;
  begin
    Result := Message_Dialog (
        Msg         => "Mine Detector" & Latin_1.Lf &
                       "Copyright (C) 2007" & Latin_1.Lf &
                       "PragmAda Software Engineering" & Latin_1.Lf &
                       "Released as Free Software under the terms" & Latin_1.Lf &
                       "of the GNU Public License" & Latin_1.Lf &
                       '"' & "Ada Inside" & '"',
        Dialog_Type => Information,
        Buttons     => Button_Ok,
        Title       => "About Mine Detector",
        Parent      => Window);
  end About_Pressed;

  -- Returns a 4-Character String of the form "RRCC", where
  --    RR is the zero-filled image of Row
  --    CC is the zero-filled image of Column
  function Image (Row : Field.Valid_Row;
                  Column : Field.Valid_Column) return String is
    Row_Image    : String   := Field.Valid_Row'Image    (Row);
    Column_Image : String   := Field.Valid_Column'Image (Column);
    Row_First    : Positive := Row_Image'First;
    Column_First : Positive := Column_Image'First;
  begin
     Row_Image (Row_Image'First) := '0';
     Column_Image (Column_Image'First) := '0';

     if Row >= 10 then
       Row_First := Row_First + 1;
     end if;

     if Column >= 10 then
       Column_First := Column_First + 1;
     end if;

     return Row_Image (Row_First .. Row_Image'Last) & Column_Image (Column_First .. Column_Image'Last);
  end Image;

  procedure Create_Level_Option_Menu is
  begin
    for I in Levels'Range loop
      Append_Text (Level, Levels(I).Name);
     end loop;
  end Create_Level_Option_Menu;

  procedure First_Game (Data : System.Address);
  pragma Convention (C, First_Game);

  procedure First_Game (Data : System.Address) is
    Button_Size : constant := 25;

    use type Glib.Guint;
  begin
    Field.Operations.Set_Mine_Count (Levels (Default_Level).Mines);
    Gtk_New (Window, Window_Toplevel);
    Set_Resizable (Window, True);
    Set_Size_Request (Window, 0, 0);
    Set_Position (Window, Win_Pos_Center);
    Set_Title (Window, "Mine Detector");
    Set_Icon (Window, Icon.Mine_Icon);
    Window_Cb.Connect (Window, "delete_event",
                       Window_Cb.To_Marshaller (When_Close'Access) );
    Gtk_New_Hbox (Box => Box, Spacing => 2);
    Add (Window, Box);
    Gtk_New (Table       => Table,
             Rows        => Glib.Guint (Field.Valid_Row'Last),
             Columns     => Glib.Guint (Field.Valid_Column'Last),
             Homogeneous => True);
    Pack_Start (Box, Table);
    Gtk_New_Vbox (Box => Right_Side, Spacing => 2);
    Pack_End (Box, Right_Side);

    Gtk_New (Mines_Left, "0");
    Pack_Start (Right_Side, Mines_Left, False);

    Button_Row : for Row in Field.Valid_Row loop
      Button_Column : for Column in Field.Valid_Column loop
        Gtk_New (Button (Row, Column), " ");
        Set_Name (Button (Row, Column), Image (Row, Column) );
        Set_Size_Request (Button (Row, Column), Button_Size, Button_Size);
        Toggle_Cb.Connect (Button (Row, Column), "toggled", Toggle'Access);
        Marker_Cb.Connect (Button (Row, Column), "button_release_event",
                           Marker_Cb.To_Marshaller (Button_Press'Access));
        Attach (Table,
                Button (Row, Column),
                Glib.Guint (Column) - 1,
                Glib.Guint (Column),
                Glib.Guint (Row) - 1,
                Glib.Guint (Row) );
      end loop Button_Column;
    end loop Button_Row;

    Gtk_New (Restart_Button, "New Game");
    Button_Cb.Connect (Restart_Button, "clicked", When_Restart_Button'Access);
    Pack_Start (Right_Side, Restart_Button, False);

    Gtk_New (Level);
    Create_Level_Option_Menu;
    Popdown (Level);
    Set_Active (Level, Default_Level);
    Show (Level);
    Pack_Start (Right_Side, Level, False);

    Gtk_New (Step_Check, "Auto Step" & Latin_1.Lf & "after Mark");
    Set_Active (Step_Check, Extended_Stepping_Desired);
    Check_Cb.Connect (Step_Check, "toggled", Step_Toggle'Access);
    Pack_Start (Right_Side, Step_Check, False);

    Gtk_New (Mark_Check, "Auto Mark");
    Set_Active (Mark_Check, Auto_Marking_Desired);
    Check_Cb.Connect (Mark_Check, "toggled", Mark_Toggle'Access);
    Pack_Start (Right_Side, Mark_Check, False);

    Gtk_New (Rules, "Rules");
    Button_Cb.Connect (Rules, "clicked", Rules_Pressed'Access);
    Pack_Start (Right_Side, Rules, False);

    Gtk_New (About, "About");
    Button_Cb.Connect (About, "clicked", About_Pressed'Access);
    Pack_Start (Right_Side, About, False);

    Gtk_New (Game_Over, You_Won_Message);
    Set_Use_Markup (Game_Over, True);
    Pack_Start (Right_Side, Game_Over, False);

    Show_All (Window);

    Field.Operations.Reset;
  end First_Game;

  procedure Init is
  begin
    Gtk.Main.Init;
    First_Game (System.Null_Address);
  end Init;

end User_If;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; version 2.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.

