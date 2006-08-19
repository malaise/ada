-- Mine Detector Game
-- Copyright (C) 2006 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- V5.0 2006 Feb 01
--
with Ada.Numerics.Discrete_Random;
with User_If;
package body Field.Operations is
   Num_Mines : Natural := 0;  -- Changed when first game is started.

   subtype Row_Id    is Integer range Valid_Row'First    - 1 .. Valid_Row'Last    + 1; -- Row around field makes things easier
   subtype Column_Id is Integer range Valid_Column'First - 1 .. Valid_Column'Last + 1;

   type State_Id is (Normal, Marked, Stepped_On); -- Possible states of a cell

   subtype Count_Value is Integer range Valid_Count'First - 1 .. Valid_Count'Last; -- Extra value means not yet counted

   type Cell_Info is record
      State   : State_Id    := Normal;
      Mine    : Boolean     := False;
      Count   : Count_Value := Count_Value'First;
      Stepped : Boolean     := False;
   end record;

   type Field_Set is array (Row_Id, Column_Id) of Cell_Info; -- A mine field

   Mine_Field : Field_Set;
   Dead       : Boolean := False;
   To_Mark    : Integer := Num_Mines;
   Step_Count : Natural := 0;

   procedure Detect (Cell : in Cell_Location) is
      -- null;
   begin -- Detect
      if Dead then
         return;
      end if;

      if Mine_Field (Cell.Row, Cell.Column).State = Marked then
         return; -- Can't count a marked cell
      end if;

      if Mine_Field (Cell.Row, Cell.Column).Count not in Valid_Count then -- Cell has not been counted
         Mine_Field (Cell.Row, Cell.Column).Count := 0;

         Count_Rows : for Row in Cell.Row - 1 .. Cell.Row + 1 loop
            Count_Columns : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
               if Mine_Field (Row, Column).Mine then
                  Mine_Field (Cell.Row, Cell.Column).Count := Mine_Field (Cell.Row, Cell.Column).Count + 1;
               end if;
            end loop Count_Columns;
         end loop Count_Rows;
      end if;

      User_If.Display_Count (Count   => Mine_Field (Cell.Row, Cell.Column).Count,
                             Stepped => Mine_Field (Cell.Row, Cell.Column).State = Stepped_On,
                             Cell    => Cell
                            )
      ;
   end Detect;

   procedure Set_Mine_Count (New_Mine_Count : in Natural) is
      -- null;
   begin -- Set_Mine_Count
      Num_Mines := New_Mine_Count;
   end Set_Mine_Count;

   procedure Reset is
      subtype Rand_Set_Index is Integer range 1 .. Valid_Row'Last * Valid_Column'Last;
      type Rand_Set is array (Rand_Set_Index) of Cell_Location; -- For randomly placing mines

      package Random is new Ada.Numerics.Discrete_Random (Rand_Set_Index);

      Rand_List : Rand_Set;
      Index     : Positive := Positive'Last;
      Gen       : Random.Generator;
      Temp      : Cell_Location;
   begin -- Reset
      Dead := False;
      Mine_Field := Field_Set'(others => (others => Cell_Info'(State   => Normal,
                                                               Mine    => False,
                                                               Count   => Count_Value'First,
                                                               Stepped => False) ) );
      To_Mark := Num_Mines;
      Step_Count := 0;

      -- Set the extra ring around the field to stepped_on
      Step_On_Sides : for Row in Mine_Field'range (1) loop
         Mine_Field (Row, Mine_Field'First (2) ).State := Stepped_On;
         Mine_Field (Row, Mine_Field'Last  (2) ).State := Stepped_On;
      end loop Step_On_Sides;

      Step_On_Top_Bottom : for Column in Mine_Field'range (2) loop
         Mine_Field (Mine_Field'First (1), Column).State := Stepped_On;
         Mine_Field (Mine_Field'Last  (1), Column).State := Stepped_On;
      end loop Step_On_Top_Bottom;

      -- Fill Rand_List with all cell locations in preparation for placing mines
      Fill_Rows : for Row in Valid_Row loop
         Fill_Columns : for Column in Valid_Column loop
            Rand_List (Valid_Column'Last * (Row - 1) + Column) := Cell_Location'(Row => Row, Column => Column);
         end loop Fill_Columns;
      end loop Fill_Rows;

      Random.Reset (Gen);

      -- Shuffle Rand_List, a list of cell locations
      Shuffle : for I in Rand_List'range loop
         Index := Random.Random (Gen);
         Temp := Rand_List (I);
         Rand_List (I) := Rand_List (Index);
         Rand_List (Index) := Temp;
      end loop Shuffle;

      -- Put mines in the first Num_Mines locations in Rand_List
      Set_Mines : for I in 1 .. Num_Mines loop
         Mine_Field (Rand_List (I).Row, Rand_List (I).Column).Mine := True;
      end loop Set_Mines;

      -- Display the mine field
      User_If.Reset_Screen;

      Display_Rows : for Row in Valid_Row loop
         Display_Columns : for Column in Valid_Column loop
            if Row    = Valid_Row'First    or else Row    = Valid_Row'Last or else
               Column = Valid_Column'First or else Column = Valid_Column'Last
            then -- Cell is on the edge; automatically count these for the player
               Detect (Cell => Cell_Location'(Row => Row, Column => Column) );
            end if;
         end loop Display_Columns;
      end loop Display_Rows;

      User_If.Display_To_Go (To_Go => To_Mark);
   end Reset;

   function Stepped_On_Neighbor (Cell : Cell_Location) return Boolean is -- See if a cell has a stepped-on neighbor
      -- null;
   begin -- Stepped_On_Neighbor
      Check_Row : for Row in Cell.Row - 1 .. Cell.Row + 1 loop
         Check_Column : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
            if (Row /= Cell.Row or else Column /= Cell.Column) and then Mine_Field (Row, Column).State = Stepped_On then
               return True;
            end if;
         end loop Check_Column;
      end loop Check_Row;

      return False;
   end Stepped_On_Neighbor;

   function Marked_Neighbor (Cell : Cell_Location) return Boolean is -- See if a cell has a marked neighbor
      -- null;
   begin -- Marked_Neighbor
      Check_Row : for Row in Cell.Row - 1 .. Cell.Row + 1 loop
         Check_Column : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
            if (Row /= Cell.Row or else Column /= Cell.Column) and then Mine_Field (Row, Column).State = Marked then
               return True;
            end if;
         end loop Check_Column;
      end loop Check_Row;

      return False;
   end Marked_Neighbor;

   function Num_Marked_Neighbors (Cell : Cell_Location) return Valid_Count is
      Result : Valid_Count := 0;
   begin -- Num_Marked_Neighbors
      Count_Rows : for Row in Cell.Row - 1 .. Cell.Row + 1 loop
         if Row in Valid_Row then
            Count_Columns : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
               if Column in Valid_Column and then Mine_Field (Row, Column).State = Marked then
                  Result := Result + 1;
               end if;
            end loop Count_Columns;
         end if;
      end loop Count_Rows;

      return Result;
   end Num_Marked_Neighbors;

   function Mark_Count_Satisfied (Cell : Cell_Location) return Boolean is
      -- null;
   begin -- Mark_Count_Satisfied
      return Mine_Field (Cell.Row, Cell.Column).Count = Num_Marked_Neighbors (Cell);
   end Mark_Count_Satisfied;

   procedure Auto_Step (Cell : in Cell_Location) is -- Doug's version
   -- Automatically step on any (unstepped-upon) neighbors of Cell if:
   --   (1) Cell has as many marked neighbors its count, or
   --   (2) the neighbor has as many marked neighbors as its count.

      Cell_Satisfied : constant Boolean := Mark_Count_Satisfied (Cell);
   begin -- Auto_Step
      Step_Rows : for Row in Cell.Row - 1 .. Cell.Row + 1 loop
         if Row in Valid_Row then
            Step_Columns : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
               if Column in Valid_Column and then Mine_Field (Row, Column).State /= Marked then
                  if Cell_Satisfied or Mark_Count_Satisfied ( (Row => Row, Column => Column) ) then
                     Step (Cell => (Row => Row, Column => Column) );
                  end if;
               end if;
            end loop Step_Columns;
         end if;
      end loop Step_Rows;
   end Auto_Step;

   procedure Mark (Cell : in Cell_Location) is
      Old_State : State_Id := Mine_Field (Cell.Row, Cell.Column).State;
   begin -- Mark
      if Dead then
         return;
      end if;

      if Stepped_On_Neighbor (Cell) or else Marked_Neighbor (Cell) then
         Mine_Field (Cell.Row, Cell.Column).State := Marked; -- Force detect to count cell's neighbors

         Count_Rows : for Row in Cell.Row - 1 .. Cell.Row + 1 loop -- Automatically detect around marked cell
            if Row in Valid_Row then
               Count_Columns : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
                  if Column in Valid_Column then
                     Detect (Cell => Cell_Location'(Row => Row, Column => Column) );
                  end if;
               end loop Count_Columns;
            end if;
         end loop Count_Rows;

         Mine_Field (Cell.Row, Cell.Column).State := Old_State;

         case Old_State is
         when Normal => -- Mark it
            Mine_Field (Cell.Row, Cell.Column).State := Marked;
            User_If.Display_Mark (Cell => Cell);
            To_Mark := To_Mark - 1;
         when Marked => -- Unmark it
            Mine_Field (Cell.Row, Cell.Column).State := Normal;

            User_If.Display_Count (Count   => Mine_Field (Cell.Row, Cell.Column).Count,
                                   Stepped => Mine_Field (Cell.Row, Cell.Column).State = Stepped_On,
                                   Cell    => Cell
                                  )
               ;

            To_Mark := To_Mark + 1;
         when Stepped_On =>
            null; -- Can't marked a stepped-on cell
         end case;

         User_If.Display_To_Go (To_Go => To_Mark);

         if Extended_Stepping.Enabled then
            Auto_Step (Cell => Cell);
         end if;
      end if;
   end Mark;

   procedure Step (Cell : in Cell_Location) is
      function Num_Normal_Neighbors (Cell : Cell_Location) return Valid_Count is
         Result : Valid_Count := 0;
      begin -- Num_Normal_Neighbors
         Count_Rows : for Row in Cell.Row - 1 .. Cell.Row + 1 loop
            if Row in Valid_Row then
               Count_Columns : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
                  if Column in Valid_Column and then Mine_Field (Row, Column).State = Normal then
                     Result := Result + 1;
                  end if;
               end loop Count_Columns;
            end if;
         end loop Count_Rows;

         return Result;
      end Num_Normal_Neighbors;
   begin -- Step
      if Dead then
         return;
      end if;

      if Mine_Field (Cell.Row, Cell.Column).State = Marked then
         User_If.Display_Mark (Cell => Cell);

         return;
      end if;

      if Mine_Field (Cell.Row, Cell.Column).Stepped then -- Avoid inifinite recursion.
         return;
      end if;

      if not Stepped_On_Neighbor (Cell) and then not Marked_Neighbor (Cell) then
         User_If.Display_Blank (Cell);
      else
         Step_Count := Step_Count + 1;
         Mine_Field (Cell.Row, Cell.Column).State := Stepped_On;
         Mine_Field (Cell.Row, Cell.Column).Stepped := True;

         Count_Rows : for Row in Cell.Row - 1 .. Cell.Row + 1 loop -- Automatically detect around stepped-on cell
            if Row in Valid_Row then
               Count_Columns : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
                  if Column in Valid_Column then
                     Detect (Cell => Cell_Location'(Row => Row, Column => Column) );
                  end if;
               end loop Count_Columns;
            end if;
         end loop Count_Rows;

         if Mine_Field (Cell.Row, Cell.Column).Mine then -- Stepped on a mine!
            Dead := True;
            User_If.Display_Mine (Cell => Cell);

            return;
         else
            User_If.Display_Count (Count   => Mine_Field (Cell.Row, Cell.Column).Count,
                                   Stepped => Mine_Field (Cell.Row, Cell.Column).State = Stepped_On,
                                   Cell    => Cell
                                  )
            ;
         end if;

         Auto_Step (Cell => Cell);

         if Dead then
            return;
         end if;

         if Auto_Marking.Enabled then
            -- See if stepping here has created any normal cells that obviously contain mines;
            -- if so, mark them.
            if Mine_Field (Cell.Row, Cell.Column).Count - Num_Marked_Neighbors (Cell) = Num_Normal_Neighbors (Cell) then
               Mark_Rows : for Row in Cell.Row - 1 .. Cell.Row + 1 loop
                  Mark_Columns : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
                     if Mine_Field (Row, Column).State = Normal then
                        Mark (Cell => (Row => Row, Column => Column) );
                     end if;
                  end loop Mark_Columns;
               end loop Mark_Rows;
            end if;
         end if;

         Step_Count := Step_Count - 1;

         if Step_Count <= 0 then
            Release_Rows : for Row in Valid_Row loop
               Release_Columns : for Column in Valid_Column loop
                  Mine_Field (Row, Column).Stepped := False;
               end loop Release_Columns;
            end loop Release_Rows;
         end if;
      end if;
   end Step;

   -- The game is Lost when a mine has been stepped on, Won when all mines have been marked & all other cells stepped on,
   -- and In_Progress otherwise
   function Game_State return Game_State_ID is
      -- null;
   begin -- Game_State
      if Dead then -- A mine has been stepped on
         return Lost;
      end if;

      Check_Rows : for Row in Valid_Row loop
         Check_Columns : for Column in Valid_Column loop
            if Mine_Field (Row, Column).State = Normal or else
               (Mine_Field (Row, Column).State = Marked) /= Mine_Field (Row, Column).Mine
            then
               return In_Progress;
            end if;
         end loop Check_Columns;
      end loop Check_Rows;

      return Won;
   end Game_State;
end Field.Operations;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
