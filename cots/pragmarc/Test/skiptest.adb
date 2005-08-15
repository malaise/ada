-- Test PragmARC.Skip_List_Unbounded
with Skip_Help;
with Ada.Text_IO;
with PragmARC.Ansi_Tty_Control;
with PragmARC.Universal_Random;

use Ada;
use Skip_Help;
procedure Skiptest is
   package ATC renames PragmARC.Ansi_Tty_Control;

   Num_Lines   : constant := 23;
   Num_Columns : constant :=  6;
   Num_Items   : constant := Num_Columns * Num_Lines;

   List   : Skip.Skip_List;
   Result : Positive;
   Search : Skip.Result;
   Copy   : Skip.Skip_List;

   subtype Stored_Integer is Positive range 1 .. Num_Items;

   type Num_List is array (Stored_Integer) of Stored_Integer;

   Number : Num_List;

   package Random is new PragmARC.Universal_Random (Supplied_Real => Float);

   procedure Put (Item : in Positive; Count : in Natural) is
      Row    : constant Positive := (Count - 1) rem Num_Lines + 1;
      Column : constant Positive := Num_Columns * ( (Count - 1) / Num_Lines) + 1;
   begin -- Put
      Text_IO.Put (ATC.Position (Row, Column) & Integer'Image (Item) );
   end Put;

   Count : Natural := 0;

   procedure Print_One (Item : in Positive; Count : in out Natural; Continue : out Boolean) is
      -- null;
   begin -- Print_One
      Continue := True;
      Count := Count + 1;
      Put (Item => Item, Count => Count);
   end Print_One;

   procedure Print_All is new Skip.Iterate (Context_Data => Natural, Action => Print_One);

   procedure Fill (Number : out Num_List) is -- Fill Number with a random perturbation of the values in Stored_Integer
      Temp  : Stored_Integer;
      Index : Stored_Integer;
   begin -- Fill
      In_Order : for I in Number'range loop
         Number (I) := I;
      end loop In_Order;

      Shuffle : for I in Number'range loop
         Temp := Number (I);
         Index := Random.Random_Int (Number'First, Number'Last);
         Number (I) := Number (Index);
         Number (Index) := Temp;
      end loop Shuffle;
   end Fill;
begin -- Skiptest
   Random.Randomize;
   Text_IO.Put_Line (ATC.Clear_Screen & ATC.Position (24, 1) & "Inserting random values");

   Fill (Number => Number);

   Fill_1 : for I in Number'range loop
      Put (Item => Number (I), Count => I);
      Skip.Insert (List => List, Item => Number (I));
   end loop Fill_1;

   Text_IO.Put (ATC.Position (24, 40) & "Press Enter");
   Text_IO.Skip_Line;
   Text_IO.Put_Line (ATC.Clear_Screen & ATC.Position (24, 1) & "Retrieving values in ascending order");

   Empty_1 : for I in 1 .. Skip.Length (List) loop
      Result := Skip.Get_First (List);
      Put (Item => Result, Count => I);
      Skip.Delete (List => List, Item => Result);
   end loop Empty_1;

   Text_IO.Put (ATC.Position (24, 40) & "Press Enter");
   Text_IO.Skip_Line;
   Text_IO.Put_Line (ATC.Clear_Screen & ATC.Position (24, 1) & "Inserting random values");

   Fill (Number => Number);

   Fill_2 : for I in Number'range loop
      Put (Item => Number (I), Count => I);
      Skip.Insert (List => List, Item => Number (I));
   end loop Fill_2;

   Text_IO.Put (ATC.Position (24, 40) & "Press Enter");
   Text_IO.Skip_Line;
   Text_IO.Put_Line (ATC.Clear_Screen & ATC.Position (24, 1) & "Retrieving values in decending order");

   Empty_2 : for I in 1 .. Skip.Length (List) loop
      Result := Skip.Get_Last (List);
      Put (Item => Result, Count => I);
      Skip.Delete (List => List, Item => Result);
   end loop Empty_2;

   Text_IO.Put (ATC.Position (24, 40) & "Press Enter");
   Text_IO.Skip_Line;
   Text_IO.Put_Line (ATC.Clear_Screen & ATC.Position (24, 1) & "Inserting random values");

   Fill (Number => Number);

   Fill_3 : for I in Number'range loop
      Put (Item => Number (I), Count => I);
      Skip.Insert (List => List, Item => Number (I));
   end loop Fill_3;

   Text_IO.Put (ATC.Position (24, 40) & "Press Enter");
   Text_IO.Skip_Line;
   Text_IO.Put_Line (ATC.Clear_Screen & ATC.Position (24, 1) & "Printing in order with an iterator");
   Print_All (List => List, Context => Count);
   Text_IO.Put (ATC.Position (24, 40) & "Press Enter");
   Text_IO.Skip_Line;
   Text_IO.Put_Line (ATC.Clear_Screen & ATC.Position (24, 1) & "Assigning to Copy");
   Skip.Assign (To => Copy, From => List);
   Text_IO.Put (ATC.Position (24, 40) & "Press Enter");
   Text_IO.Skip_Line;
   Text_IO.Put_Line (ATC.Position (24, 1) & ATC.Clear_End_Of_Line & "Printing Copy in order");
   Count := 0;
   Print_All (List => List, Context => Count);
   Text_IO.Put (ATC.Position (24, 40) & "Press Enter");
   Text_IO.Skip_Line;
   Text_IO.Put_Line (ATC.Clear_Screen & ATC.Position (24, 1) & "Using Search");
   Count := 0;

   Check : for I in Stored_Integer loop
      Search := Skip.Search (List, I);

      if Search.Found then
         Count := Count + 1;
         Put (Item => I, Count => Count);
      end if;
   end loop Check;

   Text_IO.Put (ATC.Position (24, 40) & "Press Enter");
   Text_IO.Skip_Line;
   Text_IO.Put_Line ("Is_Empty = " & Boolean'Image (Skip.Is_Empty (List) ) );
   Text_IO.Put_Line ("Length = " & Integer'Image (Skip.Length (List) ) );
   Text_IO.Put_Line ("Clearing list");
   Skip.Clear (List => List);
   Text_IO.Put_Line ("Is_Empty = " & Boolean'Image (Skip.Is_Empty (List) ) );
   Text_IO.Put_Line ("Length = " & Integer'Image (Skip.Length (List) ) );
end Skiptest;
--
-- Copyright (C) 2002 by PragmAda Software Engineering.  All rights reserved.
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- FoundatIOn; either versIOn 2, or (at your optIOn) any later versIOn.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software FoundatIOn, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.