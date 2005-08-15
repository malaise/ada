-- program randtest
-- file randtest.adb
--
-- generates and sorts 48 random numbers
-- tests universal_random and sort_quick_in_place
--
with Ada.Text_IO;
with PragmARC.Assignment;
with PragmARC.Universal_Random;
with PragmARC.Sort_Quick_In_Place;

use Ada;
use PragmARC;
procedure Randtest is
   type Real is digits 8;

   type Real_Set is array (Positive range <>) of Real;

   Num_Nums : constant := 48;

   Set : Real_Set (1 .. Num_Nums) := Real_Set'(1 .. Num_Nums => 0.0);

   procedure Assign is new Assignment (Real);

   procedure Sort is new Sort_Quick_In_Place (Element => Real, Index => Positive, Sort_Set => Real_Set);

   package Random  is new Universal_Random    (Supplied_Real => Real);
   package Real_Io is new Text_IO.Float_Io    (Num => Real);
begin -- Randtest
   Random.Randomize;

   Fill : for I in Set'range loop
      Set (I) := Random.Random;
   end loop Fill;

   Unsorted : for I in Set'First .. Set'Length / 2 loop
      Real_Io.Put (Set (I) );
      Text_IO.Put ("     ");
      Real_Io.Put (Set (I + Set'Length / 2) );
      Text_IO.New_Line;
   end loop Unsorted;

   Text_IO.Skip_Line;
   Sort (Set => Set);

   Sorted : for I in Set'First .. Set'Length / 2 loop
      Real_Io.Put (Set (I) );
      Text_IO.Put ("     ");
      Real_Io.Put (Set (I + Set'Length / 2) );
      Text_IO.New_Line;
   end loop Sorted;

   Text_IO.Skip_Line;
end Randtest;
--
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.