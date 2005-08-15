-- Test PragmARC.Sort_Heap
--
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with PragmARC.Assignment;
with PragmARC.Images.Image;
with PragmARC.Sort_Heap;
procedure Heap_Test is
   use Ada;
   use PragmARC;

   package Random is new Numerics.Discrete_Random (Natural);

   type One   is (Only);
   type Two   is (Two_First, Two_Last);
   type Three is (Three_First, Three_Middle, Three_Last);
   type Four  is (Alpha, Bravo, Charlie, Deltah);

   type Sort_Set_N is array (Positive range <>) of Natural;
   type Sort_Set_1 is array (One range <>) of Natural;
   type Sort_Set_2 is array (Two range <>) of Natural;
   type Sort_Set_3 is array (Three range <>) of Natural;
   type Sort_Set_4 is array (Four range <>) of Natural;

   procedure Assign is new PragmARC.Assignment (Natural);

   procedure Sort is new PragmARC.Sort_Heap (Natural, Positive, Sort_Set_N);
   procedure Sort is new PragmARC.Sort_Heap (Natural, One,      Sort_Set_1);
   procedure Sort is new PragmARC.Sort_Heap (Natural, Two,      Sort_Set_2);
   procedure Sort is new PragmARC.Sort_Heap (Natural, Three,    Sort_Set_3);
   procedure Sort is new PragmARC.Sort_Heap (Natural, Four,     Sort_Set_4);

   generic -- Output
      type Index is (<>);
      type Sort_Set is array (Index range <>) of Natural;
   procedure Output (Set : in out Sort_Set);

   procedure Output (Set : in out Sort_Set) is
      -- null;
   begin -- Output
      All_Nums : for I in Set'range loop
         Text_IO.Put_Line (Images.Image (Set (I), Width => 10) );
      end loop All_Nums;

      Text_IO.Skip_Line;
   end Output;

   procedure Put is new Output (Index => Positive, Sort_Set => Sort_Set_N);
   procedure Put is new Output (Index => One,      Sort_Set => Sort_Set_1);
   procedure Put is new Output (Index => Two,      Sort_Set => Sort_Set_2);
   procedure Put is new Output (Index => Three,    Sort_Set => Sort_Set_3);
   procedure Put is new Output (Index => Four,     Sort_Set => Sort_Set_4);

   Set_N : Sort_Set_N (1 .. 23);
   Set_1 : Sort_Set_1 (One);
   Set_2 : Sort_Set_2 (Two);
   Set_3 : Sort_Set_3 (Three);
   Set_4 : Sort_Set_4 (Four);
   Set_0 : Sort_Set_2 (Two_Last .. Two_First);
   Gen   : Random.Generator;
begin -- Heap_Test
   Random.Reset (Gen);
   Set_N := (others => Random.Random (Gen) );
   Put (Set_N);
   Sort (Set_N);
   Put (Set_N);
   Set_1 := (others => Random.Random (Gen) );
   Put (Set_1);
   Sort (Set_1);
   Put (Set_1);
   Set_2 := (others => Random.Random (Gen) );
   Put (Set_2);
   Sort (Set_2);
   Put (Set_2);
   Set_3 := (others => Random.Random (Gen) );
   Put (Set_3);
   Sort (Set_3);
   Put (Set_3);
   Set_4 := (others => Random.Random (Gen) );
   Put (Set_4);
   Sort (Set_4);
   Put (Set_4);
   Set_0 := (others => Random.Random (Gen) );
   Put (Set_0);
   Sort (Set_0);
   Put (Set_0);
end Heap_Test;
--
-- Copyright (C) 2004 by PragmAda Software Engineering.  All rights reserved.
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.