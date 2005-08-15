-- Test PragmARC.Set_Discrete
with Ada.Text_IO;
with PragmARC.Set_Discrete;

use Ada.Text_IO;
use PragmARC;
procedure Set_Test is
   subtype Short_Int is Integer range 1 .. 15;
   package Int_Set  is new Set_Discrete (Element => Short_Int);
   package Char_Set is new Set_Discrete (Element => Character);

   Primes : Int_Set.Set  := Int_Set.Make_Set (Int_Set.Member_List'(1 => 1, 2 => 2, 3 => 3, 4 => 5, 5 => 7, 6 => 11, 7 => 13) );
   Even   : Int_Set.Set  := Int_Set.Make_Set (Int_Set.Member_List'(1 => 2, 2 => 4, 3 => 6, 4 => 8, 5 => 10, 6 => 12, 7 => 14) );
   Valid  : Char_Set.Set := Char_Set.Make_Set (Char_Set.Member_List'(1 => 'Q', 2 => 'C', 3 => 'A', 4 => 'D', 5 => 'B') );
   Union  : Int_Set.Set;
   Inter  : Int_Set.Set;
   Diff   : Int_Set.Set;
   Symm   : Int_Set.Set;

   use type Int_Set.Set;
begin -- Set_Test
   Test_Primes : for I in Short_Int loop
      if Int_Set.Member (I, Primes) then
         Put_Line (Short_Int'Image (I) & " is prime");
      end if;

      if Int_Set.Member (I, Even) then
         Put_Line (Short_Int'Image (I) & " is even");
      end if;
   end loop Test_Primes;

   Test_Characters : for Ch in Character range 'A' .. 'Z' loop
      if Char_Set.Member (Ch, Valid) then
         Put_Line (Ch & " is valid");
      end if;
   end loop Test_Characters;

   Union := Primes + Even;
   Put_Line ("Contents of union of primes with even:");

   Union_Members : for I in Short_Int loop
      if Int_Set.Member (I, Union) then
         Put (Short_Int'Image (I) );
      end if;
   end loop Union_Members;

   New_Line;

   Put_Line ("Size of primes:" & Integer'Image (Int_Set.Size (Primes) ) );
   Put_Line ("Size of even  :" & Integer'Image (Int_Set.Size (Even  ) ) );
   Put_Line ("Size of union :" & Integer'Image (Int_Set.Size (Union ) ) );

   Inter := Primes * Even;
   Put_Line ("Contents of intersection of primes with even:");

   Inter_Members : for I in Short_Int loop
      if Int_Set.Member (I, Inter) then
         Put (Short_Int'Image (I) );
      end if;
   end loop Inter_Members;

   New_Line;

   Diff := Primes - Even;
   Put_Line ("Contents of difference of primes and even:");

   Diff_Members : for I in Short_Int loop
      if Int_Set.Member (I, Diff) then
         Put (Short_Int'Image (I) );
      end if;
   end loop Diff_Members;

   New_Line;

   Symm := Primes / Even;
   Put_Line ("Contents of symmetric difference of primes and even:");

   Symm_Members : for I in Short_Int loop
      if Int_Set.Member (I, Symm) then
         Put (Short_Int'Image (I) );
      end if;
   end loop Symm_Members;

   New_Line;
end Set_Test;
--
-- Copyright (C) 2002 by PragmAda Software Engineering.  All rights reserved.
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.