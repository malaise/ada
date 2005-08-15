-- Test PragmARC.Sort_Radix
--
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with PragmARC.Sort_Radix;

use Ada;
procedure Radix_Test is
   type Number is mod 2 ** 63;

   package Random is new Numerics.Discrete_Random (Number);

   type Sort_Set is array (Positive range <>) of Number;

   procedure Sort is new PragmARC.Sort_Radix (Number, Positive, Sort_Set);

   procedure Put (Set : in Sort_Set) is
      -- null;
   begin -- Put
      All_Nums : for I in Set'range loop
         Text_IO.Put_Line (Number'Image (Set (I) ) );
      end loop All_Nums;

      Text_Io.Skip_Line;
   end Put;

   Set : Sort_Set (1 .. 23);
   Gen : Random.Generator;
begin -- Radix_Test
   Random.Reset (Gen);

   Set := (others => Random.Random (Gen) );

   Put (Set);

   Sort (Set);

   Put (Set);
end Radix_Test;
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
