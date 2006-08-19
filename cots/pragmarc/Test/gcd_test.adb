-- Test program for GCD
--
with Ada.Text_IO;
with PragmARC.Math.Integer_Functions;

use Ada.Text_IO;
procedure GCD_Test is
   use PragmARC.Math.Integer_Functions;
begin -- GCD_Test
   Left : for I in 1 .. 10 loop
      Right : for J in 1 .. 10 loop
         Put_Line ("GCD (" & Integer'Image (I) & ',' & Integer'Image (J) & ") =" & Integer'Image (GCD (I, J) ) );
      end loop Right;
   end loop Left;
end GCD_Test;
--
-- Copyright (C) 2006 by Jeffrey R. Carter
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
