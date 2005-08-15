-- Test list sort
with Ada.Text_IO, Lsorthlp;

use Ada;
use Lsorthlp;
procedure Lsort is
   List : Int_List.Handle;
   Pos  : Int_List.Position;
   Dumb : Int_List.Context_Data;
begin -- Lsort
   Fill : for I in 1 .. 23 loop
      if I rem 2 = 0 then
         List.Append (After => List.Off_List, Item => I, New_Pos => Pos);
      else
         List.Insert (Before => List.Off_List, Item => I, New_Pos => Pos);
      end if;
   end loop Fill;

   List.Iterate (Action => Print_One'access, Context => Dumb);
   Text_IO.Skip_Line;
   List.Sort (Less_Than => Less'access);
   List.Iterate (Action => Print_One'access, Context => Dumb);
   Text_IO.Skip_Line;
end Lsort;
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