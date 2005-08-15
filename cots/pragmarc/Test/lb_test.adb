with LB_Test_Help;
with Ada.Text_IO;

use LB_Test_Help;
use Ada.Text_IO;
procedure LB_Test is
   procedure Put_All is new Int_List.Iterate (Context_Data => Integer, Action => Put);

   List : Int_List.Handle (Max_Size => 20);
   Pos  : Int_List.Position;
   Copy : Int_List.Handle (Max_Size => 15);
   Dumb : Integer;
begin -- LB_Test
   Add_10 : for I in 1 .. 10 loop
      if I rem 2 /= 0 then
         Int_List.Insert (Into => List, Item => I, Before => Int_List.First (List), New_Pos => Pos);
      else
         Int_List.Append (Into => List, Item => I, After => Int_List.Last (List), New_Pos => Pos);
      end if;
   end loop Add_10;

   Put_All (Over => List, Context => Dumb);

   Int_List.Assign (To => Copy, From => List);
   Put_All (Over => Copy, Context => Dumb);
   Skip_Line;

   Pos := Int_List.Next (Int_List.Next (Int_List.First (List), List), List);
   Int_List.Delete (From => List, Pos => Pos);

   Put_Line (Item => Integer'Image (Int_List.Length (List) ) );
   Put_Line (Item => Boolean'Image (Int_List.Is_Empty (List) ) );

   Put_All (Over => List, Context => Dumb);

   Int_List.Clear (List => List);

   Put_Line (Item => Integer'Image (Int_List.Length (List) ) );
   Put_Line (Item => Boolean'Image (Int_List.Is_Empty (List) ) );
end LB_Test;
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