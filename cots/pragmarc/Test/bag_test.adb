with Bag_Test_Help;
with Ada.Text_IO;

use Bag_Test_Help;
use Ada;
procedure Bag_Test is
   Bag : Int_Bag.Handle;
   Dum : Int_Bag.Context_Data;
begin -- Bag_Test
   Add_5 : for I in 1 .. 5 loop
      Bag.Add (Item => I);
   end loop Add_5;

   Bag.Iterate (Action => Put'access, Context => Dum);

   if Bag.Find (3).Found then
      Text_IO.Put_Line (Item => "3 is in there");
   else
      Text_IO.Put_Line (Item => "3 not found");
   end if;

   Bag.Delete (Item => 2);
   Bag.Delete (Item => 4);

   Text_IO.Put_Line (Item => Integer'Image (Bag.Size) );
   Text_IO.Put_Line (Item => Boolean'Image (Bag.Empty) );

   Add_10 : for I in 6 .. 10 loop
      Bag.Add (Item => I);
   end loop Add_10;

   Bag.Iterate (Action => Put'access, Context => Dum);

   if Bag.Find (4).Found then
      Text_IO.Put_Line (Item => "4 is in there");
   else
      Text_IO.Put_Line (Item => "4 not found");
   end if;

   Bag.Delete (Item => 7);
   Bag.Delete (Item => 9);

   Text_IO.Put_Line (Item => Integer'Image (Bag.Size) );
   Text_IO.Put_Line (Item => Boolean'Image (Bag.Empty) );

   Bag.Clear;

   Text_IO.Put_Line (Item => Integer'Image (Bag.Size) );
   Text_IO.Put_Line (Item => Boolean'Image (Bag.Empty) );
end Bag_Test;
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