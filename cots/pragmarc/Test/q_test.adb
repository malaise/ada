-- test program for unbounded queues
--
with Ada.Text_IO;
with Q_Test_Hlp;

use Ada;
use Q_Test_Hlp;
procedure Q_Test is
   Int : Positive;
   Q   : Int_Q.Handle;
begin -- q_test
   Text_IO.Put_Line ("Put some numbers on the queue");

   Put_1 : for I in 1 .. 10 loop
      Text_IO.Put_Line (Integer'Image (I) );
      Q.Put (Item => I);
   end loop Put_1;

   Text_IO.Put_Line ("Take some of them off");

   Get_1 : loop
      exit Get_1 when Q.Length <= 2;

      Q.Get (Item => Int);
      Text_IO.Put_Line (Integer'Image (Int) );
   end loop Get_1;

   Text_IO.Put_Line ("Put some more numbers on the queue");

   Put_2 : for I in 1 .. 5 loop
      Text_IO.Put_Line (Integer'Image (I) );
      Q.Put (Item => I);
   end loop Put_2;

   Text_IO.Put_Line ("Empty the queue");

   Get_2 : loop
      exit Get_2 when Q.Is_Empty;

      Q.Get (Item => Int);
      Text_IO.Put_Line (Integer'Image (Int) );
   end loop Get_2;
end Q_Test;
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