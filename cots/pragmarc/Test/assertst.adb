with Text_Io;
with PragmARC.Assertion_Handler;
procedure Assertst is
   Test : constant String := "Test String";

   I : Integer := Integer'First;

   use PragmARC.Assertion_Handler;
begin -- Assertst
   -- Test a loop invariant
   All_Chars : for I in Test'range loop
      Assert (I in Test'range);
      Text_Io.Put (Test (I) );
      Text_Io.New_Line;
   end loop All_Chars;

   -- Test something else
   Catch_It : declare
      -- null;
   begin -- Catch_It
      Assert (I > Test'Last);
      Text_Io.Put_Line ("Everything's OK.");
   exception -- Catch_It
   when Invalid_Assertion =>
      Text_Io.Put_Line ("Oops! I forgot how 'For' loops work.");
   end Catch_It;
exception -- Assertst
when Invalid_Assertion =>
   Text_Io.Put_Line ("That's interesting.");
end Assertst;
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
