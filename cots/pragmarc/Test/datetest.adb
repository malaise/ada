-- test date_handler
--
with Text_Io;
with Calendar;
with PragmARC.Date_Handler;

use PragmARC;
procedure Datetest is
   Now : Calendar.Time := Calendar.Clock;
begin -- Datetest
   Text_Io.Put_Line ("now is " & Date_Handler.Image (Date => Now) );
   Text_Io.Put_Line ("today is " & Date_Handler.Day_Name'Image (Date_Handler.Day_Of_Week (Now) ) );

   Test : for Year in reverse Positive range 1960 .. 2000 loop
      Text_Io.Put (Positive'Image (Year) & " Nov 01 was a " &
                   Date_Handler.Day_Name'Image (Date_Handler.Day_Of_Week (Year, 11, 01) )
                  )
      ;
      Text_Io.Skip_Line;
   end loop Test;
end Datetest;
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
