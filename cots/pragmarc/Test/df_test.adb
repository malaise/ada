-- Test date-formatting (Image) operations from PragmARC.Date_Handler

with PragmARC.Date_Handler;
with Ada.Calendar;
with Ada.Text_IO;

use PragmARC.Date_Handler;
use Ada;
use Ada.Text_IO;
procedure DF_Test is
   procedure Put (Label : in String; Result : in String);
   
   procedure Put (Label : in String; Result : in String) is
      -- null;
   begin -- Put
      Put_Line (Item => Label & ": >" & Result & '<');
   end Put;
   
   Year : Positive := 1;
begin -- DF_Test
   -- Test Year
   Put_Line (Item => "Year formats:");
   
   All_Years : loop
      exit All_Years when Year > 1_000;
      
      Year_Zero : for Fill in Boolean loop
         Put (Label  => Integer'Image (Year) & " Short " & Boolean'Image (Fill),
              Result => Year_Image_Short (Year, Fill) );
         Put (Label  => Integer'Image (Year) & " Long  " & Boolean'Image (Fill),
              Result => Year_Image_Long (Year, Fill) );
      end loop Year_Zero;
      
      Year := 10 * Year;
   end loop All_Years;
   
   -- Test Month
   Put_Line (Item => "Month formats:");
   
   All_Months : for Month in Calendar.Month_Number loop
      Month_Zero : for Fill in Boolean loop
         Put (Label  => Integer'Image (Month) & ' ' & Boolean'Image (Fill),
              Result => Month_Image_Numeric (Month, Fill) );
      end loop Month_Zero;
      
      All_Cases : for Casing in Case_Selection loop
         Put (Label  => Integer'Image (Month) & " Short " & Case_Selection'Image (Casing),
              Result => Month_Image_Short (Month, Casing) );
         Put (Label  => Integer'Image (Month) & " Long  " & Case_Selection'Image (Casing),
              Result => Month_Image_Long (Month, Casing) );
      end loop All_Cases;
   end loop All_Months;
   
   -- Test Day
   Put_Line (Item => "Day formats:");
   
   All_Days : for Day in Calendar.Day_Number loop
      Day_Zero : for Fill in Boolean loop
         Put (Label  => Integer'Image (Day) & ' ' & Boolean'Image (Fill),
              Result => Day_Image (Day, Fill) );
      end loop Day_Zero;
   end loop All_Days;
   
   -- Test Hour
   Put_Line (Item => "Hour formats:");
   
   All_Hours : for Hour in Hour_Number loop
      Zero_Hour : for Fill in Boolean loop
         Put (Label  => Integer'Image (Hour) & " 12 " & Boolean'Image (Fill),
              Result => Hour_Image_12 (Hour, Zero_Fill => Fill) );
         Put (Label  => Integer'Image (Hour) & " 24 " & Boolean'Image (Fill),
              Result => Hour_Image_24 (Hour, Fill) );
      end loop Zero_Hour;
   end loop All_Hours;
   
   -- Test Minute
   Put_Line (Item => "Minute formats:");
   
   All_Minutes : for Minute in Minute_Number loop
      Zero_Minute : for Fill in Boolean loop
         Put (Label  => Integer'Image (Minute) & ' ' & Boolean'Image (Fill),
              Result => Minute_Image (Minute, Fill) );
      end loop Zero_Minute;
   end loop All_Minutes;
   
   -- Test Seconds
   Put_Line (Item => "Seconds formats:");
   
   Zero_Seconds : for Fill in Boolean loop
      All_Afts : for Aft in 0 .. 4 loop
         Put (Label  => " 1.23456 " & Boolean'Image (Fill) & " Aft: " & Integer'Image (Aft),
              Result => Seconds_Image (1.23456, Fill, Aft) );
         Put (Label  => "51.23456 " & Boolean'Image (Fill) & " Aft: " & Integer'Image (Aft),
              Result => Seconds_Image (51.23456, Fill, Aft) );
      end loop All_Afts;
   end loop Zero_Seconds;
end DF_Test;
--
-- Copyright (C) 2001 by PragmAda Software Engineering.  All rights reserved.
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
