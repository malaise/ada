-- test PragmARC.Regular_Expression_Matcher via PragmARC.Character_Regular_Expression_Matcher
-- A simple "grep" program
--
with PragmARC.Character_Regular_Expression_Matcher;
with Ada.Text_IO;

use Ada;
use PragmARC;
procedure Mt is
   File   : Text_IO.File_Type;
   F_Name : String (1 .. 80);
   F_Len  : Natural;
   Pat    : String (1 .. 80);
   P_Len  : Natural;
   P_Pat  : Character_Regular_Expression_Matcher.Processed_Pattern;
   Line   : String (1 .. 256);
   L_Len  : Natural;
   Loc    : Character_Regular_Expression_Matcher.Result;
begin -- Mt
   Text_IO.Put ("Enter file name:  ");
   Text_IO.Get_Line (F_Name, F_Len);
   Text_IO.Open (File, Text_IO.In_File, F_Name (1 .. F_Len) );
   Text_IO.Put ("Enter pattern:  ");
   Text_IO.Get_Line (Pat, P_Len);
   Character_Regular_Expression_Matcher.Process (Pattern => Pat (1 .. P_Len), Processed => P_Pat);

   Search : loop
      exit Search when Text_IO.End_Of_File (File);

      Text_IO.Get_Line (File, Line, L_Len);
      Loc := Character_Regular_Expression_Matcher.Location (P_Pat, Line (1 .. L_Len) );

      if Loc.Found then
         Text_IO.Put_Line (Line (1 .. L_Len) );
         Text_IO.Put_Line ( (1 .. Loc.Start - 1 =>' ') & (1 .. Loc.Length => '^') );
      end if;
   end loop Search;

   Text_IO.Close (File);
exception -- Mt
when Character_Regular_Expression_Matcher.Illegal_Pattern =>
   Text_IO.Put_Line ("Illegal pattern");
when Storage_Error =>
   Text_IO.Put_Line ("Storage error");
end Mt;
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