-- Test PragmARC.Quick_Searcher
--
with PragmARC.Quick_Searcher;
with Ada.Text_Io;

use Ada;
use PragmARC;
procedure Qs_Test is
   File   : Text_Io.File_Type;
   F_Name : String (1 .. 80);
   F_Len  : Natural;
   Pat    : String (1 .. 80);
   P_Len  : Natural;
   Line   : String (1 .. 256);
   L_Len  : Natural;
   Loc    : Quick_Searcher.Result;
begin -- qs_test
   Text_Io.Put ("Enter file name:  ");
   Text_Io.Get_Line (F_Name, F_Len);
   Text_Io.Open (File, Text_Io.In_File, F_Name (1 .. F_Len) );
   Text_Io.Put ("Enter pattern:  ");
   Text_Io.Get_Line (Pat, P_Len);

   Search : loop
      exit Search when Text_Io.End_Of_File (File);

      Text_Io.Get_Line (File, Line, L_Len);
      Loc := Quick_Searcher.Quick_Search (Pat (1 .. P_Len), Line (1 .. L_Len) );

      if Loc.Found then
         Text_Io.Put_Line (Line (1 .. L_Len) );
      end if;
   end loop Search;

   Text_Io.Close (File);
exception -- Qs_Test
when Storage_Error =>
   Text_Io.Put_Line ("Storage error");
end Qs_Test;
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
