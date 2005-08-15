-- Program Strm_Sub, a simple stream-substitution filter
-- Reads from standard input and writes to standard output
--
-- Usage: Strm_Sub [-g] <pattern> <replacement>
--
-- <pattern> is a regular expression as implemented by PragmARC.Character_Regular_Expression_Matcher
-- <replacement> is a string to replace <pattern> on all lines on which <pattern> occurs
-- If -g is present, all occurrences of <pattern> are replaced by <replacement>, otherwise only the
-- first occurrence is replaced
--
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with PragmARC.Get_Line;
with PragmARC.Character_Regular_Expression_Matcher;

use Ada;
use Ada.Command_Line;
use Ada.Strings.Unbounded;
use Ada.Text_IO;
use PragmARC;
procedure Strm_Sub is
   procedure Usage is
      -- null;
   begin -- Usage
      Put_Line (Item => "Usage: " & Command_Name & " [-g] <pattern> <replacement>");
      Put_Line (Item => '"' & "-g" & '"' & " means replace all occurrences of <pattern> by <replacement>");
      Put_Line (Item => "Only the first occurrence of <pattern> on a line is replaced otherwise");
      New_Line;
      Put_Line (Item => "Pattern elements are:");
      Put_Line (Item => "  ?           Matches any character");
      Put_Line (Item => "  ~<element>  Matches anything except <element>");
      Put_Line (Item => "  *<element>  Matches zero or more occurrences of <element>");
      Put_Line (Item => "  [...]       Matches any of the characters between the brackets");
      Put_Line (Item => "  &           Removes the special meaning of the following character");
      Put_Line (Item => "Any other character matches itself");
   end Usage;
   
   Pattern : Character_Regular_Expression_Matcher.Processed_Pattern;
   Line    : Unbounded_String;
   Result  : Character_Regular_Expression_Matcher.Result;
   Start   : Positive;
   
   Pattern_Index : Positive := 1;
   String_Index  : Positive := Pattern_Index + 1;
   Global        : Boolean  := False;
   
   Invalid_Usage : exception;
begin -- Strm_Sub
   if Argument_Count < 2 then
      raise Invalid_Usage;
   end if;
   
   if Argument (1) = "-g" then
      if Argument_Count < 3 then
         raise Invalid_Usage;
      end if;
      
      Global := True;
      Pattern_Index := Pattern_Index + 1;
      String_Index  := Pattern_Index + 1;
   end if;
   
   Character_Regular_Expression_Matcher.Process (Pattern => Argument (Pattern_Index), Processed => Pattern);
   
   All_Lines : loop
      exit All_Lines when End_Of_File;
      
      Line := To_Unbounded_String (Get_Line);
      Start := 1;
      
      All_Cases : loop
         Fix_Lower_Bound : declare
            -- This ensures that Result.Start refers to the appropriate position in Line, even
            -- on compilers such as GNAT 3.13p that have Slice return a String with a lower
            -- bound of 1.
            S : constant String (Start .. Length (Line) ) := Slice (Line, Start, Length (Line) );
         begin -- Fix_Lower_Bound
            Result := Character_Regular_Expression_Matcher.Location (Pattern, S);
         end Fix_Lower_Bound;
         
         exit All_Cases when not Result.Found;
         
         Replace_Slice (Source => Line,
                        Low    => Result.Start,
                        High   => Result.Start + Result.Length - 1,
                        By     => Argument (String_Index) );
         Start := Result.Start + Argument (String_Index)'Length;
         
         exit All_Cases when not Global or Start > Length (Line);
      end loop All_Cases;
      
      Put_Line (Item => To_String (Line) );
   end loop All_Lines;
exception -- Strm_Sub
when Invalid_Usage =>
   Usage;
when Character_Regular_Expression_Matcher.Illegal_Pattern =>
   Put_Line (Item => "Illegal pattern");
   Usage;
end Strm_Sub;
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