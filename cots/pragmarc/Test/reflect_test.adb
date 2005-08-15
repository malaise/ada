-- Test program for PragmARC.Reflection
--
with Ada.Text_IO;
with PragmARC.Reflection;
with Reflect_Tester;
with Reflect_Lib_Level;

use Ada.Text_IO;
use PragmARC;
procedure Reflect_Test is
   package Reflect is new Reflection;
   
   B_1 : Integer; -- GNAT default name for 1st unnamed block
begin -- Reflect_Test
   Put_Line ("Reflect.Unit_Name is >" & Reflect.Unit_Name & '<');
   Reflect_Tester.Test;
   B_1 := Reflect_Tester.Test;
   Put_Line ("Reflect_Lib_Level.Unit_Name is >" & Reflect_Lib_Level.Unit_Name & '<');
   
   Named : declare
      package Reflect is new Reflection;
   begin -- Named
      Put_Line ("Named.Reflect.Unit_Name is >" & Reflect.Unit_Name & '<');
   end Named;
   
   declare -- Unnamed
      package Reflect is new Reflection;
   begin -- Unnamed
      Put_Line ("[Unnamed.]Reflect.Unit_Name is >" & Reflect.Unit_Name & '<');
   end; -- Unnamed
end Reflect_Test;
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
