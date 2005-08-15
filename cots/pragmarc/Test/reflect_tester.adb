with PragmARC.Reflection;
with Ada.Text_IO;

use Ada.Text_IO;
use PragmARC;
package body Reflect_Tester is
   procedure Test is
      package Reflect is new Reflection;
   begin -- Test
      Put_Line ("Reflect_Tester.Test(p).Reflect.Unit_Name is >" & Reflect.Unit_Name & '<');
   end Test;
   
   function Test return Integer is
      package Reflect is new Reflection;
   begin -- Test
      Put_Line ("Reflect_Tester.Test(f).Reflect.Unit_Name is >" & Reflect.Unit_Name & '<');
      
      return 0;
   end Test;
end Reflect_Tester;
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
