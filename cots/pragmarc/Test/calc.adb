-- program Calc
-- File calc.ada
--
-- Test program for Postfix_Calculator (& useful calculator program)
--
with Ada.Text_Io;
with PragmARC.Ansi_Tty_Control;
with PragmARC.Postfix_Calculator;

use Ada;
use PragmARC;
procedure Calc is
   Result : Postfix_Calculator.Real;
begin -- Calc
   Result := Postfix_Calculator.Calculator;
   Text_Io.Put (Ansi_Tty_Control.Position (Line => 25, Column => 1) );
end Calc;
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
