with Text_Io;
with PragmARC.Math.Functions;
with System;

use PragmARC.Math;
procedure Mathtest is
   type Real is digits System.Max_Digits;

   package Real_Math is new PragmARC.Math.Functions (Supplied_Real => Real);
   use Real_Math;

   package Real_Io is new Text_Io.Float_Io (Num => Real);
begin -- mathtest
   Text_Io.Put ("7.5 ** 0.75 = ");
   Real_Io.Put (7.5 ** 0.75);
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("7.5 ** 1.5 = ");
   Real_Io.Put (7.5 ** 1.5);
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("7.5 ** 7.5 = ");
   Real_Io.Put (7.5 ** 7.5);
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("sqrt (1.0) = ");
   Real_Io.Put (Sqrt (1.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("sqrt (2.0) = ");
   Real_Io.Put (Sqrt (2.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("sqrt (8.0) = ");
   Real_Io.Put (Sqrt (8.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("exp (0.5) = ");
   Real_Io.Put (Exp (0.5) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("exp (1.0) = ");
   Real_Io.Put (Exp (1.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("exp (4.5) = ");
   Real_Io.Put (Exp (4.5) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("log (2.0) = ");
   Real_Io.Put (Log (2.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("log (10.0) = ");
   Real_Io.Put (Log (10.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("log (e) = ");
   Real_Io.Put (Log (Base_E) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("log (8.5) = ");
   Real_Io.Put (Log (8.5) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("sin (20.0) = ");
   Real_Io.Put (Sin (20.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("sin (45.0) = ");
   Real_Io.Put (Sin (45.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("sin (75.0) = ");
   Real_Io.Put (Sin (75.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("cos (20.0) = ");
   Real_Io.Put (Cos (20.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("cos (45.0) = ");
   Real_Io.Put (Cos (45.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("cos (75.0) = ");
   Real_Io.Put (Cos (75.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("tan (20.0) = ");
   Real_Io.Put (Tan (20.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("tan (45.0) = ");
   Real_Io.Put (Tan (45.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("tan (75.0) = ");
   Real_Io.Put (Tan (75.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("cot (20.0) = ");
   Real_Io.Put (Cot (20.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("cot (45.0) = ");
   Real_Io.Put (Cot (45.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("cot (75.0) = ");
   Real_Io.Put (Cot (75.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arcsin (0.3) = ");
   Real_Io.Put (Arcsin (0.3) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arcsin (0.6) = ");
   Real_Io.Put (Arcsin (0.6) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arcsin (0.9) = ");
   Real_Io.Put (Arcsin (0.9) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arccos (0.3) = ");
   Real_Io.Put (Arccos (0.3) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arccos (0.6) = ");
   Real_Io.Put (Arccos (0.6) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arccos (0.9) = ");
   Real_Io.Put (Arccos (0.9) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arctan (1.0) = ");
   Real_Io.Put (Arctan (1.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arctan (2.0) = ");
   Real_Io.Put (Arctan (2.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arctan (3.0) = ");
   Real_Io.Put (Arctan (3.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arccot (1.0) = ");
   Real_Io.Put (Arccot (1.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arccot (2.0) = ");
   Real_Io.Put (Arccot (2.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arccot (3.0) = ");
   Real_Io.Put (Arccot (3.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arctan (1.0, 1.0) = ");
   Real_Io.Put (Arctan (1.0, 1.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arctan (1.0, 2.0) = ");
   Real_Io.Put (Arctan (1.0, 2.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arctan (1.0, 3.0) = ");
   Real_Io.Put (Arctan (1.0, 3.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("sinh (20.0) = ");
   Real_Io.Put (Sinh (20.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("sinh (45.0) = ");
   Real_Io.Put (Sinh (45.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("sinh (75.0) = ");
   Real_Io.Put (Sinh (75.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("cosh (20.0) = ");
   Real_Io.Put (Cosh (20.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("cosh (45.0) = ");
   Real_Io.Put (Cosh (45.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("cosh (75.0) = ");
   Real_Io.Put (Cosh (75.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("tanh (20.0) = ");
   Real_Io.Put (Tanh (20.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("tanh (45.0) = ");
   Real_Io.Put (Tanh (45.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("tanh (75.0) = ");
   Real_Io.Put (Tanh (75.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("coth (20.0) = ");
   Real_Io.Put (Coth (20.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("coth (45.0) = ");
   Real_Io.Put (Coth (45.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("coth (75.0) = ");
   Real_Io.Put (Coth (75.0 * Pi / 180.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arcsinh (0.3) = ");
   Real_Io.Put (Arcsinh (0.3) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arcsinh (0.6) = ");
   Real_Io.Put (Arcsinh (0.6) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arcsinh (0.9) = ");
   Real_Io.Put (Arcsinh (0.9) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arccosh (1.0) = ");
   Real_Io.Put (Arccosh (1.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arccosh (2.0) = ");
   Real_Io.Put (Arccosh (2.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arccosh (3.0) = ");
   Real_Io.Put (Arccosh (3.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arctanh (0.3) = ");
   Real_Io.Put (Arctanh (0.3) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arctanh (0.6) = ");
   Real_Io.Put (Arctanh (0.6) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arctanh (0.9) = ");
   Real_Io.Put (Arctanh (0.9) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arccoth (2.0) = ");
   Real_Io.Put (Arccoth (2.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arccoth (3.0) = ");
   Real_Io.Put (Arccoth (3.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("arccoth (4.0) = ");
   Real_Io.Put (Arccoth (4.0) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;
end Mathtest;
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
