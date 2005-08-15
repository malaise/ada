with Text_Io;
with PragmARC.Linear_Equation_Solver;
procedure Lin_Test is -- Test PragmARC.Linear_Equation_Solver
   type Real is digits 10;

   package Lin_Eq is new PragmARC.Linear_Equation_Solver (Supplied_Real => Real);
   use Lin_Eq;

   use type Real_Matrix.Matrix;
   use type Real_Matrix.Vector;

   A : Real_Matrix.Matrix (5, 5) := (5, 5, ( ( 0.0,  -1.0, -5.0,  11.0,  8.0),
                                             ( 8.0,  16.0, -8.0,  24.0,  8.0),
                                             (-4.0,  -5.0,  2.0,  -4.0, -3.0),
                                             (-3.0, -10.0,  3.0, -13.0,  5.0),
                                             ( 2.0,   6.0,  6.0,  -8.0, -2.0) ) );
   B : Real_Matrix.Vector (5) := (5, (5, 1, ( (1 .. 1 =>  -61.0),
                                              (1 .. 1 => -200.0),
                                              (1 .. 1 =>   56.0),
                                              (1 .. 1 =>   71.0),
                                              (1 .. 1 =>    0.0) ) ) );

   X1 : Real_Matrix.Vector (5);
   X2 : Real_Matrix.Vector (5);

   package Real_Io is new Text_Io.Float_Io (Num => Real);
begin -- Lin_Test
   X1 := Lin_Eq.Solve_Linear_Equation (A => A, B => B); -- Result should be 4, -9, 6, 0, -5
   Text_Io.Put_Line ("Linear_Equation_Solver result:");

   List_X1 : for I in X1.Value.Value'range (1) loop
      Real_Io.Put (X1.Value.Value (I, 1) );
      Text_Io.New_Line;
   end loop List_X1;

   Text_Io.New_Line;

   Inv : declare -- Try inverting a for exact solution (is A invertible?)
      -- null;
   begin -- Inv
      X2 := Real_Matrix.Invert (A) * B;
      Text_Io.Put_Line ("Inversion result:");

      List_X2 : for I in X2.Value.Value'range (1) loop
         Real_Io.Put (X2.Value.Value (I, 1) );
         Text_Io.New_Line;
      end loop List_X2;
   exception -- inv
   when Real_Matrix.Singular =>
      Text_Io.Put_Line ("Can't invert A");
   end Inv;
end Lin_Test;
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
