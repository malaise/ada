with Text_Io;
with PragmARC.Matrix_Math;
with Ada.Numerics.Generic_Elementary_Functions;
with System;
procedure Mat_Test is
   type Real is digits System.Max_Digits;
   package Real_Math is new Ada.Numerics.Generic_Elementary_Functions (Real);
   package Real_Matrix is new PragmARC.Matrix_Math (Element => Real, Neg_One_Element => -1.0, Zero_Element => 0.0, Sqrt => Real_Math.Sqrt);
   use type Real_Matrix.Matrix;
   use type Real_Matrix.Vector;
   package Real_Io is new Text_Io.Float_Io (Real);

   Add1  : Real_Matrix.Matrix (2, 3);
   Add2  : Real_Matrix.Matrix (2, 3);
   Mult1 : Real_Matrix.Matrix (2, 2);
   Mult2 : Real_Matrix.Matrix (2, 2);
   Trans : Real_Matrix.Matrix (3, 2);
   Inv   : Real_Matrix.Matrix (2, 2);
   Det   : Real_Matrix.Matrix (4, 4);
   Vec1  : Real_Matrix.Vector (3);
   Vec2  : Real_Matrix.Vector (3);

   procedure Put (Mat : Real_Matrix.Matrix) is
      -- null;
   begin -- put
      Rows : for I in Mat.Value'range (1) loop
         Cols : for J in Mat.Value'range (2) loop
            Real_Io.Put (Mat.Value (I, J), 4, 3, 0);
         end loop Cols;

         Text_Io.New_Line;
      end loop Rows;
   end Put;
begin -- mat_test
   Add1.Value := (1 => ( 1.0, -2.0, 0.0), 2 => (-1.0, 1.0, 2.0) );
   Add2.Value := (1 => (-4.0, -2.0, 4.0), 2 => (-6.0, 4.0, 4.0) );
   Mult1.Value := (1 => (1.0, 2.0), 2 => (0.0, 1.0) );
   Mult2.Value := (1 => (2.0, 2.0), 2 => (1.0, 0.0) );
   Trans.Value := (1 => (1.0, 2.0), 2 => (0.0, 1.0), 3 => (1.0, 2.0) );
   Inv.Value := (1 => (2.0, 1.0), 2 => (-1.0, 1.0) );
   Det.Value := (1 => (0.0, 1.0, 4.0, 2.0), 2 => (1.0, -2.0, 0.0, 0.0), 3 => (1.0, 1.0, -1.0, 3.0), 4 => (7.0, 0.0, 2.0, 1.0) );
   Vec1.Value.Value := (1 => (1 => 1.0), 2 => (1 => 2.0), 3 => (1 => 3.0) );
   Vec2.Value.Value := (1 => (1 => 4.0), 2 => (1 => 5.0), 3 => (1 => 6.0) );

                                             -- correct answers
   Text_Io.Put_Line ("addition");            -- /-3.000 -4.000 4.000\
   Put (Add1 + Add2);                        -- \-7.000  5.000 6.000/
   Text_Io.Skip_Line;
   Text_Io.Put_Line ("subtraction");         -- /5.000  0.000 -4.000\
   Put (Add1 - Add2);                        -- \5.000 -3.000 -2.000/
   Text_Io.Skip_Line;
   Text_Io.Put_Line ("multiplication");      -- /4.000 2.000\
   Put (Mult1 * Mult2);                      -- \1.000 0.000/
   Text_Io.Skip_Line;
   Text_Io.Put_Line ("division");            -- /1.000 -1.000\
   Put (Mult1 / Mult2);                      -- \0.500 -1.000/
   Text_Io.Skip_Line;
   Text_Io.Put_Line ("scalar * matrix");     -- / -8.000 -4.000 8.000\
   Put (2.0 * Add2);                         -- \-12.000  8.000 8.000/
   Text_Io.Skip_Line;
   Text_Io.Put_Line ("matrix * scalar");     -- / -8.000 -4.000 8.000\
   Put (Add2 * 2.0);                         -- \-12.000  8.000 8.000/
   Text_Io.Skip_Line;
   Text_Io.Put_Line ("matrix / scalar");     -- /-2.000 -1.000 2.000\
   Put (Add2 / 2.0);                         -- \-3.000  2.000 2.000/
   Text_Io.Skip_Line;
   Text_Io.Put_Line ("transpose");           -- /1.000 0.000 1.000\
   Put (Real_Matrix.Transpose (Trans) );     -- \2.000 1.000 2.000/
   Text_Io.Skip_Line;
   Text_Io.Put_Line ("invert");              -- /0.333 -0.333\
   Put (Real_Matrix.Invert (Inv) );          -- \0.333  0.667/
   Text_Io.Skip_Line;
   Text_Io.Put_Line ("determinant");         -- -189.000
   Real_Io.Put (Real_Matrix.Determinant (Det), 4, 3, 0);
   Text_Io.New_Line;
   Text_Io.Skip_Line;
   Text_Io.Put_Line ("cofactor");            -- 7.000
   Real_Io.Put (Real_Matrix.Cofactor (Det, 2, 1), 4, 3, 0);
   Text_Io.New_Line;
   Text_Io.Skip_Line;
   Text_Io.Put_Line ("sub_matrix");                  -- / 1.0 -2.0\
   Put (Real_Matrix.Sub_Matrix (Add1, 1, 1, 2, 2) ); -- \-1.0  1.0/
   Text_Io.New_Line;
   Text_Io.Skip_Line;
   Text_Io.Put_Line ("dot product");         -- 32.000
   Real_Io.Put (Vec1 * Vec2, 4, 3, 0);
   Text_Io.New_Line;
   Text_Io.Skip_Line;
   Text_Io.Put_Line ("norm");                -- 3.742
   Real_Io.Put (Real_Matrix.Norm (Vec1), 4, 3, 0);
   Text_Io.New_Line;
   Text_Io.Skip_Line;
   Text_Io.Put_Line ("cross product");         -- /-3.0\
   Put (Real_Matrix.Cross (Vec1, Vec2).Value); -- | 6.0|
   Text_Io.Skip_Line;                          -- \-3.0/
   Text_Io.Put_Line ("matrix * vector");
   Put ("*" (Add1, Vec1).Value);               -- /-3.000\
   Text_Io.Skip_Line;                          -- \ 7.000/
end Mat_Test;
--
-- Copyright (C) 2003 by PragmAda Software Engineering.  All rights reserved.
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.