-- Test PragmARC.Least_Squares_Fitting
--
with Ada.Text_IO;
with PragmARC.Least_Squares_Fitting;
with PragmARC.Universal_Random;
procedure Line_Fit_Test is
   package Least_Squares is new PragmARC.Least_Squares_Fitting (Supplied_Real => Float);
   
   package Random is new PragmARC.Universal_Random (Supplied_Real => Float);
   
   Data      : Least_Squares.Data_List (1 .. 11);
   Slope     : Float;
   Intercept : Float;
   R_Sq      : Float;
begin -- Line_Fit_Test
   Random.Randomize;
   
   Fill : for I in Data'range loop
      Data (I).X := 0.1 * Float (I - 1);
      Data (I).Y := Data (I).X + Random.Random_Range (-0.1, 0.1);
   end loop Fill;
   
   Least_Squares.Line_Fit (Data => Data, M => Slope, B => Intercept, R_Sq => R_Sq);
   
   Ada.Text_IO.Put_Line (Item => "Slope     " & Float'Image (Slope    ) );
   Ada.Text_IO.Put_Line (Item => "Intercept " & Float'Image (Intercept) );
   Ada.Text_IO.Put_Line (Item => "R Sq      " & Float'Image (R_Sq     ) );
end Line_Fit_Test;
--
-- Copyright (C) 2006 by PragmAda Software Engineering.  All rights reserved.
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA. 
