-- Test PragmARC.Least_Squares_Fitting
--
with PragmARC.Least_Squares_Fitting;
with Output_Windows;
with Plot_Windows;
with PragmARC.Universal_Random;
procedure Line_Fit_Test_Win_IO is
   package Least_Squares is new PragmARC.Least_Squares_Fitting (Supplied_Real => Float);
   
   package Random is new PragmARC.Universal_Random (Supplied_Real => Float);
   
   Data      : Least_Squares.Data_List (1 .. 11);
   Slope     : Float;
   Intercept : Float;
   R_Sq      : Float;
   Plot      : Plot_Windows.Plot_Window_Type     := Plot_Windows.Plot_Window ("Line Fit Test", "X", "Y");
   Output    : Output_Windows.Output_Window_Type := Output_Windows.Output_Window ("Results");
begin -- Line_Fit_Test_Win_IO
   Random.Randomize;
   Plot_Windows.Set_Lines (Plot => Plot, With_Lines => False);
   Plot_Windows.Set_Graph_Title (Plot=> Plot, Graph_Title => "Data");
   
   Fill : for I in Data'range loop
      Data (I).X := 0.1 * Float (I - 1);
      Data (I).Y := Data (I).X + Random.Random_Range (-0.1, 0.1);
      Plot_Windows.Add_Point (Plot => Plot, X => Data (I).X, Y => Data (I).Y);
   end loop Fill;
   
   Least_Squares.Line_Fit (Data => Data, M => Slope, B => Intercept, R_Sq => R_Sq);
   
   Output_Windows.Create_Box (OW => Output, Label => "Slope",     Initial_Value => Slope);
   Output_Windows.Create_Box (OW => Output, Label => "Intercept", Initial_Value => Intercept);
   Output_Windows.Create_Box (OW => Output, Label => "R Sq",      Initial_Value => R_Sq);
   Output_Windows.Draw (OW => Output);
   
   Plot_Windows.New_Graph (Plot => Plot);
   Plot_Windows.Set_Lines (Plot => Plot, With_Lines => True);
   Plot_Windows.Set_Graph_Title (Plot=> Plot, Graph_Title => "Fit Line");
   Plot_Windows.Set_Symbol (Plot => Plot, Symbol => Plot_Windows.No_Symbol);
   
   Plot_Windows.Add_Point (Plot => Plot, X => 0.0, Y => Intercept);
   Plot_Windows.Add_Point (Plot => Plot, X => 1.0, Y => Slope + Intercept);   
   Plot_Windows.Wait (Plot => Plot);
   Output_Windows.Close (OW => Output);
end Line_Fit_Test_Win_IO;
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
