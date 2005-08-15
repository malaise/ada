-- uses PragmARC.REM_NN_wrapper to solve the XOR problem
--
with Ada.Text_Io;
with PragmARC.ANSI_TTY_Control;
with PragmARC.REM_NN_Wrapper;
with PragmARC.Math.Functions;

use Ada;
use PragmARC;
procedure XOR_Problem is
   use REM_NN_Wrapper;

   type Pattern_ID is (Off_Off, Off_On, On_Off, On_On); -- 4 input patterns of the XOR problem

   Num_Inputs   : constant := 2; -- network architecture: 2-2-1
   Num_Hidden   : constant := 2;
   Num_Outputs  : constant := 1;
   Num_Patterns : constant := Pattern_ID'Pos (Pattern_ID'Last) - Pattern_ID'Pos (Pattern_ID'First) + 1;

   -- an epoch is one pass through all the patterns
   Max_Epochs : constant := 10000;   -- if the network can't converge in this many epochs, something's wrong
   Converged  : constant :=     0.1; -- we'll consider the network converged if the RMS error over an epoch is less than this

   package Real_Math is new Math.Functions (Supplied_Real => Real);

   package Real_Io is new Text_Io.Float_Io (Num => Real);

   RMS_Error : Real := 0.0;
   Output    : REM_NN_Wrapper.Node_Set (1 .. Num_Outputs);
   Error     : Real := 0.0;

   -- define the desired output for each pattern
   type Desired_Set is array (Pattern_ID) of REM_NN_Wrapper.Node_Set (1 .. Num_Outputs);

   Desired_Value : constant Desired_Set :=
      Desired_Set'(Off_Off => (1 .. 1 => -1.0), Off_On => (1 .. 1 => 1.0), On_Off => (1 .. 1 => 1.0), On_On => (1 .. 1 => -1.0) )
      ;

   Invalid_Pattern_Number : exception; -- raised when attempting to convert a number outside 1 .. num_patterns to a pattern_ID

   function To_Positive (Pattern : Pattern_ID) return Positive is
      -- null;
   begin -- to_positive
      return Pattern_ID'Pos (Pattern) + 1;
   end To_Positive;

   function To_Pattern (Number : Positive) return Pattern_ID is
      -- null;
   begin -- to_pattern
      return Pattern_ID'Val (Number - 1);
   exception -- to_pattern
      when others =>
         raise Invalid_Pattern_Number;
   end To_Pattern;

   -- procedure used by network package to get an input pattern & its corresponding desired output
   procedure Provide_Data (Pattern : in Positive; Input : out REM_NN_Wrapper.Node_Set; Desired : out REM_NN_Wrapper.Node_Set) is
      -- define the input values for each pattern
      type Input_Set is array (Pattern_ID) of REM_NN_Wrapper.Node_Set (1 .. Num_Inputs);

      Pattern_Input : constant Input_Set :=
         Input_Set'(Off_Off => (1 .. 2 => -1.0),
         Off_On  => (1 => -1.0, 2 => +1.0),
         On_Off  => (1 => +1.0, 2 => -1.0),
         On_On   => (1 .. 2 => +1.0)
         )
         ;

      Pat : Pattern_ID := To_Pattern (Pattern);
   begin -- provide_data
      Desired := Desired_Value (Pat);
      Input := Pattern_Input (Pat);
   end Provide_Data;

   package XOR_Net is new REM_NN_Wrapper.REM_NN (Num_Input_Nodes  => Num_Inputs,
      Num_Hidden_Nodes => Num_Hidden,
      Num_Output_Nodes => Num_Outputs,
      Num_Patterns     => Num_Patterns,
      Beta             =>  0.5,
      P                => 16.0,
      Weight_File_Name => "XOR.WGT",
      Get_Input        => Provide_Data
      )
      ;
begin -- XOR_problem
   Text_Io.Put (ANSI_TTY_Control.Clear_Screen);
   Text_Io.Put ("XOR");
   Text_Io.Put (ANSI_TTY_Control.Position (3, 1) );
   Text_Io.Put ("Epoch");

   Put_Patterns : for Pattern in Pattern_ID loop
      Text_Io.Put (ANSI_TTY_Control.Position (5 + Pattern_ID'Pos (Pattern), 1) );
      Text_Io.Put (Pattern_ID'Image (Pattern) );
   end loop Put_Patterns;

   Text_Io.Put (ANSI_TTY_Control.Position (7 + Pattern_ID'Pos (Pattern_ID'Last), 1) );
   Text_Io.Put ("RMS_error");

   All_Epochs : for Epoch in 1 .. Max_Epochs loop
      Text_Io.Put (ANSI_TTY_Control.Position (3, 10) );
      Text_Io.Put (Integer'Image (Epoch) );
      RMS_Error := 0.0;

      All_Patterns : for Pattern in Pattern_ID loop
         XOR_Net.Respond (Pattern => To_Positive (Pattern), Output => Output);
         XOR_Net.Train;
         Error := Desired_Value (Pattern) (1) - Output (1);
         RMS_Error := RMS_Error + Error ** 2;
         Text_Io.Put (ANSI_TTY_Control.Position (5 + Pattern_ID'Pos (Pattern), 10) );
         Real_Io.Put (Output (1) );
         Text_Io.Put (ANSI_TTY_Control.Position (5 + Pattern_ID'Pos (Pattern), 40) );
         Real_Io.Put (Error);
      end loop All_Patterns;

      RMS_Error := Real_Math.Sqrt (RMS_Error / Real (Num_Patterns) );
      Text_Io.Put (ANSI_TTY_Control.Position (7 + Pattern_ID'Pos (Pattern_ID'Last), 10) );
      Real_Io.Put (RMS_Error);

      exit All_Epochs when RMS_Error < Converged;
   end loop All_Epochs;

   Text_Io.Put (ANSI_TTY_Control.Position (17, 1) );

   if RMS_Error >= Converged then
      Text_Io.Put_Line ("Network did not converge");

      return;
   end if;

   Text_Io.Put_Line ("Success!");
   XOR_Net.Save_Weights;

   Test : declare
      package Use_Net is new REM_NN_Wrapper.REM_NN (Num_Input_Nodes    => Num_Inputs,
         Num_Hidden_Nodes   => Num_Hidden,
         Num_Output_Nodes   => Num_Outputs,
         Num_Patterns       => 1,
         New_Random_Weights => False,
         Weight_File_Name   => "XOR.WGT",
         Get_Input          => Provide_Data
         )
         ;
   begin -- test
      Test_Patterns : for Pattern in Pattern_ID loop
         Text_Io.Put (Pattern_ID'Image (Pattern) & "  ");
         Use_Net.Respond (Pattern => To_Positive (Pattern), Output => Output);
         Error := Desired_Value (Pattern) (1) - Output (1);
         Real_Io.Put (Output (1) );
         Text_Io.Put ("  ");
         Real_Io.Put (Error);
         Text_Io.New_Line;
      end loop Test_Patterns;
   end Test;
end XOR_Problem;
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
