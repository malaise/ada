-- Test PragmARC.PragmARC.Genetic_Algorithm;
--
with Ada.Numerics.Float_Random;
with Ada.Text_IO;
with PragmARC.Genetic_Algorithm;
with System;
procedure Evolve_Pi is
   type Big is digits System.Max_Digits;

   function Fitness (Value : Big) return Float is
      use Ada.Numerics;
   begin -- Fitness
      return -Float (abs (Value - Pi) );
   end Fitness;
   
   Float_Gen : Ada.Numerics.Float_Random.Generator;
   
   function Random return Big is
      -- null;
   begin -- Random
      return 6.0 * Big (Ada.Numerics.Float_Random.Random (Gen => Float_Gen) );
   end Random;
   
   function Mate (Left : Big; Right : Big) return Big is
      -- null;
   begin -- Mate
      return (Left + Right) / 2.0;
   end Mate;
   
   procedure Mutate (Value : in out Big) is
      -- null;
   begin -- Mutate
      Value := Value + 0.2 * Big (Ada.Numerics.Float_Random.Random (Gen => Float_Gen) ) - 0.1;
   end Mutate;
   
   procedure Evolve is new PragmARC.Genetic_Algorithm
      (Gene => Big, Random => Random, Fitness => Fitness, Mate => Mate, Mutate => Mutate);
      
   Best : Big;
   Fit  : Float;
begin -- Evolve_Pi
   Ada.Numerics.Float_Random.Reset (Gen => Float_Gen);
   Evolve (Population_Size           =>    100,
           Max_Generations           =>  1_000,
           Num_No_Change_Generations =>     10,
           Mutation_Probability      =>      0.1,
           Num_Elite_Saved           =>     10,
           Best                      => Best,
           Fit                       => Fit);
   Ada.Text_IO.Put_Line (Item => Big'Image (Best) & ' ' & Float'Image (Fit) );
end Evolve_Pi;
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
