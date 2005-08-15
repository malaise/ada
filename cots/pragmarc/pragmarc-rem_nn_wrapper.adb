-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Sequential_Io;
with PragmARC.Universal_Random;
with Ada.Numerics.Generic_Elementary_Functions;

use Ada;
package body PragmARC.REM_NN_Wrapper is
   package body REM_NN is
      subtype Input_Id  is Positive range 1 .. Num_Input_Nodes;
      subtype Hidden_Id is Positive range 1 .. Num_Hidden_Nodes;

      subtype Pattern_Id is Positive range 1 .. Num_Patterns;
      type Desired_Set is array (Pattern_Id) of Output_Set;
      type Count_Value is range 0 .. System.Max_Int;

      -- Basics about nodes:
      -- A node maintains the weights & related values for the connections TO itself
      -- A node also calculates its output value and supplies it to other nodes (those to which it connects)
      -- on demand

      package Input is -- Definition of input nodes
         type Node_Handle is limited private;

         procedure Set_Input (Node : in out Node_Handle; Value : in Real); -- The node accepts its external input value
         function Get_Output (From : Node_Handle) return Real; -- The node provides its output on demand
      private -- Input
         type Node_Handle is record -- An input node just provides its input value as its output
            Output : Real := 0.0;
         end record;
      end Input;

      type Input_Node_Set is array (Input_Id) of Input.Node_Handle;

      Deriv_Lim : constant := 1.0E-4;

      type Weight_Group is record
         Weight : Real := 0.0;
         Active : Boolean := True;
         G : Real := 2.0 * Ec;
         Delta_W_Rm : Real := 0.0;
         Deriv_Rm : Real := Deriv_Lim;
      end record;
      type Weight_Set is array (Positive range <>) of Weight_Group;

      package Hidden is -- Definition of hidden nodes
         type Node_Handle is limited private;

         procedure Respond (Node : in out Node_Handle); -- The node collects its input & calculates its output
         function Get_Output (From : Node_Handle) return Real; -- The node provides its output on demand
         procedure Train (Node : in out Node_Handle; Id : in Hidden_Id); -- The node updates weights on connections to it

         -- To use pre-calculated weights, the network has to be able to set weights
         -- To save weights, the network has to be able to obtain weights
         procedure Set_Weight (Node : in out Node_Handle; From : in Input_Id; Weight : in Weight_Group);
         function Get_Weight (Node : Node_Handle; From : Input_Id) return Weight_Group;
         procedure Set_Bias_Weight (Node : in out Node_Handle; Weight : in Weight_Group);
         function Get_Bias_Weight (Node : Node_Handle) return Weight_Group;
      private -- Hidden
         type Node_Handle is record
            Output : Real := 0.0;
            Deriv  : Real := 0.0;
            Bias   : Weight_Group;
            Weight : Weight_Set (Input_Id); -- Weights from input nodes to this node
         end record;
      end Hidden;

      type Hidden_Node_Set is array (Hidden_Id) of Hidden.Node_Handle;

      type Star_Group is record
         E_Star : Real := 0.0;
         H_Star : Real := 0.0;
      end record;
      type Star_Set is array (Hidden_Id) of Star_Group;

      package Output is -- Definition of output nodes
         type Node_Handle (Input_To_Output : Boolean) is limited private;

         procedure Respond (Node : in out Node_Handle; Result : out Real);
         -- The node collects its input & calculates its output, which is provided in result
         procedure Train (Node : in out Node_Handle; Id : in Output_Id); -- The node updates weights on connections to it
         function Get_Stars (Node : Node_Handle; From : Hidden_Id) return Star_Group;
         -- The node provides weighted values of E* & H* to hidden nodes on demand

         -- To use pre-calculated weights, the network has to be able to set weights
         -- To save weights, the network has to be able to obtain weights
         procedure Set_Input_Weight (Node : in out Node_Handle; From : in Input_Id; Weight : in Weight_Group);
         function Get_Input_Weight (Node : Node_Handle; From : Input_Id) return Weight_Group;
         procedure Set_Hidden_Weight (Node : in out Node_Handle; From : in Hidden_Id; Weight : in Weight_Group);
         function Get_Hidden_Weight (Node : Node_Handle; From : Hidden_Id) return Weight_Group;
         procedure Set_Bias_Weight (Node : in out Node_Handle; Weight : in Weight_Group);
         function Get_Bias_Weight (Node : Node_Handle) return Weight_Group;
      private -- Output
         -- An output node has connections from hidden nodes, which have weights to update
         -- The hidden nodes require propagated values of E* & H*
         -- These values must be propagated BEFORE the weights on the connections are updated
         -- Because an output node's TRAIN procedure is called before the hidden node's TRAIN,
         -- the output node stores the weighted values of E* & H* in hidden_star before updating the weights

         type Node_Handle (Input_To_Output : Boolean) is record
            Output        : Real := 0.0;
            Deriv         : Real := 0.0;
            Bias          : Weight_Group;
            Hidden_Weight : Weight_Set (Hidden_Id); -- Weights from hidden nodes to this node
            Hidden_Star   : Star_Set; -- Weighted E* & H* values; see comment block above

            case Input_To_Output is
            when False =>
               null;
            when True =>
               Input_Weight : Weight_Set (Input_Id); -- Weights from input nodes to this node
            end case;
         end record;
      end Output;

      subtype Output_Node_Handle is Output.Node_Handle (Input_To_Output => Input_To_Output_Connections);
      type Output_Node_Set is array (Output_Id) of Output_Node_Handle;

      Input_Lim  : constant := 300.0;
      H_Star_Lim : constant := 100.0;

      -- Network state information
      Input_Node  : Input_Node_Set;
      Hidden_Node : Hidden_Node_Set;
      Output_Node : Output_Node_Set;
      Desired     : Desired_Set := Desired_Set'(others => Output_Set'(others => 0.0) );
      Target      : Output_Set := Output_Set'(others => 0.0); -- Current D infinity

      Current_Pattern : Positive;

      Cycle_P : Natural_Real := P; -- Values of the parameters which are used, taking into account effect of K_? values
      Cycle_Q : Natural_Real := Q;
      Cycle_S : Natural_Real := S;

      Update_Count : Count_Value := 0;

      package Connection_Io is new Sequential_Io (Element_Type => Weight_Group);

      -- Variables used for initialization
      Weight_File : Connection_Io.File_Type;
      Weight      : Weight_Group;

      package Random is new Universal_Random (Supplied_Real => Real);
      package Real_Math is new Numerics.Generic_Elementary_Functions (Float_Type => Real);

      function Random_Range (Min : Real; Max : Real) return Real renames Random.Random_Range;

      -- Transfer: apply the node transfer function to a weighted summed input value
      --           Calculates node output & derivative
      procedure Transfer (Net_Input : in Real; Output : out Real; Deriv : out Real) is
         A : Real := Real_Math.Exp (Real'Min (Real'Max (0.5 * Net_Input, -Input_Lim), Input_Lim) );
         B : Real := 1.0 / A;
      begin -- Transfer
         Output := (A - B) / (A + B); -- Hyperbolic tangent (tanh)
         Deriv := 2.0 / ( (A + B) ** 2); -- Derivative of tanh
      end Transfer;

      -- New_Rm: Calculate the new value of a Recursive Mean
      function New_Rm (Length : Positive_Real; Old_Value : Real; New_Value : Real) return Real is
         -- null;
      begin -- New_Rm
         return (1.0 - 1.0 / Length) * Old_Value + (1.0 / Length) * New_Value;
      end New_Rm;

      -- Update_values: Update the values related to a connection
      procedure Update_Values (Sender_Out   : in Real;
                               Receiver_Out : in Real;
                               E_Star       : in Real;
                               H_Star       : in Real;
                               Weight       : in out Weight_Group
                              )
      is
         Delta_W_Lim : constant := 100.0;
         Psi_Lim     : constant := 100.0;
         Denom_Lim   : constant := 1.0E-6;
         Weight_Lim  : constant := 100.0;

         Delta_W : Real;
         Psi     : Real;
         Denom   : Real;
      begin -- Update_Values
         -- Update numerator & denominator of delta W
         Weight.Deriv_Rm := New_Rm (Cycle_P, Weight.Deriv_Rm, (Sender_Out * H_Star) ** 2);
         Weight.Delta_W_Rm := New_Rm (Cycle_Q, Weight.Delta_W_Rm, (Beta / Cycle_P) * Sender_Out * E_Star);

         Delta_W := Real'Min (Real'Max (Weight.Delta_W_Rm / Weight.Deriv_Rm, - Delta_W_Lim), Delta_W_Lim);

         -- Determine if this connection needs to be inactivated or reactivated
         -- Thinning needs the current value of Weight.Weight, so do thinning calculations before applying Delta_W
         if Thinning_Active then
            Psi := Real'Min (Real'Max (2.0 * Sender_Out * Receiver_Out * Weight.Weight, -Psi_Lim), Psi_Lim);
            Denom := (2.0 * Sender_Out * Receiver_Out) ** 2;
            if Denom < Denom_Lim then
               Weight.G := New_Rm (Cycle_S,
                                   Weight.G,
                                   E_Star ** 2 + (Sender_Out * H_Star) ** 2 * 0.5 * Weight.Weight ** 2 *
                                   Real_Math.Exp (2.0 * Sender_Out * Receiver_Out * Weight.Weight)
                                  )
               ;
            else
               Weight.G := New_Rm (Cycle_S,
                                   Weight.G,
                                   E_Star ** 2 + (Sender_Out * H_Star) ** 2 *
                                   ( (1.0 - (1.0 + Psi) * Real_Math.Exp (-Psi)) / Denom) *
                                   Real_Math.Exp (2.0 * Sender_Out * Receiver_Out * Weight.Weight)
                                  )
               ;
            end if;

            if Weight.Active then
               Weight.Active := Weight.G > Ec;
            else
               Weight.Active := Weight.G > Ec + Delta_Ec;
            end if;
         end if;

         Weight.Weight := Real'Min (Real'Max (Weight.Weight + Delta_W, -Weight_Lim), Weight_Lim);
      end Update_Values;

      procedure Respond (Pattern : in Positive; Output : out Output_Set) is
         Input_Value : Node_Set (Input_Id);
      begin -- Respond
         Current_Pattern := Pattern;
         Get_Input (Pattern => Pattern, Input => Input_Value, Desired => Target);

         -- Get network response
         -- Send input to input nodes
         All_Input : for Node in Input_Id loop
            Input.Set_Input (Node => Input_Node (Node), Value => Input_Value (Node) );
         end loop All_Input;

         -- For hidden nodes
         All_Hidden : for Node in Hidden_Id loop
            Hidden.Respond (Node => Hidden_Node (Node) );
         end loop All_Hidden;

         -- For output nodes
         All_Output : for Node in Output_Id loop
            REM_NN.Output.Respond (Node => Output_Node (Node), Result => Output (Node) );
         end loop All_Output;
      end Respond;

      procedure Train is
         -- null;
      begin -- Train
         -- Update global "constants"
         Update_Count := Update_Count + 1;
         Cycle_P := Real'Max (P, Real (Update_Count) * K_P);
         Cycle_Q := Real'Max (Q, Real (Update_Count) * K_Q);
         Cycle_S := Real'Max (S, Real (Update_Count) * K_S);

         All_Outputs : for Node in Output_Id loop
            Desired (Current_Pattern) (Node) := New_Rm (R, Desired (Current_Pattern) (Node), Target (Node) );
            Output.Train (Node => Output_Node (Node), Id => Node);
         end loop All_Outputs;

         All_Hidden : for Node in Hidden_Id loop
            Hidden.Train (Node => Hidden_Node (Node), Id => Node);
         end loop All_Hidden;
      end Train;

      procedure Save_Weights is
         -- null;
      begin -- Save_Weights
         Connection_Io.Create (File => Weight_File, Name => Weight_File_Name);

         From_Inputs : for I_Id in Input_Id loop
            To_Hidden : for H_Id in Hidden_Id loop
               Connection_Io.Write (File => Weight_File, Item => Hidden.Get_Weight (Hidden_Node (H_Id), I_Id) );
            end loop To_Hidden;

            if Input_To_Output_Connections then
               To_Output : for O_Id in Output_Id loop
                  Connection_Io.Write (File => Weight_File, Item => Output.Get_Input_Weight (Output_Node (O_Id), I_Id) );
               end loop To_Output;
            end if;
         end loop From_Inputs;

         From_Hidden : for H_Id in Hidden_Id loop
            Connection_Io.Write (File => Weight_File, Item => Hidden.Get_Bias_Weight (Hidden_Node (H_Id) ) );

            Hidden_To_Output : for O_Id in Output_Id loop
               Connection_Io.Write (File => Weight_File, Item => Output.Get_Hidden_Weight (Output_Node (O_Id), H_Id) );
            end loop Hidden_To_Output;
         end loop From_Hidden;

         Output_Bias : for O_Id in Output_Id loop
            Connection_Io.Write (File => Weight_File, Item => Output.Get_Bias_Weight (Output_Node (O_Id) ) );
         end loop Output_Bias;

         Connection_Io.Close (File => Weight_File);
      end Save_Weights;

      package body Input is
         procedure Set_Input (Node : in out Node_Handle; Value : in Real) is
            -- null;
         begin -- Set_Input
            Node.Output := Value;
         end Set_Input;

         function Get_Output (From : Node_Handle) return Real is
            -- null;
         begin -- Get_Output
            return From.Output;
         end Get_Output;
      end Input;

      package body Hidden is
         procedure Respond (Node : in out Node_Handle) is
            Net_Input : Real := 0.0;
         begin -- respond
            if Node.Bias.Active then
               Net_Input := Node.Bias.Weight;
            end if;

            Sum_Input : for I_Id in Input_Id loop
               if Node.Weight (I_Id).Active then
                  Net_Input := Net_Input + Input.Get_Output (Input_Node (I_Id) ) * Node.Weight (I_Id).Weight;
               end if;
            end loop Sum_Input;

            Transfer (Net_Input => Net_Input, Output => Node.Output, Deriv => Node.Deriv);
         end Respond;

         function Get_Output (From : Node_Handle) return Real is
            -- null;
         begin -- Get_Output
            return From.Output;
         end Get_Output;

         procedure Train (Node : in out Node_Handle; Id : in Hidden_Id) is
            Star : Star_Group;
            Prop : Star_Group;
            In_Use : Boolean := False;
         begin -- Train
            -- Sum propagated E* & H* from output nodes
            Sum_Stars : for O_Id in Output_Id loop
               Prop := Output.Get_Stars (Output_Node (O_Id), Id);
               Star := Star_Group'(E_Star => Star.E_Star + Prop.E_Star,
                                   H_Star => Star.H_Star + Prop.H_Star
                                  )
               ;
            end loop Sum_Stars;

            Star.E_Star := Node.Deriv * Star.E_Star;
            Star.H_Star := Real'Min (Real'Max (Node.Deriv * Star.H_Star, -H_Star_Lim), H_Star_Lim);

            -- Update connections to this node
            Modify : for I_Id in Input_Id loop
               Update_Values (Sender_Out => Input.Get_Output (Input_Node (I_Id) ),
                              Receiver_Out => Node.Output,
                              E_Star => Star.E_Star,
                              H_Star => Star.H_Star,
                              Weight => Node.Weight (I_Id)
                             )
               ;
            end loop Modify;

            Update_Values (Sender_Out => 1.0, -- Update bias
                           Receiver_Out => Node.Output,
                           E_Star => Star.E_Star,
                           H_Star => Star.H_Star,
                           Weight => Node.Bias
                          )
            ;

            -- Check for inactivity
            Check : for I_Id in Input_Id loop
               In_Use := In_Use or Node.Weight (I_Id).Active;
            end loop Check;

            if not In_Use then -- No active input connections, so turn off bias
               Node.Bias.Active := False;
            end if;
         end Train;

         procedure Set_Weight (Node : in out Node_Handle; From : in Input_Id; Weight : in Weight_Group) is
            -- null;
         begin -- Set_Weight
            Node.Weight (From) := Weight;
         end Set_Weight;

         function Get_Weight (Node : Node_Handle; From : Input_Id) return Weight_Group is
            -- null;
         begin -- Get_Weight
            return Node.Weight (From);
         end Get_Weight;

         procedure Set_Bias_Weight (Node : in out Node_Handle; Weight : in Weight_Group) is
            -- null;
         begin -- Set_Bias_Weight
            Node.Bias := Weight;
         end Set_Bias_Weight;

         function Get_Bias_Weight (Node : Node_Handle) return Weight_Group is
            -- null;
         begin -- Get_Bias_Weight
            return Node.Bias;
         end Get_Bias_Weight;
      end Hidden;

      package body Output is
         procedure Respond (Node : in out Node_Handle; Result : out Real) is
            Net_Input : Real := 0.0;
         begin -- Respond
            if Node.Bias.Active then
               Net_Input := Node.Bias.Weight;
            end if;

            if Node.Input_To_Output then
               Sum_Input : for I_Id in Input_Id loop
                  if Node.Input_Weight (I_Id).Active then
                     Net_Input := Net_Input + Input.Get_Output (Input_Node (I_Id) ) * Node.Input_Weight (I_Id).Weight;
                  end if;
               end loop Sum_Input;
            end if;

            Sum_Hidden : for H_Id in Hidden_Id loop
               if Node.Hidden_Weight (H_Id).Active then
                  Net_Input := Net_Input + Hidden.Get_Output (Hidden_Node (H_Id) ) * Node.Hidden_Weight (H_Id).Weight;
               end if;
            end loop Sum_Hidden;

            Transfer (Net_Input => Net_Input, Output => Node.Output, Deriv => Node.Deriv);

            Result := Node.Output;
         end Respond;

         procedure Train (Node : in out Node_Handle; Id : in Output_Id) is
            Star : Star_Group;
         begin -- Train
            -- Calculate E* & H* for this node
            Star.H_Star := Real'Min (Real'Max (Node.Deriv, -H_Star_Lim), H_Star_Lim);
            Star.E_Star := Star.H_Star * (Desired (Current_Pattern) (Id) - Node.Output +
                                          Random_Range (-Random_E_Star_Range, Random_E_Star_Range)
                                         )
            ;
            Star.H_Star := Star.H_Star + Random_Range (-Random_H_Star_Range, Random_H_Star_Range);

            -- E* & H* have to be propagated back before the weights are updated
            -- This is done by multiplying them by the corresponding weights, & storing the result in Node.Hidden_Star
            -- The values in Node.Hidden_Star are then returned in response to calls to Get_Star
            Adjust_Stars : for H_Id in Hidden_Id loop
               if not Node.Hidden_Weight (H_Id).Active then
                  Node.Hidden_Star (H_Id) := Star_Group'(E_Star => 0.0, H_Star => 0.0);
               else
                  Node.Hidden_Star (H_Id) := Star_Group'(E_Star => Node.Hidden_Weight (H_Id).Weight * Star.E_Star,
                                                         H_Star => Node.Hidden_Weight (H_Id).Weight * Star.H_Star
                                                        )
                  ;
               end if;
            end loop Adjust_Stars;

            -- Update all connections to this node
            if Node.Input_To_Output then
               Update_Input : for I_Id in Input_Id loop
                  Update_Values (Sender_Out => Input.Get_Output (Input_Node (I_Id) ),
                                 Receiver_Out => Node.Output,
                                 E_Star => Star.E_Star,
                                 H_Star => Star.H_Star,
                                 Weight => Node.Input_Weight (I_Id)
                                )
                  ;
               end loop Update_Input;
            end if;

            Update_Hidden : for H_Id in Hidden_Id loop
               Update_Values (Sender_Out => Hidden.Get_Output (Hidden_Node (H_Id) ),
                              Receiver_Out => Node.Output,
                              E_Star => Star.E_Star,
                              H_Star => Star.H_Star,
                              Weight => Node.Hidden_Weight (H_Id)
                             )
               ;
            end loop Update_Hidden;

            Update_Values (Sender_Out => 1.0, -- Update bias value
                           Receiver_Out => Node.Output,
                           E_Star => Star.E_Star,
                           H_Star => Star.H_Star,
                           Weight => Node.Bias
                          )
            ;
         end Train;

         function Get_Stars (Node : Node_Handle; From : Hidden_Id) return Star_Group is
            -- null;
         begin -- Get_Stars
            return Node.Hidden_Star (From);
         end Get_Stars;

         procedure Set_Input_Weight (Node : in out Node_Handle; From : in Input_Id; Weight : in Weight_Group) is
            -- null;
         begin -- Set_Input_Weight
            Node.Input_Weight (From) := Weight;
         end Set_Input_Weight;

         function Get_Input_Weight (Node : Node_Handle; From : Input_Id) return Weight_Group is
            -- null;
         begin -- Get_Input_Weight
            return Node.Input_Weight (From);
         end Get_Input_Weight;

         procedure Set_Hidden_Weight (Node : in out Node_Handle; From : in Hidden_Id; Weight : in Weight_Group) is
            -- null;
         begin -- Set_Hidden_Weight
            Node.Hidden_Weight (From) := Weight;
         end Set_Hidden_Weight;

         function Get_Hidden_Weight (Node : Node_Handle; From : Hidden_Id) return Weight_Group is
            -- null;
         begin -- Get_Hidden_Weight
            return Node.Hidden_Weight (From);
         end Get_Hidden_Weight;

         procedure Set_Bias_Weight (Node : in out Node_Handle; Weight : in Weight_Group) is
            -- null;
         begin -- Set_Bias_Weight
            Node.Bias := Weight;
         end Set_Bias_Weight;

         function Get_Bias_Weight (Node : Node_Handle) return Weight_Group is
            -- null;
         begin -- Get_Bias_Weight
            return Node.Bias;
         end Get_Bias_Weight;
      end Output;
   begin -- REM_NN
      if Num_Hidden_Nodes <= 0 and then not Input_To_Output_Connections then
         raise Invalid_Architecture;
      end if;

      Random.Randomize;

      if not New_Random_Weights then
         Connection_Io.Open (File => Weight_File, Mode => Connection_Io.In_File, Name => Weight_File_Name);
      end if;

      -- Get initial values for weights
      From_Inputs : for I_Id in Input_Id loop
         To_Hidden : for H_Id in Hidden_Id loop
            if New_Random_Weights then -- Random selection of initial weights
               Weight.Weight := Random_Range (-Random_Weight_Range, Random_Weight_Range);
            else -- read initial weights from file
               Connection_Io.Read (File => Weight_File, Item => Weight);
            end if;

            Hidden.Set_Weight (Node => Hidden_Node (H_Id), From => I_Id, Weight => Weight);
         end loop To_Hidden;

         if Input_To_Output_Connections then
            To_Output : for O_Id in Output_Id loop
               if New_Random_Weights then
                  Weight.Weight := Random_Range (-Random_Weight_Range, Random_Weight_Range);
               else
                  Connection_Io.Read (File => Weight_File, Item => Weight);
               end if;

               Output.Set_Input_Weight (Node => Output_Node (O_Id), From => I_Id, Weight => Weight);
            end loop To_Output;
         end if;
      end loop From_Inputs;

      From_Hidden : for H_Id in Hidden_Id loop
         if New_Random_Weights then
            Weight.Weight := Random_Range (-Random_Weight_Range, Random_Weight_Range);
         else
            Connection_Io.Read (File => Weight_File, Item => Weight);
         end if;

         Hidden.Set_Bias_Weight (Node => Hidden_Node (H_Id), Weight => Weight);

         Hidden_To_Output : for O_Id in Output_Id loop
            if New_Random_Weights then
               Weight.Weight := Random_Range (-Random_Weight_Range, Random_Weight_Range);
            else
               Connection_Io.Read (File => Weight_File, Item => Weight);
            end if;

            Output.Set_Hidden_Weight (Node => Output_Node (O_Id), From => H_Id, Weight => Weight);
         end loop Hidden_To_Output;
      end loop From_Hidden;

      Output_Bias : for O_Id in Output_Id loop
         if New_Random_Weights then
            Weight.Weight := Random_Range (-Random_Weight_Range, Random_Weight_Range);
         else
            Connection_Io.Read (File => Weight_File, Item => Weight);
         end if;

         Output.Set_Bias_Weight (Node => Output_Node (O_Id), Weight => Weight);
      end loop Output_Bias;

      if not New_Random_Weights then
         Connection_Io.Close (File => Weight_File);
      end if;

      -- Pass each pattern through the network to obtain initial response (D zero)
      All_Patterns : for Pattern in Desired'range loop
         Respond (Pattern => Pattern, Output => Desired (Pattern) );
      end loop All_Patterns;
   end REM_NN;
end PragmARC.REM_NN_Wrapper;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.