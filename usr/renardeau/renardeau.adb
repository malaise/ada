-- With 6 random integers (1 to 9 or 25, 50, 75 or 100)
-- With the 4 operations + - / * on integers
-- With a random target integer number (100 to 999)
-- Try to obtain the target or to get as close as possible to it
with Ada.Unchecked_Deallocation;
with Argument, Images, Bit_Ops, Unbounded_Arrays, Basic_Proc;
procedure Renardeau is

  -- Internal representation
  subtype Num is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

  -- Number of base number
  Nb_Bases : constant := 6;
  subtype Bases_Range is Positive range 1 .. Nb_Bases;

  -- Mask of used based number
  subtype Used_Mask is Natural range 0 .. 2**Nb_Bases - 1;

  -- Operations
  type Operations is (Add, Mul, Sub, Div, Nop);
  subtype Real_Operations is Operations range Add .. Div;
  Operations_Images : constant array (Real_Operations) of Character
                    := ('+', '*', '-', '/');

  -- Cell of intermediate result
  type Cell;
  type Cell_Access is access Cell;
  type Cell is record
    Value : Num := 0;
    Left : Cell_Access;
    Operation : Operations := Nop;
    Right : Cell_Access;
    Used : Used_Mask := 0;
    Next : Cell_Access;
  end record;
  procedure Free is new Ada.Unchecked_Deallocation (Cell, Cell_Access);

  -- X mode
  type Bases_Array is array (Bases_Range) of Positive;
  type Output_Rec is record
    Left : Positive;
    Operation : Real_Operations;
    Right : Positive;
    Result : Positive;
  end record;
  No_Operation : constant Output_Rec := (1, Sub, 1, 1);
  function Is_No_Operation (Output : Output_Rec) return Boolean is
    Tmp : Output_Rec := Output;
  begin
    Tmp.Result := No_Operation.Result;
    return Tmp = No_Operation;
  end Is_No_Operation;

  type Output_Array is array (Positive range <>) of Output_Rec;
  package Unbounded_Ouputs is new Unbounded_Arrays (Output_Rec, Output_Array);


  package X is
    procedure Get_Inputs (Bases : out Bases_Array;
                          Target : out Positive;
                          Cancel : out Boolean);
    procedure Put_Outputs (Found : Boolean;
                           Outputs : in Unbounded_Ouputs.Unbounded_Array;
                           Finish : out Boolean);
  end X;
  package body X is separate;

  -- Inputs
  Invalid_Argument : exception;
  X_Mode : Boolean;
  procedure Get_Inputs (Bases : out Bases_Array;
                        Target : out Positive;
                        Cancel : out Boolean) is
  begin
    X_Mode := Argument.Get_Nbre_Arg = 0;
    -- Afpx option
    if X_Mode then
      X.Get_Inputs (Bases, Target, Cancel);
    else
      Cancel := False;
      if Argument.Get_Nbre_Arg /= 7 then
        Basic_Proc.Put_Line_Error ("Invalid arguments.");
        raise Invalid_Argument;
      end if;
      for I in Bases'Range loop
        begin
          Bases(I) := Positive'Value(Argument.Get_Parameter (I));
          if Bases(I) > 9
          and then Bases(I) /= 10 and then Bases(I) /= 25
          and then Bases(I) /= 50 and then Bases(I) /= 75
          and then Bases(I) /= 100 then
            raise Constraint_Error;
          end if;
        exception
          when Constraint_Error =>
            Basic_Proc.Put_Line_Error ("Invalid argument "
                     & Argument.Get_Parameter (Occurence => I) & ".");
          raise Invalid_Argument;
        end;
      end loop;
      begin
        Target := Positive'Value(Argument.Get_Parameter (Nb_Bases + 1));
        if Target < 100 or else Target > 999 then
          raise Constraint_Error;
        end if;
      exception
        when Constraint_Error =>
          Basic_Proc.Put_Line_Error ("Invalid argument "
                   & Argument.Get_Parameter (Occurence => Nb_Bases + 1)
                   & ".");
        raise Invalid_Argument;
      end;
    end  if;
  end Get_Inputs;

  -- Output
  procedure Put_Outputs (Found : Boolean;
                         Outputs : in Unbounded_Ouputs.Unbounded_Array;
                         Finish : out Boolean) is
    Output : Output_Rec;
  begin
    -- Afpx_Option
    if X_Mode then
      X.Put_Outputs (Found, Outputs, Finish);
    else
      Finish := True;
      if Found then
        Basic_Proc.Put_Line_Output ("Found");
      else
        Basic_Proc.Put_Line_Output ("Closest");
      end if;
      if Is_No_Operation (Outputs.Element(1)) then
        Basic_Proc.Put_Line_Output (
                 Images.Integer_Image (Outputs.Element(1).Result));
      else
        for I in 1 .. Outputs.Length loop
          Output := Outputs.Element(I);
          Basic_Proc.Put_Line_Output (
            Images.Integer_Image (Output.Left) & " "
          & Operations_Images (Output.Operation) & " "
          & Images.Integer_Image (Output.Right) & " = "
          & Images.Integer_Image (Output.Result));
        end loop;
      end if;
    end if;
  end Put_Outputs;

  -- Set the outputs array
  -- If no computation necessary, then set one output with 1 - 1
  --  and the result set
  function Set_Outputs (Result : Cell_Access)
           return Unbounded_Ouputs.Unbounded_Array is
    Outputs : Unbounded_Ouputs.Unbounded_Array;
    procedure Add_Output (C : in Cell_Access) is
    begin
      if C.Operation = Nop then
        -- End of the chain
        return;
      end if;
      Add_Output (C.Left);
      Add_Output (C.Right);
      -- Append current cell
      Outputs.Append (Output_Rec'(Left => Natural(C.Left.Value),
                                  Operation => C.Operation,
                                  Right => Natural(C.Right.Value),
                                  Result => Natural(C.Value)));
    end Add_Output;
    Output : Output_Rec;
  begin
    Add_Output (Result);
    if Outputs.Length = 0 then
      -- No operation necessary
      Output := No_Operation;
      Output.Result := Positive(Result.Value);
      Outputs.Append (Output);
    end if;
    return Outputs;
  end Set_Outputs;

  -- See if 2 Cell use a common Base
  procedure Check_Reuse (C1, C2 : in Cell_Access;
                         Ok : out Boolean;
                         Used : out Used_Mask) is
    use Bit_Ops; --## rule line off use
  begin
    Used := 0;
    if (C1.Used and C2.Used) /= 0 then
      -- C1 and C2 come from a common Base
      Ok := False;
    else
      -- Merge ancestors
      Used := C1.Used or C2.Used;
      Ok := True;
    end if;
  end Check_Reuse;

  -- Combine 2 Cells with an operation, return null if impossible
  -- /!\ MUST Left.Value >= Right.Value
  function Combine (Left, Right : Cell_Access;
                    Operation : Real_Operations) return Cell_Access is
    Value : Num;
  begin
    case Operation is
      when Add => Value := Left.Value + Right.Value;
      when Mul => Value := Left.Value * Right.Value;
      when Sub =>
        if Left.Value = Right.Value then
          -- Prevent reaching 0
          return null;
        end if;
        Value := Left.Value - Right.Value;
      when Div =>
        if Left.Value rem Right.Value /= 0 then
          -- Integer division only
          return null;
        end if;
        Value := Left.Value / Right.Value;
    end case;
    return new Cell'(
        Value => Value,
        Left => Left,
        Operation => Operation,
        Right => Right,
        Used => 0,
        Next => null);
  end Combine;

  -- Combine all operations between 2 Cells, append them
  procedure Append (Start, Current : in out Cell_Access;
                    Left, Right : in Cell_Access) is
    Reuse_Ok : Boolean;
    Used : Used_Mask;
    L, R : Cell_Access;
    Result : Cell_Access;
  begin
    -- Check that Left and Right don't have a common Base ancestor
    Check_Reuse (Left, Right, Reuse_Ok, Used);
    if not Reuse_Ok then
      return;
    end if;
    -- Sort values so that Left.Value >= Right.Value
    if Left.Value >= Right.Value then
      L := Left;
      R := Right;
    else
      L := Right;
      R := Left;
    end if;
    -- Append the combination by each operation
    for Oper in Real_Operations loop
      Result := Combine (L, R, Oper);
      if Result /= null then
        Result.Used := Used;
        -- Chain and point to new
        if Start = null then
          Start := Current;
        end if;
        if Current /= null then
          Current.Next := Result;
        end if;
        Current := Result;
      end if;
    end loop;
  end Append;

  -- Compute complexity of a cell
  function Compute_Complexity (Current : Cell_Access) return Natural is
    (if Current = null then 0
     else Compute_Complexity (Current.Left) * 10
        + Compute_Complexity (Current.Right) * 10
        + 1);

  -- Inputs
  Bases : Bases_Array;
  Target : Positive;

  -- Go on or not?
  Done : Boolean;

  -- Lowest complexity
  Lowest_Complexity_Init : constant Natural := 10 ** (Nb_Bases + 1);

  -- Lowest result
  Lowest_Result_Init : constant := 1000;
  Lowest_Result : Num;

  -- Current, Previous, Left, Right, Result Cells
  Curr, Prev, Left, Right, Result : Cell_Access;

  -- Results
  Results : array (Bases_Range) of Cell_Access;
begin
  loop
    -- Get inputs
    Get_Inputs (Bases, Target, Done);
    exit when Done;

    -- Init Results(0) with a list of Base numbers
    Prev := null;
    for I in Bases_Range loop
      Curr := new Cell;
      Curr.Value := Num(Bases(I));
      Curr.Used := Bit_Ops.Shl (1, I - 1);
      if Prev = null then
        Results(1) := Curr;
      else
        Prev.Next := Curr;
      end if;
      Prev := Curr;
    end loop;

    -- Build all Results
    -- Results(2): 2 Base numbers = Results(1) X Results(1)
    Curr := null;
    Left := Results(1);
    while Left /= null loop
      Right := Left.Next;
      while Right /= null loop
        Append (Results(2), Curr, Left, Right);
        Right := Right.Next;
      end loop;
      Left := Left.Next;
    end loop;
    -- Results(3): 3 Base numbers = Results(2) X Results(1)
    Curr := null;
    Left := Results(2);
    while Left /= null loop
      Right := Results(1);
      while Right /= null loop
        Append (Results(3), Curr, Left, Right);
        Right := Right.Next;
      end loop;
      Left := Left.Next;
    end loop;
    -- Results(4): 4 Base numbers = Results(2) X Results(2)
    --                            + Results(3) X Results(1)
    Curr := null;
    Left := Results(2);
    while Left /= null loop
      Right := Left.Next;
      while Right /= null loop
        Append (Results(4), Curr, Left, Right);
        Right := Right.Next;
      end loop;
      Left := Left.Next;
    end loop;
    Left := Results(3);
    while Left /= null loop
      Right := Results(1);
      while Right /= null loop
        Append (Results(4), Curr, Left, Right);
        Right := Right.Next;
      end loop;
      Left := Left.Next;
    end loop;
    -- Results(5): 5 Base numbers = Results(3) X Results(2)
    --                            + Results(4) X Results(1)
    Curr := null;
    Left := Results(3);
    while Left /= null loop
      Right := Results(2);
      while Right /= null loop
        Append (Results(5), Curr, Left, Right);
        Right := Right.Next;
      end loop;
      Left := Left.Next;
    end loop;
    Left := Results(4);
    while Left /= null loop
      Right := Results(1);
      while Right /= null loop
        Append (Results(5), Curr, Left, Right);
        Right := Right.Next;
      end loop;
      Left := Left.Next;
    end loop;
    -- Results(6): 6 Base numbers = Results(3) X Results(3)
    --                            + Results(4) X Results(2)
    --                            + Results(5) X Results(1)
    Curr := null;
    Left := Results(3);
    while Left /= null loop
      Right := Left.Next;
      while Right /= null loop
        Append (Results(6), Curr, Left, Right);
        Right := Right.Next;
      end loop;
      Left := Left.Next;
    end loop;
    Left := Results(4);
    while Left /= null loop
      Right := Results(2);
      while Right /= null loop
        Append (Results(6), Curr, Left, Right);
        Right := Right.Next;
      end loop;
      Left := Left.Next;
    end loop;
    Left := Results(5);
    while Left /= null loop
      Right := Results(1);
      while Right /= null loop
        Append (Results(6), Curr, Left, Right);
        Right := Right.Next;
      end loop;
      Left := Left.Next;
    end loop;

    -- Search best result with lowest complexity
    declare
      Lowest_Complexity : Natural := Lowest_Complexity_Init;
      Complexity : Natural;
      Num_Target : constant Num := Num(Target);
    begin
      Lowest_Result := Lowest_Result_Init;
      for Res of Results loop
        while Res /= null loop
          if abs (Res.Value - Num_Target) < Lowest_Result then
            -- Store if best result so far
            Result := Res;
            Lowest_Result := abs (Res.Value - Num_Target);
          end if;
          if Res.Value = Num_Target then
            -- Store if Target reached and best complexity so far
            Complexity := Compute_Complexity (Res);
            if Complexity < Lowest_Complexity then
              Lowest_Complexity := Complexity;
              Result := Res;
            end if;
          end if;
          Res := Res.Next;
        end loop;
     end loop;
   end;

    -- Output result
    Put_Outputs (Lowest_Result = 0, Set_Outputs(Result), Done);

    -- Cleanup
    for I in Results'Range loop
      Prev := null;
      Curr := Results(I);
      Results(I) := null;
      loop
        if Prev /= null then
          Free (Prev);
        end if;
        exit when Curr = null;
        Prev := Curr;
        Curr := Curr.Next;
      end loop;
    end loop;

    exit when Done;

  end loop;
exception
  when Invalid_Argument =>
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
      & " [ <bases> <target> ]");
    Basic_Proc.Put_Line_Error ("  <bases>  ::= <base>{6}");
    Basic_Proc.Put_Line_Error ("  <base>   ::= 1 .. 10 | 25 | 50 | 75 | 100");
    Basic_Proc.Put_Line_Error ("  <target> ::= 100 .. 999");
    Basic_Proc.Set_Error_Exit_Code;
end Renardeau;

