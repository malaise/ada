with Argument, Int_Image, Bit_Ops, Unbounded_Arrays, Basic_Proc;
procedure Renardeau is
  function Image is new Int_Image (Natural);

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
    Value : Natural := 0;
    Left : Cell_Access;
    Operation : Operations := Nop;
    Right : Cell_Access;
    Used : Used_Mask := 0;
    Next : Cell_Access;
  end record;

  -- Inputs
  type Bases_Array is array (Bases_Range) of Positive;
  Invalid_Argument : exception;
  Afpx_Mode : Boolean;
  procedure Get_Inputs (Bases : out Bases_Array; Target : out Positive) is
  begin
    -- Afpx option
    Afpx_Mode := False;
    if Afpx_Mode then
      null;
    else
      if Argument.Get_Nbre_Arg /= 7 then
        raise Invalid_Argument;
      end if;
      for I in Bases'Range loop
        Bases(I) := Positive'Value(Argument.Get_Parameter (I));
      end loop;
      Target := Positive'Value(Argument.Get_Parameter (Nb_Bases + 1));
    end  if;
  exception
    when others =>
      raise Invalid_Argument;
  end Get_Inputs;

  -- Output
  type Output_Rec is record
    Left : Positive;
    Operation : Real_Operations;
    Right : Positive;
    Result : Positive;
  end record;
  type Output_Array is array (Positive range <>) of Output_Rec;
  package Unbounded_Ouputs is new Unbounded_Arrays (Output_Rec, Output_Array);
  procedure Put_Outputs (Found : Boolean;
                         Outputs : in Unbounded_Ouputs.Unbounded_Array;
                         Done : out Boolean) is
    Output : Output_Rec;
  begin
    -- Afpx_Option
    if Afpx_Mode then
      null;
    else
      if Found then
        Basic_Proc.Put_Line_Output ("Found");
      else
        Basic_Proc.Put_Line_Output ("Closest");
      end if;
      for I in 1 .. Outputs.Length loop
        Output := Outputs.Element (I);
        Basic_Proc.Put_Line_Output (
          Image (Output.Left) & " " & Operations_Images (Output.Operation) & " "
        & Image (Output.Right) & " = " & Image (Output.Result));
      end loop;
      Done := True;
    end if;
  end Put_Outputs;

  -- Set the outputs array
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
      Outputs.Append (Output_Rec'(Left => C.Left.Value,
                                  Operation => C.Operation,
                                  Right => C.Right.Value,
                                  Result => C.Value));
    end Add_Output;
  begin
    Add_Output (Result);
    return Outputs;
  end Set_Outputs;

  -- See if 2 Cell use a common Base
  procedure Check_Reuse (C1, C2 : in Cell_Access;
                         Ok : out Boolean;
                         Used : out Used_Mask) is
    use Bit_Ops;
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
    Value : Positive;
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
  procedure Append (Current : in out Cell_Access;
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
        if Current /= null then
          Current.Next := Result;
        end if;
        Current := Result;
      end if;
    end loop;
  end Append;

  -- Compute complexity of a cell
  function Compute_Complexity (Current : Cell_Access) return Natural is
  begin
    if Current = null then
      return 0;
    else
      return Compute_Complexity (Current.Left) * 10
           + Compute_Complexity (Current.Right) * 10
           + 1;
    end if;
  end Compute_Complexity;

  -- Inputs
  Bases : Bases_Array;
  Target : Positive;

  -- Go on or not?
  Done : Boolean;

  -- Lowest complexity
  Lowest_Complexity_Init : constant Natural := 10 ** (Nb_Bases + 1);

  -- Lowest result
  Lowest_Result_Init : constant := 1000;
  Lowest_Result : Natural;

  -- Current, Previous, Left, Right, Result Cells
  Curr, Prev, Left, Right, Result : Cell_Access;

  -- Results
  Results : array (Bases_Range) of Cell_Access;
begin
  loop
    -- Get inputs
    Get_Inputs (Bases, Target);

    -- Init Results(0) with a list of Base numbers
    Prev := null;
    for I in Bases_Range loop
      Curr := new Cell;
      Curr.Value := Bases(I);
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
        Append (Curr, Left, Right);
        if Results(2) = null then
          Results(2) := Curr;
        end if;
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
        Append (Curr, Left, Right);
        if Results(3) = null then
          Results(3) := Curr;
        end if;
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
        Append (Curr, Left, Right);
        if Results(4) = null then
          Results(4) := Curr;
        end if;
        Right := Right.Next;
      end loop;
      Left := Left.Next;
    end loop;
    Left := Results(3);
    while Left /= null loop
      Right := Results(1);
      while Right /= null loop
        Append (Curr, Left, Right);
        if Results(4) = null then
          Results(4) := Curr;
        end if;
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
        Append (Curr, Left, Right);
        if Results(5) = null then
          Results(5) := Curr;
        end if;
        Right := Right.Next;
      end loop;
      Left := Left.Next;
    end loop;
    Left := Results(4);
    while Left /= null loop
      Right := Results(1);
      while Right /= null loop
        Append (Curr, Left, Right);
        if Results(5) = null then
          Results(5) := Curr;
        end if;
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
        Append (Curr, Left, Right);
        if Results(6) = null then
          Results(6) := Curr;
        end if;
        Right := Right.Next;
      end loop;
      Left := Left.Next;
    end loop;
    Left := Results(4);
    while Left /= null loop
      Right := Results(2);
      while Right /= null loop
        Append (Curr, Left, Right);
        if Results(6) = null then
          Results(6) := Curr;
        end if;
        Right := Right.Next;
      end loop;
      Left := Left.Next;
    end loop;
    Left := Results(5);
    while Left /= null loop
      Right := Results(1);
      while Right /= null loop
        Append (Curr, Left, Right);
        if Results(6) = null then
          Results(6) := Curr;
        end if;
        Right := Right.Next;
      end loop;
      Left := Left.Next;
    end loop;
  
    -- Search best result with lowest complexity
    declare
      Lowest_Complexity : Natural := Lowest_Complexity_Init;
      Complexity : Natural;
    begin
      Lowest_Result := Lowest_Result_Init;
      for I in Results'Range loop
        Curr := Results(I);
        while Curr /= null loop
          if abs (Curr.Value - Target) < Lowest_Result then
            -- Store if best result so far
            Result := Curr;
            Lowest_Result := abs (Curr.Value - Target);
          end if;
          if Curr.Value = Target then
            -- Store if Target reached and best complexity so far
            Complexity := Compute_Complexity (Curr);
            if Complexity < Lowest_Complexity then
              Lowest_Complexity := Complexity;
              Result := Curr;
            end if;
          end if;
          Curr := Curr.Next;
        end loop;
     end loop; 
   end;

    -- Output result
    Put_Outputs (Lowest_Result = 0, Set_Outputs(Result), Done);
    exit when Done;

    -- Cleanup

  end loop;
  
end Renardeau;

