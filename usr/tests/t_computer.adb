-- Set some variables in Computer and then evaluate and compute an expression
-- Variables and expression are defined as arguments
with Basic_Proc, Argument, Computer.Computation, Str_Util, Arbitrary;
procedure T_Computer is
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
     & " [ { <name>=<value> } ] <expression>");
  end Usage;

  Loc : Natural;
  N : Arbitrary.Number;
  Mem : Computer.Computation.Memory_Type;
begin
  if Argument.Get_Nbre_Arg = 0 then
    Usage;
    return;
  end if;
  Mem.Set_External_Resolver (Computer.Env_Resolver);
  -- All args but last are variables
  for I in 1 .. Argument.Get_Nbre_Arg - 1 loop
    declare
      Str : constant String := Argument.Get_Parameter(I);
    begin
      Loc := Str_Util.Locate (Str, "=");
      if Loc = 0 or else Loc = 1 then
        raise Constraint_Error;
      end if;
      Mem.Set (Str (1 .. Loc - 1), Str (Loc + 1 .. Str'Last),
                    Modifiable => False, Persistent => I rem 2 = 0);
    end;
  end loop;

  -- Last arg is the expression
  -- Evaluate expression
  Basic_Proc.Put_Line_Output ("Evaluation: ");
  Basic_Proc.Put_Line_Output (Mem.Eval (
         Argument.Get_Parameter(Argument.Get_Nbre_Arg)));
  -- Try to compute expression
  Basic_Proc.Put_Line_Output ("Computation: ");
  N := Mem.Compute (Argument.Get_Parameter(Argument.Get_Nbre_Arg));
  Basic_Proc.Put_Line_Output (N.Image);
exception
  when Computer.Invalid_Expression =>
    Basic_Proc.Put_Line_Output ("Invalid expression");
  when others =>
    Usage;
    raise;
end T_Computer;

