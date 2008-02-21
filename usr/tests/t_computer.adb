with Ada.Text_Io;
with Argument, Computer, String_Mng, Environ;
procedure T_Computer is
  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
     & " [ { <name>=<value> } ] <expression>");
  end Usage;

  Loc : Natural;
  N : Integer;
begin
  if Argument.Get_Nbre_Arg = 0 then
    Usage;
    return;
  end if;
  Computer.External_Resolver := Environ.Getenv_If_Set'Access;
  -- All args but last are variables
  for I in 1 .. Argument.Get_Nbre_Arg - 1 loop
    declare
      Str : constant String := Argument.Get_Parameter(I);
    begin
      Loc := String_Mng.Locate (Str, "=");
      if Loc = 0 or else Loc = 1 or else Loc = Str'Last then
        raise Constraint_Error;
      end if;
      Computer.Set (Str (1 .. Loc - 1), Str (Loc + 1 .. Str'Last),
                    Modifiable => False, Persistent => I rem 2 = 0);
    end;
  end loop;

  -- Last arg is the expression
  -- Evaluate expression
  Ada.Text_Io.Put_Line ("Evaluation: ");
  Ada.Text_Io.Put_Line (Computer.Eval (
         Argument.Get_Parameter(Argument.Get_Nbre_Arg)));
  -- Try to compute expression
  Ada.Text_Io.Put_Line ("Computation: ");
  N := Computer.Compute (Argument.Get_Parameter(Argument.Get_Nbre_Arg));
  Ada.Text_Io.Put_Line (N'Img);
exception
  when Computer.Invalid_Expression =>
    Ada.Text_Io.Put_Line ("Invalid expression");
  when others =>
    Usage;
    raise;
end T_Computer;

