with Ada.Text_Io;
with Argument, Computer, String_Mng;
procedure T_Computer is
  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
     & " [ { <name>=<value> } ] <expression>");
  end Usage;
  Loc : Natural;
  N : Integer;
begin
  -- All args but last are variables
  for I in 1 .. Argument.Get_Nbre_Arg - 1 loop
    declare
      Str : constant String := Argument.Get_Parameter(I);
    begin
      Loc := String_Mng.Locate (Str, "=");
      if Loc = 0 or else Loc = 1 or else Loc = Str'Last then
        raise Constraint_Error;
      end if;
      Computer.Set (Str (1 .. Loc - 1), Str (Loc + 1 .. Str'Last));
    end;
  end loop;

  -- Last arg is the expression
  Ada.Text_Io.Put_Line (Computer.Eval (
         Argument.Get_Parameter(Argument.Get_Nbre_Arg)));
  N := Computer.Compute (Argument.Get_Parameter(Argument.Get_Nbre_Arg));
  Ada.Text_Io.Put_Line (N'Img);
exception
  when others =>
    Usage;
    raise;
end T_Computer;

