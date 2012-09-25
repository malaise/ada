-- Euro <-> Franc conversion
with As.U, Argument, Upper_Char, Get_Float, My_Math, Euro_Franc, Str_Util,
     Basic_Proc;
use type My_Math.Real;

procedure E2F is

  package Real_Ef is new Euro_Franc (My_Math.Real, My_Math.Real);

  Amount : My_Math.Real;
  To_Francs : Boolean;

  The_Argument : As.U.Asu_Us;

  procedure Help is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
                       & " <amount>[<unit>]");
    Basic_Proc.Put_Line_Output ("   <amount> : integer or float (2 digits)");
    Basic_Proc.Put_Line_Output ("   <unit>   : f or e");
  end Help;

  procedure Put (V : in My_Math.Real) is
    I : My_Math.Inte;
  begin
    I := My_Math.Round(V * 100.0);
    My_Math.Real_Io.Put(My_Math.Real(I) / 100.0, 0, 2, 0);
  end Put;


begin

  -- Check one argument
  if Argument.Get_Nbre_Arg /= 1 then
    Help;
    return;
  end if;

  -- Get it
  declare
    Unit : Character;
    Dot : Natural;
  begin
    Argument.Get_Parameter(The_Argument);

    -- Check unit, get way of conversion
    Unit := Upper_Char(The_Argument.Element (The_Argument.Length));
    if Unit = 'E' then
      To_Francs := True;
      The_Argument.Delete (The_Argument.Length, The_Argument.Length);
    elsif Unit = 'F' then
      To_Francs := False;
      The_Argument.Delete (The_Argument.Length, The_Argument.Length);
    elsif Unit in '0' .. '9' then
       To_Francs := True;
    else
      raise Constraint_Error;
    end if;

    -- If value is float, must have 2 digits for cents
    Dot := Str_Util.Locate (The_Argument.Image, ".", 1);
    if Dot /= 0 and then Dot + 2 /= The_Argument.Length then
      raise Constraint_Error;
    end if;

    -- Get it in Real format
    Amount := My_Math.Real(Get_Float.Get_Float(The_Argument.Image));

  exception
    when others =>
      Basic_Proc.Put_Line_Error ("Error, invalid argument: "
                          & Argument.Get_Parameter);
      Help;
      return;
  end;

  Put(Amount);
  if To_Francs then
    Basic_Proc.Put_Output ("e");
  else
    Basic_Proc.Put_Output ("f");
  end if;
  Basic_Proc.Put_Output (" = ");
  if To_Francs then
    Put(Real_Ef.Euros_To_Francs(Amount));
    Basic_Proc.Put_Line_Output ("f");
  else
    Put(Real_Ef.Francs_To_Euros(Amount));
    Basic_Proc.Put_Line_Output ("e");
  end if;

end E2F;

