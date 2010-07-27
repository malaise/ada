with Ada.Text_Io;

with Text_Handler, Argument, Upper_Char, Get_Float, My_Math, Euro_Franc;
use type My_Math.Real;

procedure E2F is


  package Real_Ef is new Euro_Franc (My_Math.Real, My_Math.Real);

  Amount : My_Math.Real;
  To_Francs : Boolean;

  The_Argument : Text_Handler.Text(20);

  procedure Help is
    use Ada.Text_Io;
  begin
    Put_Line("Usage: " & Argument.Get_Program_Name
                       & " <amount>[<unit>]");
    Put_Line("   <amount> : integer or float (2 digits)");
    Put_Line("   <unit>   : f or e");
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
    Len : Natural;
    Unit : Character;
    Dot : Natural;
  begin
    Argument.Get_Parameter(The_Argument);

    -- Check unit, get way of conversion
    Len := Text_Handler.Length(The_Argument);
    Unit := Upper_Char(Text_Handler.Value(The_Argument)(Len));
    if Unit = 'E' then
      To_Francs := True;
      Len := Len - 1;
    elsif Unit = 'F' then
      To_Francs := False;
      Len := Len - 1;
    elsif Unit in '0' .. '9' then
       To_Francs := True;
    else
      raise Constraint_Error;
    end if;

    -- Isolate the value
    Text_Handler.Set(The_Argument,
                     Text_Handler.Value(The_Argument)(1 .. Len));

    -- If value is float, must have 2 digits for cents
    Dot := Text_Handler.Locate(The_Argument, '.', 1);
    if Dot /= 0 and then Dot + 2 /= Len then
      raise Constraint_Error;
    end if;

    -- Get it in Real format
    Amount := My_Math.Real(Get_Float.Get_Float(
                   Text_Handler.Value(The_Argument)));

  exception
    when others =>
      Ada.Text_Io.Put_Line("Error, invalid argument: "
                          & Argument.Get_Parameter);
      Help;
      return;
  end;

  Put(Amount);
  if To_Francs then
    Ada.Text_Io.Put("e");
  else
    Ada.Text_Io.Put("f");
  end if;
  Ada.Text_Io.Put(" = ");
  if To_Francs then
    Put(Real_Ef.Euros_To_Francs(Amount));
    Ada.Text_Io.Put_Line("f");
  else
    Put(Real_Ef.Francs_To_Euros(Amount));
    Ada.Text_Io.Put_Line("e");
  end if;

end E2F;

