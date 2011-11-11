-- One or several beeps
-- usage: beep [ [ <number_of beeps> [ <delay_between_beeps> ] ]
-- Defaults are 1 beep and each 0.25s
with Ada.Characters.Latin_1;
with Argument, Basic_Proc;
procedure Beep is
  Nb_Beep : Positive;
  subtype Delta_Beep_Range is Duration range 0.01 .. 1.0;
  Delta_Beep : Delta_Beep_Range;
begin
  if Argument.Get_Nbre_Arg > 2 then
    raise Constraint_Error;
  end if;
  if Argument.Get_Nbre_Arg >= 1 then
    Nb_Beep := Integer'Value(Argument.Get_Parameter(Occurence => 1));
  else
    Nb_Beep := 1;
  end if;
  if Argument.Get_Nbre_Arg >= 2 then
    Delta_Beep := Delta_Beep_Range'Value(Argument.Get_Parameter(Occurence => 2));
  else
    Delta_Beep := 0.25;
  end if;
  for I in 1 .. Nb_Beep loop
    Basic_Proc.Put_Output (Ada.Characters.Latin_1.Bel);
    if I /= Nb_Beep then
      delay Delta_Beep;
    end if;
  end loop;
exception
  when others =>
    Basic_Proc.Put_Output (Ada.Characters.Latin_1.Bel);
end Beep;

