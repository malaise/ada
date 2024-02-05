with Argument, Basic_Proc;
with Types, Scrambler_Gen;
-- Generate a random scrambler definition for enigma config file
-- Optionnaly, the scambler can be symetrical, for reflector
procedure Scrambler_Generator is

  -- Error/Usage
  procedure Error is
  begin
    Basic_Proc.Put_Line_Error ("ERROR. Usage " & Argument.Get_Program_Name
       & " [ -s ]");
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- Shall we generate a symetric scrambler or not
  Symetric : Boolean := False;

  subtype Id_Range is Positive range 1 .. Positive(Types.Lid'Last) + 1;
  Scrambler : String (Id_Range);

begin
  -- One optionnal "-s"
  if Argument.Get_Nbre_Arg > 1
  or else (Argument.Get_Nbre_Arg = 1
           and then Argument.Get_Parameter /= "-s") then
    Error;
    return;
  end if;
  Symetric := Argument.Get_Nbre_Arg = 1;

  -- Compute random scrambler
  Scrambler := Scrambler_Gen.Generate (Symetric);

  -- Put reference then map
  for C in Types.Letter loop
    Basic_Proc.Put_Output (C);
  end loop;
  Basic_Proc.New_Line_Output;
  for I in Id_Range loop
    Basic_Proc.Put_Output (Scrambler(I));
  end loop;
  Basic_Proc.New_Line_Output;

end Scrambler_Generator;

