with Ada.Text_Io;
with Argument, Sys_Calls;
with Types, Scrambler_Gen;
-- Generate a scrambler for enigma config file
procedure Scrambler_Generator is

  -- Error/Usage
  procedure Error is
  begin
    Sys_Calls.Put_Line_Error ("ERROR. Usage " & Argument.Get_Program_Name
       & " [ -s ]");
    Sys_Calls.Set_Error_Exit_Code;
  end Error;

  -- Shall we generate a symetric scrambler or not
  Symetric : Boolean := False;

  subtype Id_Range is Positive range 1 .. Positive(Types.Lid'Last) + 1;
  Scrambler : String (Id_Range'Range);

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
    Ada.Text_Io.Put (C);
  end loop;
  Ada.Text_Io.New_Line;
  for I in Id_Range'Range loop
    Ada.Text_Io.Put (Scrambler(I));
  end loop;

end Scrambler_Generator;

