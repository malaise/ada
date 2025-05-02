with Basic_Proc, Rnd, Argument;

-- Outputs one random among arguments
procedure One_Of_Arg is
  Dummy : Integer;
begin
  if Argument.Get_Nbre_Arg = 0 then
    return;
  end if;
  Rnd.Gen.Randomize;
  Rnd.Gen.Run (100);

  Basic_Proc.Put_Line_Output (String'(Argument.Get_Parameter(
         Rnd.Gen.Int_Random(1, Argument.Get_Nbre_Arg))));
end One_Of_Arg;

