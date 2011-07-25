with My_Io, Rnd, Argument;

-- Outputs one random among arguments
procedure One_Of_Arg is
begin
  if Argument.Get_Nbre_Arg = 0 then
    return;
  end if;
  Rnd.Randomize;

  My_Io.Put_Line (String'(Argument.Get_Parameter(
         Rnd.Int_Random(1, Argument.Get_Nbre_Arg))));
end One_Of_Arg;

