with Argument, Basic_Proc, Mixed_Str;
with File_Hash;
procedure T_File_Hash is
  Res : Boolean;
begin
  if Argument.Get_Nbre_Arg = 0 then
    return;
  end if;
  File_Hash.Init (Argument.Get_Parameter (1));
  for I in 2 .. Argument.Get_Nbre_Arg loop
    begin
      Basic_Proc.Put_Output (Argument.Get_Parameter(Occurence => I) & " -> ");
      Res := File_Hash.Exists (Argument.Get_Parameter(I));
      Basic_Proc.Put_Line_Output (Mixed_Str (Res'Img));
    exception
      when File_Hash.Too_Long =>
        Basic_Proc.Put_Line_Output ("NOT FOUND");
    end;
  end loop;
end T_File_Hash;

