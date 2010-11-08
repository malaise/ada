with As.U; use As.U;
with Basic_Proc, Argument;
with Analist;
procedure T_Analist is

  Anagrams : Asu_Ua.Unb_Array;

begin
  if Argument.Get_Nbre_Arg = 0 then
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
                              & " <dictio_file> { <letters> }");
    return;
  end if;
  Basic_Proc.Put_Line_Output ("Loading...");
  Analist.Init (Argument.Get_Parameter (1));
  Basic_Proc.Put_Line_Output ("Loaded.");
  for I in 2 .. Argument.Get_Nbre_Arg loop
    Analist.List (Argument.Get_Parameter (I), Anagrams);
    Basic_Proc.Put_Line_Output (Argument.Get_Parameter (Occurence => I)
                                & " -->");
    for J in 1 .. Anagrams.Length loop
      Basic_Proc.Put_Line_Output (Asu_Ts (Anagrams.Element (J)));
    end loop;
  end loop;
end T_Analist;

