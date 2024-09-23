with As.U.Utils, Basic_Proc, Argument;
with Database, Analist;
procedure T_Analist is

  Anagrams : As.U.Utils.Asu_Ua.Unb_Array;
  Start : Positive;
  Nouns : Boolean;

begin
  if Argument.Get_Nbre_Arg = 0 then
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
                     & " <words_file> <nouns_file> [ -n ] { <letters> }");
    return;
  end if;
  Basic_Proc.Put_Line_Output ("Loading...");
  Database.Init (Argument.Get_Parameter (1), Argument.Get_Parameter (2));
  if Argument.Is_Set (3) and then Argument.Get_Parameter (3) = "-n" then
    Nouns := True;
    Start := 4;
  else
    Nouns := False;
    Start := 3;
  end if;
  Basic_Proc.Put_Line_Output ("Loaded.");
  for I in Start .. Argument.Get_Nbre_Arg loop
    Analist.List (Argument.Get_Parameter (I), Nouns, Anagrams);
    Basic_Proc.Put_Line_Output (Argument.Get_Parameter (Occurence => I)
                                & " -->");
    for J in 1 .. Anagrams.Length loop
      Basic_Proc.Put_Line_Output (Anagrams.Element(J).Image);
    end loop;
  end loop;
end T_Analist;

