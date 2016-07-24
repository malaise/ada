with Basic_Proc, Argument, As.U, Any_Def, Scanner;
procedure T_Scanner is
  Scan_Format : As.U.Asu_Us;
  Words : Scanner.Any_Sequence;
begin
  if Argument.Get_Nbre_Arg <= 1 then
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        & " <scan_format> { [ <string> ] }");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;
  Scan_Format := As.U.Tus (Argument.Get_Parameter (Occurence => 1));

  Basic_Proc.Put_Output ("Length:");
  begin
    Basic_Proc.Put_Line_Output (
        Natural'Image (Scanner.Length (Scan_Format.Image)));
  exception
    when Scanner.Unknown_Length => Basic_Proc.Put_Line_Output (" Unknown");
  end;

  for I in 2 .. Argument.Get_Nbre_Arg loop
    Basic_Proc.Put_Line_Output ("Input: "
                              & Argument.Get_Parameter (Occurence => I));
    Words := Scanner.Scan (Argument.Get_Parameter (Occurence => I),
                           Scan_Format.Image);
    for J in 1 .. Words.Length loop
      Basic_Proc.Put_Line_Output (" > " & Any_Def.Image (Words.Element (J)));
    end loop;
  end loop;

end T_Scanner;

