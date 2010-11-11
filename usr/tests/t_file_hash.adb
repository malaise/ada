with As.U; use As.U;
with Argument, Basic_Proc, File_Hash;
procedure T_File_Hash is
  List : File_Hash.List_Mng.List_Type;
  Word : Asu_Us;
  Found : Boolean;
begin

  if Argument.Get_Nbre_Arg = 0 then
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                             & " <file> [ { <word> } ]");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  File_Hash.Load (Argument.Get_Parameter (1), List);

  for I in 2 .. Argument.Get_Nbre_Arg loop
    Word := Asu_Tus (Argument.Get_Parameter (Occurence => I));
    Basic_Proc.Put_Line_Output (Asu_Ts (Word) & " -> ");
    List.Search_First (Word, Found);
    if Found then
      List.Read_Current (Word);
      Basic_Proc.Put_Line_Output ("FOUND " & Asu_Ts (Word));
    else
      Basic_Proc.Put_Line_Output ("NOT FOUND");
    end if;
  end loop;

end T_File_Hash;

