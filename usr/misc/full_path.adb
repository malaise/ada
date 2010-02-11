with Basic_Proc, Argument, Directory, Normalize_Path;
procedure Full_Path is

begin
  -- Check one arg not empty
  if Argument.Get_Nbre_Arg /= 1
  or else Argument.Get_Parameter (Occurence => 1) = "" then
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                                         & " <path>");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Put full absolute path
  declare
    Arg : constant String := Argument.Get_Parameter (Occurence => 1);
  begin
    if Arg(Arg'First) = '/' then
      -- Arg is a full path
      Basic_Proc.Put_Line_Output (Normalize_Path (Arg));
    else
      -- Arg is a relative path, prepend current path
      Basic_Proc.Put_Line_Output (Normalize_Path (
          Directory.Get_Current & "/" & Arg));
    end if;
  end;
end Full_Path;

