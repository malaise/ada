-- List directories
-- Usage: ddir [ { <path> } ]
with As.U, Basic_Proc, Argument, Directory;
procedure Ddir is

  procedure Ddir_One (Dir_Name : in String) is
    Dir_Dsc : Directory.Dir_Desc;
    Entry_Name : As.U.Asu_Us;
    Kind : Directory.File_Kind_List;
    use type Directory.File_Kind_List;
  begin
    begin
      Dir_Dsc.Open (Dir_Name);
    exception
      when Directory.Name_Error =>
        Basic_Proc.Put_Line_Output ("ERROR no such directory " & Dir_Name);
        return;
      when Directory.Access_Error =>
        Basic_Proc.Put_Line_Output ("ERROR reading directory " & Dir_Name);
        return;
    end;

    Basic_Proc.Put_Line_Output ("Directories of " & Dir_Name);
    loop
      begin
        Dir_Dsc.Next_Entry (Entry_Name);
      exception
        when Directory.End_Error =>
          exit;
      end;
      begin
        Kind := Directory.File_Kind (Dir_Name & "/" & Entry_Name.Image);
      exception
        when Directory.Name_Error | Directory.Access_Error =>
          -- A link to nowhere?
          Kind := Directory.Unknown;
      end;
      if Kind = Directory.Dir
      and then Entry_Name.Image /= "."
      and then Entry_Name.Image /= ".." then
        Basic_Proc.Put_Line_Output (Entry_Name.Image);
      end if;
    end loop;
    Dir_Dsc.Close;
    Basic_Proc.New_Line_Output;
  end Ddir_One;

begin
  if Argument.Get_Nbre_Arg = 0 then
    Ddir_One (".");
  else
    for I in 1 .. Argument.Get_Nbre_Arg loop
      Ddir_One (Argument.Get_Parameter(I));
    end loop;
  end if;
end Ddir;

