with As.U; use As.U;
with My_Io, Argument, Directory;
procedure Ddir is

  procedure Ddir_One (Dir_Name : in String) is
    Dir_Dsc : Directory.Dir_Desc;
    Entry_Name : Asu_Us;
    Kind : Directory.File_Kind_List;
    use Directory;
  begin
    begin
      Dir_Dsc := Directory.Open(Dir_Name);
    exception
      when Directory.Name_Error =>
        My_Io.Put_Line ("ERROR no such directory " & Dir_Name);
        return;
      when Directory.Access_Error =>
        My_Io.Put_Line ("ERROR reading directory " & Dir_Name);
        return;
    end;

    My_Io.Put_Line ("Directories of " & Dir_Name);
    loop
      begin
        Directory.Next_Entry (Dir_Dsc, Entry_Name);
      exception
        when Directory.End_Error =>
          exit;
      end;
      begin
        Kind := Directory.File_Kind (Dir_Name & "/" & Asu_Ts (Entry_Name));
      exception
        when Directory.Name_Error | Directory.Access_Error =>
          -- A link to nowhere?
          Kind := Directory.Unknown;
      end;
      if Kind = Directory.Dir
      and then Asu_Ts (Entry_Name) /= "."
      and then Asu_Ts (Entry_Name) /= ".." then
        My_Io.Put_Line (Asu_Ts (Entry_Name));
      end if;
    end loop;
    Directory.Close(Dir_Dsc);
    My_Io.New_Line;
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

