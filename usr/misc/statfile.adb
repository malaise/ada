-- For each file provided as argument consider it contains a list of Ada
--  files (one per line) and put the number of Ada statements of these files,
--  then put the total.
with Ada.Text_Io;
with Argument;
with One_File_Statements;

procedure Statfile is

  procedure Stat_One_File (List_File_Name : in String) is
    List_File : Ada.Text_Io.File_Type;
    File_Name : String (1 .. 5000);
    File_Name_Len : Natural;
  begin
    begin
      Ada.Text_Io.Open (List_File, Ada.Text_Io.In_File, List_File_Name);
    exception
      when others =>
        Ada.Text_Io.Put_Line ("Exception raised when opening list file "
                        & List_File_Name & " SKIPPING");
        return;
    end;

    while not Ada.Text_Io.End_Of_File (List_File) loop
      begin
        Ada.Text_Io.Get_Line (List_File, File_Name, File_Name_Len);
      exception
        when others =>
          Ada.Text_Io.Put_Line ("Exception raised when reading line "
                          & Ada.Text_Io.Positive_Count'Image(Ada.Text_Io.Line(List_File))
                          & " of list file " & List_File_Name & " SKIPPING");
          Ada.Text_Io.Close (List_File);
          return;
      end;

      if File_Name_Len /= 0 then
        One_File_Statements.Print_Statements_Of_File(File_Name(1 .. File_Name_Len));
      end if;
    end loop;
    Ada.Text_Io.Close (List_File);
  end Stat_One_File;

begin

  for Arg in 1 .. Argument.Get_Nbre_Arg loop
    Ada.Text_Io.Put_Line ("Processing list file " & String'(Argument.Get_Parameter(Arg)));
    Stat_One_File(Argument.Get_Parameter(Arg));
  end loop;

  One_File_Statements.Print_Statements_Of_File("");

end Statfile;

