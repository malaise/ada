-- For each file provided as argument consider it contains a list of Ada
--  files (one per line) and put the number of Ada statements of these files,
--  then put the total.
with Argument, Basic_Proc, Text_Line;
with One_File_Statements;

procedure Statfile is

  First_File : Positive;

  procedure Stat_One_File (List_File_Name : in String) is
    List_File : Text_Line.File_Type;
  begin
    begin
      List_File.Open_All (Text_Line.In_File, List_File_Name);
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Exception raised when opening list file "
                        & List_File_Name & " SKIPPING");
        return;
    end;

    loop
      declare
        Name : constant String := List_File.Get;
      begin
        exit when Name = "";
        if Name'Length /= 1 then
          One_File_Statements.Print_Statements_Of_File (Text_Line.Trim (Name));
        end if;
      exception
        when others =>
          Basic_Proc.Put_Line_Error (
               "Exception raised when reading list file "
             & List_File_Name & " SKIPPING");
          List_File.Close_All;
          return;
      end;
    end loop;
    List_File.Close_All;
  end Stat_One_File;

begin

  if Argument.Get_Nbre_Arg >= 1
  and then (Argument.Get_Parameter (1) = "--help"
            or else Argument.Get_Parameter (1) = "-h") then
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
      & " [ -j | --java] { <file_name> }");
    return;
  end if;

  -- Parse option -j | --java
  First_File := 1;
  if Argument.Get_Nbre_Arg >= 1
  and then (Argument.Get_Parameter (1) = "--java"
            or else Argument.Get_Parameter (1) = "-j") then
    One_File_Statements.Java_Syntax := True;
    First_File := First_File + 1;
  end if;

  -- Process files of files
  for Arg in First_File .. Argument.Get_Nbre_Arg loop
    Basic_Proc.Put_Line_Output ("Processing list file "
        & String'(Argument.Get_Parameter (Arg)));
    Stat_One_File (Argument.Get_Parameter (Arg));
  end loop;

  One_File_Statements.Print_Statements_Of_File ("");

end Statfile;

