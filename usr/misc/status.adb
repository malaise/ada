-- Check if target file: $n is up to date comparing to source files:
--  $1, $2 .. $n-1. This is: target file exists and is newer that sources.
-- exit 0  -->  $n file is ok (newer than all sources)
-- exit 1  -->  $n file is older than some sources or does not exist
-- exit 2  -->  Error: cannot read some source files
-- exit 3  -->  Argument or internal error
with Ada.Calendar;
use Ada.Calendar;
with Argument, Sys_Calls, Directory;
procedure Status is
  -- The exit values
  Exit_Ok             : constant := 0;
  Exit_Nok            : constant := 1;
  Exit_Src_Not_Found  : constant := 2;
  Exit_Internal_Error : constant := 3;

  -- The final exit code
  Exit_Code : Integer;

  -- Info unused but got with File_Stat
  Kind : Directory.File_Kind_List;
  Rights : Natural;
  Fsize  : Directory.Size_T;

  -- Modif time of target, current source
  Target_Mtime, Source_Mtime : Directory.Time_T;
  Target_Time, Source_Time : Ada.Calendar.Time;

begin

  -- Check arguments, at least 2
  if Argument.Get_Nbre_Arg < 2 then
    Sys_Calls.Put_Line_Error("SYNTAX ERROR. Usage: "
                           & Argument.Get_Program_Name
                           & " { <source_file> } <target_file>");
    Sys_Calls.Set_Exit_Code (Exit_Internal_Error);
    return;
  end if;

  -- Initialize final result
  Exit_Code := Exit_Ok;

  -- Check that target file exists and get its modif date
  begin
    Directory.File_Stat(Argument.Get_Parameter(Argument.Get_Nbre_Arg),
                        Kind, Rights, Target_Mtime, Fsize);
  exception
    when Directory.Name_Error =>
      -- Not found
      Exit_Code := Exit_Nok;
    when others =>
      -- Other error
      Sys_Calls.Put_Line_Error(
              "ACCESS ERROR: Cannot read status of target file "
            & String'(Argument.Get_Parameter(Argument.Get_Nbre_Arg)));
      Sys_Calls.Set_Exit_Code(Exit_Internal_Error);
      return;
  end;
  Target_Time := Directory.Time_Of(Target_Mtime);

  -- Check that each source exists and is before result
  for Arg_No in 1 .. Argument.Get_Nbre_Arg - 1 loop

    if String'(Argument.Get_Parameter(Arg_No)) =
       Argument.Get_Parameter(Argument.Get_Nbre_Arg) then
      Sys_Calls.Put_Line_Error(
              "SEMANTIC ERROR: Source file "
            & String'(Argument.Get_Parameter(Arg_No))
            & " is also the target file.");
      Sys_Calls.Put_Line_Error("Usage: "
            & Argument.Get_Program_Name
            & " { <source_file> } <target_file>");
      Sys_Calls.Set_Exit_Code(Exit_Internal_Error);
      return;
    end if;

    begin
      Directory.File_Stat(Argument.Get_Parameter(Arg_No),
                        Kind, Rights, Source_Mtime, Fsize);
    exception
      when Directory.Name_Error =>
        -- Not found
        Sys_Calls.Put_Line_Error(
                "NAME ERROR: Source file "
              & String'(Argument.Get_Parameter(Arg_No))
              & " not found.");
        Sys_Calls.Set_Exit_Code(Exit_Src_Not_Found);
        return;
      when others =>
        -- Other error
        Sys_Calls.Put_Line_Error(
                "ACCESS ERROR: Cannot read status of source file "
              & String'(Argument.Get_Parameter(Arg_No)));
        Sys_Calls.Set_Exit_Code(Exit_Internal_Error);
        return;
    end;
    Source_Time := Directory.Time_Of(Source_Mtime);

    if Exit_Code = Exit_Ok and then Target_Time <= Source_Time then
      -- Source files exist so far and this one is after result
      Exit_Code := Exit_Nok;
    end if;

  end loop;

  Sys_Calls.Set_Exit_Code(Exit_Code);

end Status;

