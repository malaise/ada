-- Check if target file: $n is up to date comparing to source files:
--  $1, $2 .. $n-1. This is: target file exists and is newer that all sources.
-- exit 0  -->  $n file is ok (newer than all sources)
-- exit 1  -->  $n file does not exist or is older than some sources
-- exit 2  -->  Error: cannot read some source file(s)
-- exit 3  -->  Argument or internal error
with Ada.Calendar;
use Ada.Calendar;
with Argument, Sys_Calls;
procedure Status is
  -- The exit values
  Exit_Ok             : constant := 0;
  Exit_Nok            : constant := 1;
  Exit_Src_Not_Found  : constant := 2;
  Exit_Internal_Error : constant := 3;

  -- The final exit code
  Exit_Code : Integer;

  -- Info got with File_Stat
  Fstat : Sys_Calls.File_Stat_Rec;

  -- Modif time of target, current source
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
    Fstat := Sys_Calls.File_Stat(Argument.Get_Parameter(Argument.Get_Nbre_Arg));
  exception
    when Sys_Calls.Name_Error =>
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
  Target_Time := Sys_Calls.Time_Of(Fstat.Modif_Time);

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
      Fstat := Sys_Calls.File_Stat(Argument.Get_Parameter(Arg_No));
    exception
      when Sys_Calls.Name_Error =>
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
    Source_Time := Sys_Calls.Time_Of(Fstat.Modif_Time);

    if Exit_Code = Exit_Ok and then Target_Time <= Source_Time then
      -- Source files exist so far and this one is after result
      Exit_Code := Exit_Nok;
    end if;

  end loop;

  Sys_Calls.Set_Exit_Code(Exit_Code);

end Status;

