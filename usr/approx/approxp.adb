with Text_Io;
with Text_Handler, Argument;
with Menu1;
procedure Approxp is

  procedure Usage is
  begin
    Text_Io.Put_Line ("Usage " & Argument.Get_Program_Name
                               & " [ <file_name> ]");
  end;

begin
  if Argument.Get_Nbre_Arg > 1 then
    Usage;
    return;
  elsif Argument.Get_Nbre_Arg = 1 then
-- Verdix bug:
-- Global variable STR in ARGUMENT body is set when calling
--    GET_PARAMETER, but GET_PARAMETER returns the address
--    (dop vector) of STR that we will pass to MENU1.MAIN_SCREEN
--    instead of a copy. THIS IS THE BUG
-- So, when CON_IO calls ARGUMENT.GET_PROGRAM_NAME, this overwrites
--    this STR variable which changes indirectly the INIT_FILE_NAME
--    of MAIN_SCREEN
-- The workaround is to make a copy of ARGUMENT.GET_PARAMETER
--
-- Original code => bug:
--    MENU1.MAIN_SCREEN (ARGUMENT.GET_PARAMETER);
-- New code:
    declare
      File_Name_Txt : Text_Handler.Text(Argument.Max_Len_Arg);
    begin
      Argument.Get_Parameter(File_Name_Txt);
      Menu1.Main_Screen (Text_Handler.Value(File_Name_Txt));
    end;
-- End
  else
    Menu1.Main_Screen ("");
  end if;
end Approxp;
    
