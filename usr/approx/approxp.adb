with TEXT_IO;
with TEXT_HANDLER, ARGUMENT;
with MENU1;
procedure APPROXP is

  procedure USAGE is
  begin
    TEXT_IO.PUT_LINE ("Usage " & ARGUMENT.GET_PROGRAM_NAME
                               & " [ <file_name> ]");
  end;

begin
  if ARGUMENT.GET_NBRE_ARG > 1 then
    USAGE;
    return;
  elsif ARGUMENT.GET_NBRE_ARG = 1 then
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
      FILE_NAME_TXT : TEXT_HANDLER.TEXT(ARGUMENT.MAX_LEN_ARG);
    begin
      ARGUMENT.GET_PARAMETER(FILE_NAME_TXT);
      MENU1.MAIN_SCREEN (TEXT_HANDLER.VALUE(FILE_NAME_TXT));
    end;
-- End
  else
    MENU1.MAIN_SCREEN ("");
  end if;
end APPROXP;
    
