-- executes some commands in all the sub-dir in the tree of sub-dir
--  under the current
-- Syntax: pexec command [ { ;command } ]
--  each command can contain spaces
with DIRECTORY;
with MY_IO, TEXT_HANDLER, RECURS, SYS_CALLS;
with COMMAND;
procedure PEXEC is

  NO_ACTION : BOOLEAN;
  NO_NAME_OF_DIR : BOOLEAN;
  NOT_IN_CURRENT : BOOLEAN;
  FIRST_LEVEL_ONLY : BOOLEAN;
  LEAVES_ONLY : BOOLEAN;
  NO_STOP_ON_ERROR : BOOLEAN;

  INITIAL_DIR : TEXT_HANDLER.TEXT(DIRECTORY.MAX_DIR_NAME_LEN);

  procedure RESTORE is
  begin
    DIRECTORY.CHANGE_CURRENT (TEXT_HANDLER.VALUE(INITIAL_DIR));
  exception
    when DIRECTORY.NAME_ERROR =>
      MY_IO.PUT_LINE ("Error going back to original directory.");
  end RESTORE;


  function EXECUTE return BOOLEAN is
    EXEC_RETURN : INTEGER;
    TO_CALL : TEXT_HANDLER.TEXT (1024);
  begin
    for I in 1 .. COMMAND.NBRE_COMMANDS loop
      TEXT_HANDLER.SET (TO_CALL, COMMAND.NTH_COMMAND(I));
      if not NO_ACTION then
        MY_IO.PUT_LINE("--> " & TEXT_HANDLER.VALUE (TO_CALL));
      end if;

      EXEC_RETURN := SYS_CALLS.CALL_SYSTEM (TEXT_HANDLER.VALUE(TO_CALL));

      if EXEC_RETURN /= 0 then
        MY_IO.PUT_LINE (
         "Error executing command " & TEXT_HANDLER.VALUE (TO_CALL) );
        return FALSE;
      end if;

    end loop;
    return TRUE;
  end EXECUTE;

  procedure MY_RECURS is new RECURS (DO_IN_DIR => EXECUTE);

begin
  DIRECTORY.GET_CURRENT (INITIAL_DIR);

  COMMAND.PARSE (NO_ACTION, NO_NAME_OF_DIR, NOT_IN_CURRENT, FIRST_LEVEL_ONLY,
                 LEAVES_ONLY, NO_STOP_ON_ERROR);

  MY_RECURS (NAME_OF_DIR      => not NO_NAME_OF_DIR,
             IN_CURRENT       => not NOT_IN_CURRENT,
             FIRST_LEVEL_ONLY => FIRST_LEVEL_ONLY,
             LEAVES_ONLY      => LEAVES_ONLY,
             STOP_ON_ERROR    => not NO_STOP_ON_ERROR);

  RESTORE;
exception
  when COMMAND.NO_COMMAND =>
    MY_IO.NEW_LINE;
    MY_IO.PUT_LINE ("No command line to pexec.");
    COMMAND.PRINT_USAGE;
    RESTORE;
  when others =>
    MY_IO.NEW_LINE;
    MY_IO.PUT_LINE ("Unexpected error.");
    COMMAND.PRINT_USAGE;
    RESTORE;
    raise;
end PEXEC;
