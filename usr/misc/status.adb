-- Check if target file: $n is up to date comparing to source files:
--  $1, $2 .. $n-1. This is: target file exists and is newer that sources.
-- exit 0  -->  $n file is ok (newer than all sources)
-- exit 1  -->  $n file is older than some sources or does not exist
-- exit 2  -->  Error: cannot read some source files
-- exit 3  -->  Argument or internal error
with CALENDAR;
use CALENDAR;
with ARGUMENT, SYS_CALLS, DIRECTORY;
procedure STATUS is
  -- The exit values
  EXIT_OK             : constant := 0;
  EXIT_NOK            : constant := 1;
  EXIT_SRC_NOT_FOUND  : constant := 2;
  EXIT_INTERNAL_ERROR : constant := 3;

  -- The final exit code
  EXIT_CODE : INTEGER;

  -- Info unused but got with FILE_STAT
  KIND : DIRECTORY.FILE_KIND_LIST;
  RIGHTS : NATURAL;

  -- Modif time of target, current source
  TARGET_MTIME, SOURCE_MTIME : DIRECTORY.TIME_T;
  TARGET_TIME, SOURCE_TIME : CALENDAR.TIME;

begin

  -- Check arguments, at least 2
  if ARGUMENT.GET_NBRE_ARG < 2 then
    SYS_CALLS.PUT_LINE_ERROR(
        "SYNTAX ERROR. Usage: "
      & ARGUMENT.GET_PROGRAM_NAME & " { <source_file> } <target_file>");
    SYS_CALLS.SET_EXIT_CODE (EXIT_INTERNAL_ERROR);
    return;
  end if;

  -- Initialize final result
  EXIT_CODE := EXIT_OK;

  -- Check that target file exists and get its modif date
  begin
    DIRECTORY.FILE_STAT(ARGUMENT.GET_PARAMETER(1),
                        KIND, RIGHTS, TARGET_MTIME);
  exception
    when DIRECTORY.NAME_ERROR =>
      -- Not found
      EXIT_CODE := EXIT_NOK;
    when others =>
      -- Other error
      SYS_CALLS.PUT_LINE_ERROR(
          "ACCESS ERROR: Cannot read status of target file "
        & STRING'(ARGUMENT.GET_PARAMETER(1)));
      SYS_CALLS.SET_EXIT_CODE (EXIT_INTERNAL_ERROR);
      return;
  end;
  TARGET_TIME := DIRECTORY.TIME_OF(TARGET_MTIME);

  -- Check that each source exists and is before result
  for ARG_NO in 2 .. ARGUMENT.GET_NBRE_ARG loop

    if STRING'(ARGUMENT.GET_PARAMETER(ARG_NO)) = ARGUMENT.GET_PARAMETER(1) then
      SYS_CALLS.PUT_LINE_ERROR(
         "SEMANTIC ERROR: Source file "
       & STRING'(ARGUMENT.GET_PARAMETER(ARG_NO))
       & " is also the result file.");
      SYS_CALLS.PUT_LINE_ERROR("Usage: "
       & ARGUMENT.GET_PROGRAM_NAME & " { <source_file> } <target_file>");
      SYS_CALLS.SET_EXIT_CODE (EXIT_INTERNAL_ERROR);
      return;
    end if;

    begin
      DIRECTORY.FILE_STAT(ARGUMENT.GET_PARAMETER(ARG_NO),
                        KIND, RIGHTS, SOURCE_MTIME);
    exception
      when DIRECTORY.NAME_ERROR =>
        -- Not found
        SYS_CALLS.PUT_LINE_ERROR(
            "NAME ERROR: Source file "
          & STRING'(ARGUMENT.GET_PARAMETER(ARG_NO))
          & " not found.");
        SYS_CALLS.SET_EXIT_CODE (EXIT_SRC_NOT_FOUND);
        return;
      when others =>
        -- Other error
        SYS_CALLS.PUT_LINE_ERROR(
            "ACCESS ERROR: Cannot read status of source file "
          & STRING'(ARGUMENT.GET_PARAMETER(ARG_NO)));
        SYS_CALLS.SET_EXIT_CODE (EXIT_INTERNAL_ERROR);
        return;
    end;
    SOURCE_TIME := DIRECTORY.TIME_OF(SOURCE_MTIME);

    if EXIT_CODE = EXIT_OK and then TARGET_TIME <= SOURCE_TIME then
      -- Source files exist so far and this one is after result
      EXIT_CODE := EXIT_NOK;
    end if;

  end loop;

  SYS_CALLS.SET_EXIT_CODE(EXIT_CODE);

end STATUS;

