with ARGUMENT;
with ONE_FILE_STATEMENTS;

procedure STAT is
  FIRST : POSITIVE := 1;
  PUT_IT : BOOLEAN := TRUE;
begin

  if ARGUMENT.GET_NBRE_ARG >= 1
  and then ARGUMENT.GET_PARAMETER(1) = "-s" then
    FIRST := 2;
    PUT_IT := FALSE;
  end if;

  -- Store all files
  for ARG in FIRST .. ARGUMENT.GET_NBRE_ARG loop
    -- One stat on each file
    ONE_FILE_STATEMENTS.PRINT_STATEMENTS_OF_FILE(ARGUMENT.GET_PARAMETER(ARG), PUT_IT);
  end loop;

  ONE_FILE_STATEMENTS.PRINT_STATEMENTS_OF_FILE("");

end STAT;

