with ARGUMENT;
with ONE_FILE_STATEMENTS;

procedure STAT is
begin

  -- Store all files
  for ARG in 1 .. ARGUMENT.GET_NBRE_ARG loop
    -- One stat on each file
    ONE_FILE_STATEMENTS.PRINT_STATEMENTS_OF_FILE(ARGUMENT.GET_PARAMETER(ARG));
  end loop;

  ONE_FILE_STATEMENTS.PRINT_STATEMENTS_OF_FILE("");

end STAT;

