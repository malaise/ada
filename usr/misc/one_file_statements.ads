package ONE_FILE_STATEMENTS is

  -- If FILE_NAME is empty, put total so far and reset it
  procedure PRINT_STATEMENTS_OF_FILE (
             FILE_NAME : STRING;
             PUT_IT : in BOOLEAN := TRUE);

end ONE_FILE_STATEMENTS;
