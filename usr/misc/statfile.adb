with TEXT_IO;
with ARGUMENT;
with ONE_FILE_STATEMENTS;

procedure STATFILE is

  procedure STAT_ONE_FILE (LIST_FILE_NAME : in STRING) is
    LIST_FILE : TEXT_IO.FILE_TYPE;
    FILE_NAME : STRING (1 .. 5000);
    FILE_NAME_LEN : NATURAL;
  begin
    begin
      TEXT_IO.OPEN (LIST_FILE, TEXT_IO.IN_FILE, LIST_FILE_NAME);
    exception
      when others => 
        TEXT_IO.PUT_LINE ("Exception raised when opening list file "
                        & LIST_FILE_NAME & " SKIPPING");
        return;
    end;

    while not TEXT_IO.END_OF_FILE (LIST_FILE) loop
      begin
        TEXT_IO.GET_LINE (LIST_FILE, FILE_NAME, FILE_NAME_LEN);
      exception
        when others => 
          TEXT_IO.PUT_LINE ("Exception raised when reading line "
                          & TEXT_IO.POSITIVE_COUNT'IMAGE(TEXT_IO.LINE(LIST_FILE))
                          & " of list file " & LIST_FILE_NAME & " SKIPPING");
          TEXT_IO.CLOSE (LIST_FILE);
          return;
      end;
      
      if FILE_NAME_LEN /= 0 then
        ONE_FILE_STATEMENTS.PRINT_STATEMENTS_OF_FILE(FILE_NAME(1 .. FILE_NAME_LEN));
      end if;
    end loop;
    TEXT_IO.CLOSE (LIST_FILE);
  end STAT_ONE_FILE; 

begin

  for ARG in 1 .. ARGUMENT.GET_NBRE_ARG loop
    TEXT_IO.PUT_LINE ("Processing list file " & STRING'(ARGUMENT.GET_PARAMETER(ARG)));
    STAT_ONE_FILE(ARGUMENT.GET_PARAMETER(ARG));
  end loop;

  ONE_FILE_STATEMENTS.PRINT_STATEMENTS_OF_FILE("");

end STATFILE;

