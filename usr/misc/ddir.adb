with MY_IO, ARGUMENT, TEXT_HANDLER, DIRECTORY;
procedure DDIR is

  procedure USAGE is
  begin
    MY_IO.PUT_LINE ("Usage: " & ARGUMENT.GET_PROGRAM_NAME & " [ { <directory> } ]");
  end USAGE;

  procedure DDIR_ONE (DIR_NAME : in STRING) is
    DIR_DSC : DIRECTORY.DIR_DESC;
    ENTRY_NAME, FULL_DIR_NAME : TEXT_HANDLER.TEXT (DIRECTORY.MAX_DIR_NAME_LEN);
    KIND : DIRECTORY.FILE_KIND_LIST;
    RIGHTS : NATURAL;
    use DIRECTORY;
  begin
    begin
      DIR_DSC := DIRECTORY.OPEN(DIR_NAME);
    exception
      when DIRECTORY.NAME_ERROR =>
        MY_IO.PUT_LINE ("ERROR reading directory " & DIR_NAME);
        return;
    end;

    loop
      begin
        DIRECTORY.NEXT_ENTRY (DIR_DSC, ENTRY_NAME);
      exception
        when DIRECTORY.END_ERROR =>
          exit;
      end;
      begin
        DIRECTORY.FILE_STAT (DIR_NAME & "/" & TEXT_HANDLER.VALUE(ENTRY_NAME),
                             KIND, RIGHTS);
      exception
        when DIRECTORY.NAME_ERROR =>
          -- A link to nowhere?
          KIND := DIRECTORY.UNKNOWN;
      end;
      if KIND = DIRECTORY.DIR
      and then TEXT_HANDLER.VALUE(ENTRY_NAME) /= "."
      and then TEXT_HANDLER.VALUE(ENTRY_NAME) /= ".." then
        MY_IO.PUT_LINE (TEXT_HANDLER.VALUE(ENTRY_NAME));
      end if;
    end loop;
    DIRECTORY.CLOSE(DIR_DSC);
  end DDIR_ONE;

begin
  if ARGUMENT.GET_NBRE_ARG = 0 then
    DDIR_ONE (".");
  else
    for I in 1 .. ARGUMENT.GET_NBRE_ARG loop
      DDIR_ONE (ARGUMENT.GET_PARAMETER(I));
    end loop;
  end if;
end DDIR;
