with MY_IO, DIRECTORY, TEXT_HANDLER, ARGUMENT;
procedure T_DIR is
  FILE_NAME : TEXT_HANDLER.TEXT(DIRECTORY.MAX_DIR_NAME_LEN);
  DIR_NAME : TEXT_HANDLER.TEXT(DIRECTORY.MAX_DIR_NAME_LEN);
  KIND : DIRECTORY.FILE_KIND_LIST;
  RIGHTS : NATURAL;
  MAX_LEN : constant := 50;
  PAD : constant STRING (1 .. MAX_LEN) := (others => ' ');

  function GET_NEW_DIR return STRING is
    STR : STRING (1 .. 1024);
    LEN : NATURAL;
  begin
    MY_IO.PUT ("Enter new directory: ");
    MY_IO.GET_LINE (STR, LEN);
    return STR (1 .. LEN);
  end  GET_NEW_DIR;

  use DIRECTORY;
begin

  loop

    MY_IO.PUT_LINE ("PWD ->" & DIRECTORY.GET_CURRENT & "<");

    declare
      DSC : DIRECTORY.DIR_DESC;
    begin
      if ARGUMENT.GET_NBRE_ARG /= 0 then
        ARGUMENT.GET_PARAMETER(DIR_NAME);
      else
        DIRECTORY.GET_CURRENT(DIR_NAME);
      end if;
      DSC := DIRECTORY.OPEN(TEXT_HANDLER.VALUE(DIR_NAME));
      loop
        DIRECTORY.NEXT_ENTRY(DSC, FILE_NAME);
        MY_IO.PUT ("  ---->" & TEXT_HANDLER.VALUE (FILE_NAME) & "< ");
        MY_IO.PUT (PAD(1 .. MAX_LEN - TEXT_HANDLER.LENGTH(FILE_NAME)));
        DIRECTORY.FILE_STAT (
         TEXT_HANDLER.VALUE (DIR_NAME) & '/' &
         TEXT_HANDLER.VALUE (FILE_NAME), KIND, RIGHTS);
        MY_IO.PUT (RIGHTS, BASE => 8, WIDTH => 4);
        MY_IO.PUT_LINE (" " & DIRECTORY.FILE_KIND_LIST'IMAGE(KIND));
        if KIND = DIRECTORY.SYMBOLIC_LINK then
          MY_IO.PUT_LINE ("    ++++>" & DIRECTORY.READ_LINK (
              TEXT_HANDLER.VALUE (DIR_NAME) & '/' &
              TEXT_HANDLER.VALUE (FILE_NAME)) & '<');
        end if;
      end loop;
    exception
      when DIRECTORY.END_ERROR =>
        DIRECTORY.CLOSE (DSC);
    end;
    if ARGUMENT.GET_NBRE_ARG /= 0 then
      return;
    end if;

    MY_IO.NEW_LINE;
    loop
      begin
        DIRECTORY.CHANGE_CURRENT(GET_NEW_DIR);
        exit;
      exception
        when DIRECTORY.NAME_ERROR =>
          MY_IO.PUT_LINE ("-> Not found.");
      end;
    end loop;

  end loop;

end T_DIR;

      
     
      
