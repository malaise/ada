with MY_IO, DIRECTORY, TEXT_HANDLER;
procedure T_DIR is
  FILE_NAME : TEXT_HANDLER.TEXT(1024);
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

begin

  loop

    MY_IO.PUT_LINE ("PWD ->" & DIRECTORY.GET_CURRENT & "<");

    declare
      DSC : DIRECTORY.DIR_DESC;
    begin
      DSC := DIRECTORY.OPEN(DIRECTORY.GET_CURRENT);
      loop
        DIRECTORY.NEXT_ENTRY(DSC, FILE_NAME);
        MY_IO.PUT ("  ---->" & TEXT_HANDLER.VALUE (FILE_NAME) & "< ");
        MY_IO.PUT (PAD(1 .. MAX_LEN - TEXT_HANDLER.LENGTH(FILE_NAME)));
        DIRECTORY.FILE_STAT (TEXT_HANDLER.VALUE (FILE_NAME), KIND, RIGHTS);
        MY_IO.PUT (RIGHTS, BASE => 8, WIDTH => 4);
        MY_IO.PUT_LINE (" " & DIRECTORY.FILE_KIND_LIST'IMAGE(KIND));
      end loop;
    exception
      when DIRECTORY.END_ERROR =>
        DIRECTORY.CLOSE (DSC);
    end;

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

      
     
      
