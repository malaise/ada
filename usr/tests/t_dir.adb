with CALENDAR;
with MY_IO, INT_IO, DIRECTORY, TEXT_HANDLER, ARGUMENT, DAY_MNG, NORMAL;
procedure T_DIR is
  FILE_NAME : TEXT_HANDLER.TEXT(DIRECTORY.MAX_DIR_NAME_LEN);
  DIR_NAME : TEXT_HANDLER.TEXT(DIRECTORY.MAX_DIR_NAME_LEN);
  KIND : DIRECTORY.FILE_KIND_LIST;
  RIGHTS : NATURAL;
  MTIME  : DIRECTORY.TIME_T;
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

  procedure PUT_RIGHTS (RIGHTS : in NATURAL) is
    STR : STRING(1 .. 7) := (others => ' ');
    ZSTR : STRING(1 .. 4) := (others => '0');
    F,L : NATURAL;
  begin
    INT_IO.PUT (STR, RIGHTS, BASE => 8);
    for I in STR'RANGE loop
      if STR(I) = '#' then
        F := I+1;
        exit;
      end if;
    end loop;
    for I in reverse STR'RANGE loop
      if STR(I) = '#' then
        L := I - 1;
        exit;
      end if;
    end loop;

    ZSTR(4-L+F .. 4) := STR(F .. L);
    MY_IO.PUT(ZSTR);
  end PUT_RIGHTS;

  procedure PUT_DATE (MTIME : in DIRECTORY.TIME_T) is
    T : CALENDAR.TIME;
    YEAR   : CALENDAR.YEAR_NUMBER;
    MONTH  : CALENDAR.MONTH_NUMBER;
    DAY    : CALENDAR.DAY_NUMBER;
    DUR    : CALENDAR.DAY_DURATION;
    HOURS    : DAY_MNG.T_HOURS;
    MINUTES  : DAY_MNG.T_MINUTES;
    SECONDS  : DAY_MNG.T_SECONDS;
    MILLISEC : DAY_MNG.T_MILLISEC;
  begin
    T := DIRECTORY.TIME_OF(MTIME);
    CALENDAR.SPLIT (T, YEAR, MONTH, DAY, DUR);
    DAY_MNG.SPLIT (DUR, HOURS, MINUTES, SECONDS, MILLISEC);
    MY_IO.PUT(" " &
              NORMAL(YEAR, 4, GAP =>'0') & '/' &
              NORMAL(MONTH, 2, GAP =>'0') & '/' &
              NORMAL(DAY, 2, GAP =>'0') & ' '  &
              NORMAL(HOURS, 2, GAP =>'0') & ':'  &
              NORMAL(MINUTES, 2, GAP =>'0') & ':'  &
              NORMAL(SECONDS, 2, GAP =>'0') );
  end PUT_DATE;

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
      if TEXT_HANDLER.VALUE(DIR_NAME) = "/" then
        TEXT_HANDLER.EMPTY(DIR_NAME);
      end if;
      loop
        DIRECTORY.NEXT_ENTRY(DSC, FILE_NAME);
        MY_IO.PUT ("  ---->" & TEXT_HANDLER.VALUE (FILE_NAME) & "< ");
        MY_IO.PUT (PAD(1 .. MAX_LEN - TEXT_HANDLER.LENGTH(FILE_NAME)));
        begin
          DIRECTORY.FILE_STAT (
             TEXT_HANDLER.VALUE (DIR_NAME) & '/' &
             TEXT_HANDLER.VALUE (FILE_NAME), KIND, RIGHTS, MTIME);
          PUT_RIGHTS (RIGHTS);
          PUT_DATE (MTIME);
          MY_IO.PUT_LINE (" " & DIRECTORY.FILE_KIND_LIST'IMAGE(KIND));
          if KIND = DIRECTORY.SYMBOLIC_LINK then
            MY_IO.PUT_LINE ("    ++++>" & DIRECTORY.READ_LINK (
                TEXT_HANDLER.VALUE (DIR_NAME) & '/' &
                TEXT_HANDLER.VALUE (FILE_NAME)) & '<');
          end if;
        exception
          when DIRECTORY.NAME_ERROR =>
            MY_IO.PUT_LINE ("???? ???");
          when DIRECTORY.ACCESS_ERROR =>
            MY_IO.PUT_LINE ("!!!! !!!");
        end;
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
        when DIRECTORY.ACCESS_ERROR =>
          MY_IO.PUT_LINE ("-> Permission.");
      end;
    end loop;

  end loop;

end T_DIR;

