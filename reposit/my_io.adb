package body MY_IO is

  ERROR_MESSAGE  : constant STRING := "WRONG FORMAT. Try again";
  SEPARE_MESSAGE : constant STRING := " : ";

  procedure PUT_LINE(ITEM  : in BOOLEAN;
                     WIDTH : in TEXT_IO.FIELD := BOOL_IO.DEFAULT_WIDTH;
                     SET   : in TEXT_IO.TYPE_SET := BOOL_IO.DEFAULT_SETTING) is
  begin
    BOOL_IO.PUT(ITEM, WIDTH, SET);
    TEXT_IO.NEW_LINE;
  end PUT_LINE;

  procedure PUT_LINE(ITEM : in CHARACTER) is
  begin
    TEXT_IO.PUT(ITEM);
    TEXT_IO.NEW_LINE;
  end PUT_LINE;

  procedure PUT_LINE(ITEM  : in INTEGER;
                     WIDTH : in TEXT_IO.FIELD := INT_IO.DEFAULT_WIDTH;
                     BASE  : in TEXT_IO.NUMBER_BASE := INT_IO.DEFAULT_BASE) is
  begin
    INT_IO.PUT(ITEM, WIDTH, BASE);
    TEXT_IO.NEW_LINE;
  end PUT_LINE;

  procedure PUT_LINE(ITEM  : in LONG_LONG_INTEGER;
                     WIDTH : in TEXT_IO.FIELD := LONG_IO.DEFAULT_WIDTH;
                     BASE  : in TEXT_IO.NUMBER_BASE := LONG_IO.DEFAULT_BASE) is
  begin
    LONG_IO.PUT(ITEM, WIDTH, BASE);
    TEXT_IO.NEW_LINE;
  end PUT_LINE;

  procedure PUT_LINE(ITEM : in FLOAT;
                     FORE : in TEXT_IO.FIELD := FLO_IO.DEFAULT_FORE;
                     AFT  : in TEXT_IO.FIELD := FLO_IO.DEFAULT_AFT;
                     EXP  : in TEXT_IO.FIELD := DEFAULT_EXP) is
  begin
    FLO_IO.PUT(ITEM, FORE, AFT, EXP);
    TEXT_IO.NEW_LINE;
  end PUT_LINE;


  procedure SAFE_GET(PROMPT : in STRING;
                     ITEM   : out INTEGER) is
    STR                       : STRING(1 .. INTEGER'WIDTH);
    LAST_READ, LAST_CONVERTED : NATURAL;
  begin
    PUT(PROMPT & SEPARE_MESSAGE);
    loop
      begin
        TEXT_IO.GET_LINE(STR, LAST_READ);
        INT_IO.GET(STR(1 .. LAST_READ), ITEM, LAST_CONVERTED);
        if LAST_CONVERTED /= LAST_READ then
          raise TEXT_IO.DATA_ERROR;
        end if;
        return;
      exception
        when others =>
          PUT(ERROR_MESSAGE & SEPARE_MESSAGE);
      end;
    end loop;
  end SAFE_GET;

  procedure SAFE_GET(PROMPT : in STRING;
                     ITEM   : out LONG_LONG_INTEGER) is
    STR                       : STRING(1 .. LONG_LONG_INTEGER'WIDTH);
    LAST_READ, LAST_CONVERTED : NATURAL;
  begin
    PUT(PROMPT & SEPARE_MESSAGE);
    loop
      begin
        TEXT_IO.GET_LINE(STR, LAST_READ);
        LONG_IO.GET(STR(1 .. LAST_READ), ITEM, LAST_CONVERTED);
        if LAST_CONVERTED /= LAST_READ then
          raise TEXT_IO.DATA_ERROR;
        end if;
        return;
      exception
        when others =>
          PUT(ERROR_MESSAGE & SEPARE_MESSAGE);
      end;
    end loop;
  end SAFE_GET;

  procedure SAFE_GET(PROMPT : in STRING;
                     ITEM   : out FLOAT) is
    STR                       : STRING(1 .. 132);
    LAST_READ, LAST_CONVERTED : NATURAL;
    INT_VAL                   : LONG_LONG_INTEGER;
  begin
    PUT(PROMPT & SEPARE_MESSAGE);
    loop
      begin
        begin
          TEXT_IO.GET_LINE(STR, LAST_READ);
          FLO_IO.GET(STR(1 .. LAST_READ), ITEM, LAST_CONVERTED);
          if LAST_CONVERTED /= LAST_READ then
            raise TEXT_IO.DATA_ERROR;
          end if;
          return;
        exception
          when others =>
            LONG_IO.GET(STR(1 .. LAST_READ), INT_VAL, LAST_CONVERTED);
            if LAST_CONVERTED /= LAST_READ then
              raise TEXT_IO.DATA_ERROR;
            end if;
            ITEM := FLOAT(INT_VAL);
            return;
        end;
      exception
        when others =>
          PUT(ERROR_MESSAGE & SEPARE_MESSAGE);
      end;
    end loop;
  end SAFE_GET;

end MY_IO;
