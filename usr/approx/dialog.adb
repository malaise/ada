with CON_IO, AFPX, NORMAL;
with POINT_STR, RESOL;
package body DIALOG is

  -- If points are not saved, ask for confirmation
  function CONFIRM_LOST return BOOLEAN is
  begin
    if POINTS.P_SAVED then
      return TRUE;
    else
      return SCREEN.CONFIRM(SCREEN.C_DATA_LOST);
    end if;
  end CONFIRM_LOST;
    

  -- Remove trailing spaces. No heading nor intermediate spaces allowed
  procedure PARSE_SPACES (TXT : in out TEXT_HANDLER.TEXT;
                          OK : out BOOLEAN) is
    STR : constant STRING := TEXT_HANDLER.VALUE(TXT);
    L : NATURAL;
  begin
    L := 0;
    for I in reverse STR'RANGE loop
      if STR(I) /= ' ' and then STR(I) /= ASCII.HT then
        -- Significant char
        if L = 0 then
          L := I;
        end if;
      else
        -- space
        if L /= 0 then
          -- Space before significant char
          OK := FALSE;
          return;
        end if;
      end if;
    end loop;
    -- If all spaces, L = 0 => empty
    TEXT_HANDLER.SET (TXT, STR(1 .. L));
    OK := TRUE;
  end PARSE_SPACES;


  function PARSE_LEADING_SPACE (STR : STRING) return STRING is
  begin
    if STR(STR'FIRST) = ' ' then
      return STR(NATURAL'SUCC(STR'FIRST) .. STR'LAST);
    else
      return STR;
    end if;
  end PARSE_LEADING_SPACE;

  -- Get a coordinate
  --  If SET is set IN, then a put_then_get is performed, else a get
  --  Validity is checked and SET is set OUT according to the final result
  -- subtype D_COORDINATE_LIST is SCREEN.S_INFO_LIST range (SCREEN.I_X .. SCREEN.I_YMAX);
  procedure READ_COORDINATE (KIND : in D_COORDINATE_LIST;
           SET : in out BOOLEAN; COORDINATE : in out POINTS.P_T_COORDINATE) is
    CURSOR_FIELD : AFPX.FIELD_RANGE;
    CURSOR_COL : CON_IO.COL_RANGE := 0;
    REDISPLAY : BOOLEAN := FALSE;
    PTG_RESULT : AFPX.RESULT_REC;

    procedure ENCODE is
      COO_STR : POINT_STR.COORDINATE_STRING;
    begin
      COO_STR := POINT_STR.COORDINATE_IMAGE(COORDINATE);
      AFPX.CLEAR_FIELD (SCREEN.GET_FLD);
      AFPX.ENCODE_FIELD (SCREEN.GET_FLD, (0, 0),
                         PARSE_LEADING_SPACE(COO_STR));
    end ENCODE;

    function DECODE return BOOLEAN is
      BUFF : AFPX.STR_TXT;
      OK : BOOLEAN;
    begin
      AFPX.DECODE_FIELD (SCREEN.GET_FLD, 0, BUFF);
      PARSE_SPACES(BUFF, OK);
      if OK then
        begin
          COORDINATE := POINT_STR.COORDINATE_VALUE (TEXT_HANDLER.VALUE(BUFF));
        exception
          when CONSTRAINT_ERROR =>
            OK := FALSE;
        end;
      end if;
      if OK then
        return TRUE;
      else
        SCREEN.ERROR (SCREEN.E_WRONG_COORDINATE);
        SCREEN.INIT_FOR_GET (CURSOR_FIELD);
        return FALSE;
      end if;
    end DECODE;

  begin
    SCREEN.INIT_FOR_GET (CURSOR_FIELD);
    if SET then
      ENCODE;
    else
      AFPX.CLEAR_FIELD(SCREEN.GET_FLD);
    end if;

    loop
      SCREEN.INFORM(KIND);
      AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
      REDISPLAY := FALSE;
      case PTG_RESULT.EVENT is
        when AFPX.KEYBOARD =>
          case PTG_RESULT.KEYBOARD_KEY is
            when AFPX.RETURN_KEY =>
              if DECODE then
                SET := TRUE;
                return;
              end if;
            when AFPX.ESCAPE_KEY =>
              SET := FALSE;
              return;
            when AFPX.BREAK_KEY =>
              null;
          end case;
        when AFPX.MOUSE_BUTTON =>
          case PTG_RESULT.FIELD_NO is
            when SCREEN.LIST_SCROLL_FLD_RANGE'FIRST .. SCREEN.LIST_SCROLL_FLD_RANGE'LAST =>
              SCREEN.SCROLL(PTG_RESULT.FIELD_NO);
            when SCREEN.OK_BUTTON_FLD =>
              if DECODE then
                SET := TRUE;
                return;
              end if;
            when SCREEN.CANCEL_BUTTON_FLD =>
              SET := FALSE;
              return;
            when others =>
              null;
          end case;
        when AFPX.REFRESH =>
          REDISPLAY := TRUE;
      end case;
    end loop;
  end READ_COORDINATE;


  -- Get a new degree
  procedure READ_DEGREE is

    CURSOR_FIELD : AFPX.FIELD_RANGE;
    CURSOR_COL : CON_IO.COL_RANGE := 0;
    REDISPLAY : BOOLEAN := FALSE;
    PTG_RESULT : AFPX.RESULT_REC;
    DEGREE : NATURAL;

    procedure ENCODE is
    begin
      AFPX.ENCODE_FIELD (SCREEN.GET_FLD, (0, 0), NORMAL (DEGREE, SCREEN.GET_GET_WIDTH, FALSE));
    end ENCODE;

    function DECODE return BOOLEAN is
      BUFF : AFPX.STR_TXT;
      OK : BOOLEAN;
    begin
      AFPX.DECODE_FIELD (SCREEN.GET_FLD, 0, BUFF);
      PARSE_SPACES(BUFF, OK);
      if OK then
        begin
          DEGREE := NATURAL'VALUE(TEXT_HANDLER.VALUE(BUFF));
          RESOL.R_SET_DEGREE(DEGREE);
        exception
          when CONSTRAINT_ERROR | RESOL.R_DEGREE_OUT =>
            OK := FALSE;
        end;
      end if;
      if OK then
        return TRUE;
      else
        SCREEN.ERROR (SCREEN.E_WRONG_DEGREE);
        SCREEN.INIT_FOR_GET (CURSOR_FIELD);
        return FALSE;
      end if;
    end DECODE;

  begin
    SCREEN.INIT_FOR_GET (CURSOR_FIELD);
    SCREEN.PUT_TITLE (SCREEN.GET_DEGREE);
    DEGREE := RESOL.R_DEGREE;
    ENCODE;

    loop
      SCREEN.INFORM(SCREEN.I_DEGREE);
      AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
      REDISPLAY := FALSE;
      case PTG_RESULT.EVENT is
        when AFPX.KEYBOARD =>
          case PTG_RESULT.KEYBOARD_KEY is
            when AFPX.RETURN_KEY =>
              if DECODE then
                return;
              end if;
            when AFPX.ESCAPE_KEY =>
              return;
            when AFPX.BREAK_KEY =>
              null;
          end case;
        when AFPX.MOUSE_BUTTON =>
          case PTG_RESULT.FIELD_NO is
            when SCREEN.LIST_SCROLL_FLD_RANGE'FIRST .. SCREEN.LIST_SCROLL_FLD_RANGE'LAST =>
              SCREEN.SCROLL(PTG_RESULT.FIELD_NO);
            when SCREEN.OK_BUTTON_FLD =>
              if DECODE then
                return;
              end if;
            when SCREEN.CANCEL_BUTTON_FLD =>
              return;
            when others =>
              null;
          end case;
        when AFPX.REFRESH =>
          REDISPLAY := TRUE;
      end case;
    end loop;
  end READ_DEGREE;

  -- Display polynom
  procedure PUT_POLYNOM (POLYNOM : RESOL.VECTOR) is
    procedure INSERT(STR : in STRING) is
      REC : AFPX.LINE_REC;
    begin
      REC.LEN := STR'LENGTH;
      REC.STR (1 .. REC.LEN) := STR;
      AFPX.LINE_LIST_MNG.INSERT (AFPX.LINE_LIST, REC);
    end INSERT;
  begin
    SCREEN.PUT_TITLE(SCREEN.POLYNOM);
    SCREEN.INFORM(SCREEN.I_CLEAR);
    -- Encode in list
    AFPX.LINE_LIST_MNG.DELETE_LIST(AFPX.LINE_LIST);
    for I in POLYNOM'RANGE loop
      -- factor * X^ijkl
      INSERT (POINT_STR.COORDINATE_IMAGE(POLYNOM(I))
        & " * X^" & NORMAL (I-1, SCREEN.MAX_DEGREE_WIDTH, GAP => '0'));
    end loop;
    -- Rewind
    AFPX.LINE_LIST_MNG.MOVE_TO (AFPX.LINE_LIST, AFPX.LINE_LIST_MNG.NEXT,
                                NUMBER => 0, FROM_CURRENT => FALSE);
    -- Go to top
    AFPX.UPDATE_LIST (AFPX.TOP);
    -- Let screen/afpx do the job
    SCREEN.ERROR (SCREEN.E_DONE);
  end PUT_POLYNOM;

  -- Display y=f(x)
  procedure PUT_YFX (POINT : in POINTS.P_T_ONE_POINT) is
    MY_FLD : constant AFPX.FIELD_RANGE := 32;
  begin
    -- Enable FX, enable and protect y (get field)
    AFPX.SET_FIELD_COLORS (MY_FLD, FOREGROUND => CON_IO.CYAN);
    AFPX.SET_FIELD_ACTIVATION(SCREEN.GET_FLD, TRUE);
    AFPX.SET_FIELD_PROTECTION(SCREEN.GET_FLD, TRUE);
    AFPX.CLEAR_FIELD(SCREEN.GET_FLD);

    -- Encode data
    AFPX.ENCODE_FIELD (MY_FLD, (0,0),
      " F(" & 
      PARSE_LEADING_SPACE (POINT_STR.COORDINATE_IMAGE(POINT.X))
      & ") =");
    AFPX.ENCODE_FIELD (SCREEN.GET_FLD, (0,0),
      POINT_STR.COORDINATE_IMAGE(POINT.Y));
    
    -- Let screen/afpx do the job
    SCREEN.ERROR (SCREEN.E_DONE);
    -- Clean up
    AFPX.SET_FIELD_COLORS (MY_FLD, FOREGROUND => CON_IO.BLACK);
    AFPX.CLEAR_FIELD(MY_FLD);
  end PUT_YFX;
end DIALOG;

