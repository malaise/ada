with MY_IO, ARGUMENT, AFPX, CON_IO, DYNAMIC_LIST, DIR_MNG;
procedure T_AFPX is

  procedure DIR_SORT is new DIR_MNG.FILE_LIST_MNG.SORT (DIR_MNG.LESS_THAN);
  DIR_LIST : DIR_MNG.FILE_LIST_MNG.LIST_TYPE;
  DIR_ITEM : DIR_MNG.FILE_ENTRY_REC;
  AFPX_ITEM : AFPX.LINE_REC;

  CURSOR_FIELD : AFPX.FIELD_RANGE;
  CURSOR_COL   : CON_IO.COL_RANGE;
  PTG_RESULT   : AFPX.RESULT_REC;
  REDISPLAY    : BOOLEAN;
  FLIP_FLOP : BOOLEAN;

  use AFPX;

  procedure NEXT_FIELD (CURSOR_FIELD : in out AFPX.FIELD_RANGE) is
    LOC : AFPX.ABSOLUTE_FIELD_RANGE;
  begin
    LOC := AFPX.NEXT_CURSOR_FIELD (CURSOR_FIELD);
    if LOC = 0 then
      CURSOR_FIELD := 1;
    else
      CURSOR_FIELD := LOC;
    end if;
  end NEXT_FIELD;

begin
  AFPX.USE_DESCRIPTOR(1);

  -- List directory and store it in AFPX list
  if ARGUMENT.GET_NBRE_ARG = 0 then
    DIR_MNG.LIST_DIR (DIR_LIST, "");
  else
    DIR_MNG.LIST_DIR (DIR_LIST,
                      ARGUMENT.GET_PARAMETER (OCCURENCE => 1));
  end if;

  DIR_SORT (DIR_LIST);
  DIR_MNG.FILE_LIST_MNG.MOVE_TO (DIR_LIST, DIR_MNG.FILE_LIST_MNG.NEXT,
                                 0 , FALSE);
  loop
    DIR_MNG.FILE_LIST_MNG.READ (DIR_LIST, DIR_ITEM,
                                DIR_MNG.FILE_LIST_MNG.CURRENT);
    AFPX_ITEM.LEN := DIR_ITEM.LEN;
    AFPX_ITEM.STR := (others => ' ');
    AFPX_ITEM.STR(1 .. AFPX_ITEM.LEN) := DIR_ITEM.NAME (1 .. DIR_ITEM.LEN);
    AFPX_ITEM.STR(AFPX_ITEM.LEN+1) := '>';
    AFPX_ITEM.STR(AFPX_ITEM.LEN+2) :=
       DIR_MNG.FILE_KIND_LIST'IMAGE(DIR_ITEM.KIND)(1);
    AFPX_ITEM.LEN := AFPX_ITEM.LEN + 2;
    AFPX.LINE_LIST_MNG.INSERT (AFPX.LINE_LIST, AFPX_ITEM);
    exit when DIR_MNG.FILE_LIST_MNG.GET_POSITION (DIR_LIST)
    = DIR_MNG.FILE_LIST_MNG.LIST_LENGTH (DIR_LIST);
    DIR_MNG.FILE_LIST_MNG.MOVE_TO (DIR_LIST);
  end loop;
  AFPX.LINE_LIST_MNG.MOVE_TO (AFPX.LINE_LIST, AFPX.LINE_LIST_MNG.NEXT,
     0, FALSE);

  AFPX.LINE_LIST_MNG.READ (AFPX.LINE_LIST, AFPX_ITEM,
                           AFPX.LINE_LIST_MNG.CURRENT);

  AFPX.ENCODE_FIELD (2, (1, 0), ">" &
                                AFPX_ITEM.STR (1 .. AFPX_ITEM.LEN) & "<");



  CURSOR_FIELD := 1;
  CURSOR_COL := 0;
  FLIP_FLOP := TRUE;
  REDISPLAY := FALSE;

  loop
    AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
    REDISPLAY := FALSE;

    case PTG_RESULT.EVENT is
      when AFPX.KEYBOARD =>
        case PTG_RESULT.KEYBOARD_KEY is
          when AFPX.RETURN_KEY =>
            if AFPX.DECODE_FIELD(CURSOR_FIELD, 0)(1 .. 4) = "exit" then
              AFPX.SET_FIELD_ACTIVATION (CURSOR_FIELD, FALSE);
              AFPX.CLEAR_FIELD (2);
              AFPX.ENCODE_FIELD (2, (1, 0), ">" &
                                 AFPX.DECODE_FIELD(CURSOR_FIELD, 0) & "<");
              NEXT_FIELD (CURSOR_FIELD);
              CURSOR_COL := 0;
            else
              AFPX.CLEAR_FIELD (2);
              AFPX.ENCODE_FIELD (2, (1, 0), ">" &
                                 AFPX.DECODE_FIELD(CURSOR_FIELD, 0) & "<");
            end if;
          when AFPX.ESCAPE_KEY =>
            AFPX.CLEAR_FIELD (CURSOR_FIELD);
            AFPX.CLEAR_FIELD (2);
            AFPX.ENCODE_FIELD (2, (1, 0), ">" &
                               AFPX.DECODE_FIELD(CURSOR_FIELD, 0) & "<");
            CURSOR_COL := 0;
          when AFPX.BREAK_KEY =>
            exit;
        end case;
        FLIP_FLOP := not FLIP_FLOP;
        AFPX.SET_FIELD_ACTIVATION (5, FLIP_FLOP);
        AFPX.SET_FIELD_PROTECTION (0, not FLIP_FLOP);
      when AFPX.MOUSE_BUTTON =>
        case PTG_RESULT.FIELD_NO is
          when 4 =>
            exit;
          when 5 | AFPX.LIST_FIELD_NO =>
            AFPX.LINE_LIST_MNG.READ (AFPX.LINE_LIST, AFPX_ITEM,
                                     AFPX.LINE_LIST_MNG.CURRENT);
            AFPX.CLEAR_FIELD (2);
            AFPX.ENCODE_FIELD (2, (1, 0),
                               ">" & AFPX_ITEM.STR (1 .. AFPX_ITEM.LEN) & "<");
          when 8 =>
            AFPX.UPDATE_LIST(AFPX.UP);
          when 9 =>
            AFPX.UPDATE_LIST(AFPX.DOWN);
          when 10 =>
            AFPX.UPDATE_LIST(AFPX.PAGE_UP);
          when 11 =>
            AFPX.UPDATE_LIST(AFPX.PAGE_DOWN);
          when 12 =>
            AFPX.UPDATE_LIST(AFPX.TOP);
          when 13 =>
            AFPX.UPDATE_LIST(AFPX.BOTTOM);
          when 14 =>
            AFPX.UPDATE_LIST(AFPX.CENTER);
          when others =>
            null;
        end case;
      when AFPX.FD_EVENT =>
        AFPX.CLEAR_FIELD (2);
         AFPX.ENCODE_FIELD (2, (1, 0), ">> Fd Event <<");
      when AFPX.REFRESH =>
        REDISPLAY := TRUE;
    end case;

  end loop;

  CON_IO.RESET_TERM;

end T_AFPX;
