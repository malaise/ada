with TEXT_IO;
with ARGUMENT, CON_IO, AFPX, NORMAL;
procedure T_DSCR is
  DSCR_NO : AFPX.DESCRIPTOR_RANGE;
  CURSOR_FIELD : AFPX.ABSOLUTE_FIELD_RANGE;
  CURSOR_COL : CON_IO.COL_RANGE;
  REDISPLAY : BOOLEAN;
  PTG_RESULT : AFPX.RESULT_REC;
  LINE : AFPX.LINE_REC;

  procedure USAGE is
  begin
    TEXT_IO.PUT_LINE("ERROR. Usage " & ARGUMENT.GET_PROGRAM_NAME
                                     & " [ <dscr_no> ]");
  end USAGE;

  procedure SET_DSCR(NO : in AFPX.DESCRIPTOR_RANGE) is
  begin
    AFPX.USE_DESCRIPTOR(NO);

    CURSOR_FIELD := AFPX.NEXT_CURSOR_FIELD(0);
    if CURSOR_FIELD not in AFPX.FIELD_RANGE then
      -- No get field
      CURSOR_FIELD := AFPX.FIELD_RANGE'FIRST;
    end if;
    CURSOR_COL := 0;
    REDISPLAY := FALSE;
  end SET_DSCR;


  use AFPX;
begin

  if ARGUMENT.GET_NBRE_ARG > 1 then
    USAGE;
    return;
  end if;

  if ARGUMENT.GET_NBRE_ARG = 1 then
    begin
      DSCR_NO := AFPX.DESCRIPTOR_RANGE'VALUE(ARGUMENT.GET_PARAMETER);
    exception
      when others =>
        USAGE;
        return;
    end;
  else
    DSCR_NO := 1;
  end if;
    
  for I in 1 .. 999 loop
    LINE.STR(1 .. 3) := NORMAL(I, 3, GAP => '0');
    LINE.LEN := 3;
    AFPX.LINE_LIST_MNG.INSERT (AFPX.LINE_LIST, LINE);
  end loop;
  AFPX.LINE_LIST_MNG.MOVE_TO(AFPX.LINE_LIST, NUMBER => 0, FROM_CURRENT=> FALSE);

  SET_DSCR(DSCR_NO);

  loop
    AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
    REDISPLAY := FALSE;
    case PTG_RESULT.EVENT is
      when AFPX.KEYBOARD =>
        case PTG_RESULT.KEYBOARD_KEY is
          when AFPX.RETURN_KEY =>
            null;
          when AFPX.ESCAPE_KEY =>
            DSCR_NO := DSCR_NO + 1;
            SET_DSCR(DSCR_NO);
          when AFPX.BREAK_KEY =>
            exit;
        end case;
      when AFPX.MOUSE_BUTTON =>
        null;
      when AFPX.FD_EVENT =>
        null;
      when AFPX.REFRESH =>
        REDISPLAY := TRUE;
    end case;
  end loop;

end T_DSCR;

