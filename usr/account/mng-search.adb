with CON_IO;
separate(MNG)

procedure SEARCH is
  -- Afpx put_then_get stuff
  CURSOR_FIELD : AFPX.ABSOLUTE_FIELD_RANGE := 0;
  CURSOR_COL   : CON_IO.COL_RANGE := 0;
  PTG_RESULT   : AFPX.RESULT_REC;
  REDISPLAY    : BOOLEAN := FALSE;
  -- Operation to match
  OPER : OPER_DEF.OPER_REC;

  -- Unselect current oper
  procedure UNSEL is
  begin
    if SEL_LIST_MNG.GET_POSITION(SEL_LIST) /= 1 then
      SEL_LIST_MNG.DELETE(SEL_LIST, SEL_LIST_MNG.PREV);
    else
      SEL_LIST_MNG.DELETE(SEL_LIST, SEL_LIST_MNG.NEXT);
    end if;
  end UNSEL;

  type MATCH_PROT is access
       function(CUR, CRIT : OPER_DEF.OPER_REC) return BOOLEAN;

  function REF_MATCH(CUR, CRIT : OPER_DEF.OPER_REC) return BOOLEAN is
    use type OPER_DEF.KIND_LIST;
  begin
    return CUR.KIND = OPER_DEF.CHEQUE
           and then CUR.REFERENCE = CRIT.REFERENCE;
  end REF_MATCH;

  function STATUS_MATCH(CUR, CRIT : OPER_DEF.OPER_REC) return BOOLEAN is
    use type OPER_DEF.STATUS_LIST;
  begin
    return CUR.STATUS = CRIT.STATUS;
  end STATUS_MATCH;

  -- Unselect all non matching oper
  procedure UNSEL_ALL(MATCH : in MATCH_PROT; CRIT : in OPER_DEF.OPER_REC) is
     OPER : OPER_DEF.OPER_REC;
  begin
    if SEL_LIST_MNG.IS_EMPTY(SEL_LIST) then
      return;
    end if;
    -- Scan from first
    SEL_LIST_MNG.MOVE_TO(SEL_LIST, SEL_LIST_MNG.NEXT, 0, FALSE);
    loop
      LIST_UTIL.MOVE_TO_CURRENT;
      OPER_LIST_MNG.READ(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
      if not MATCH(OPER, CRIT) then
        -- Remove current and move to next or remove last
        UNSEL;
        exit when SEL_LIST_MNG.IS_EMPTY(SEL_LIST);
      else
        -- Move to next
        exit when SEL_LIST_MNG.GET_POSITION(SEL_LIST)
                = SEL_LIST_MNG.LIST_LENGTH(SEL_LIST);
        SEL_LIST_MNG.MOVE_TO(SEL_LIST);
      end if;
    end loop;
  end UNSEL_ALL;
    

begin

  if IN_SUBLIST then
    -- Unselect
    UNSEL;
    REFRESH_SCREEN(UNCHANGED);
    return;
  end if; 

  -- Not in sublist: get criteria
  AFPX.USE_DESCRIPTOR(4);
  SCREEN.ENCODE_FILE_NAME(TEXT_HANDLER.VALUE(ACCOUNT_NAME));
  SCREEN.ENCODE_NB_OPER(OPER_LIST_MNG.LIST_LENGTH(OPER_LIST),
                        SEL_LIST_MNG.LIST_LENGTH(SEL_LIST));
  SCREEN.ENCODE_SAVED(ACCOUNT_SAVED);
  CURSOR_FIELD := AFPX.NEXT_CURSOR_FIELD(0);

  loop
    AFPX.PUT_THEN_GET(CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
    REDISPLAY := FALSE;
    case PTG_RESULT.EVENT is

      when AFPX.KEYBOARD =>
        case PTG_RESULT.KEYBOARD_KEY is
          when AFPX.RETURN_KEY =>
            -- Return = Search ref
            OPER.REFERENCE := AFPX.DECODE_FIELD(12, 0);
            UNSEL_ALL(REF_MATCH'access, OPER);
            IN_SUBLIST := TRUE;
            exit;
          when AFPX.ESCAPE_KEY | AFPX.BREAK_KEY =>
            -- Escape/Break = Cancel
            IN_SUBLIST := FALSE;
            exit;
        end case;
      when AFPX.MOUSE_BUTTON =>
        case PTG_RESULT.FIELD_NO is
          when 9 =>
            -- Defered
            OPER.STATUS := OPER_DEF.DEFERED;
            UNSEL_ALL(STATUS_MATCH'access, OPER);
            IN_SUBLIST := TRUE;
            exit;
          when 10 =>
            -- Not entered
            OPER.STATUS := OPER_DEF.NOT_ENTERED;
            UNSEL_ALL(STATUS_MATCH'access, OPER);
            IN_SUBLIST := TRUE;
            exit;
          when 13 =>
            -- Cancel
            IN_SUBLIST := FALSE;
            exit;
          when others =>
            null;
        end case;
      when AFPX.REFRESH =>
        REDISPLAY := TRUE;
      when AFPX.FD_EVENT =>
        null;
    end case;

  end loop;

  SCREEN.RESET;
  SCREEN.SUBLIST(IN_SUBLIST);
  REFRESH_SCREEN(BOTTOM);

end SEARCH;

