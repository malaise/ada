with CON_IO;
separate (MNG)
package body EDITION is

  -- Affectation of kind and status buttons
  KIND_BUTTONS : constant array (OPER_DEF.KIND_LIST) of AFPX.FIELD_RANGE :=
    (OPER_DEF.CHEQUE => 18,
     OPER_DEF.CREDIT => 19,
     OPER_DEF.TRANSFER => 20,
     OPER_DEF.WITHDRAW => 21);

  STATUS_BUTTONS : constant array (OPER_DEF.STATUS_LIST) of AFPX.FIELD_RANGE :=
    (OPER_DEF.ENTERED => 23,
     OPER_DEF.NOT_ENTERED => 24,
     OPER_DEF.DEFERED => 25);
  CURRENT_BUTTON_COLOR : constant CON_IO.EFFECTIVE_BASIC_COLORS := CON_IO.MAGENTA;
  OTHER_BUTTON_COLOR   : constant CON_IO.EFFECTIVE_BASIC_COLORS := CON_IO.CYAN;

  procedure PROTECT_DATA is
    -- Protect get and button fields
  begin
    AFPX.SET_FIELD_PROTECTION( 9, TRUE);
    AFPX.SET_FIELD_PROTECTION(11, TRUE);
    AFPX.SET_FIELD_PROTECTION(13, TRUE);
    AFPX.SET_FIELD_PROTECTION(15, TRUE);
    AFPX.SET_FIELD_PROTECTION(16, TRUE);
    AFPX.SET_FIELD_PROTECTION(18, TRUE);
    AFPX.SET_FIELD_PROTECTION(19, TRUE);
    AFPX.SET_FIELD_PROTECTION(20, TRUE);
    AFPX.SET_FIELD_PROTECTION(21, TRUE);
    AFPX.SET_FIELD_PROTECTION(23, TRUE);
    AFPX.SET_FIELD_PROTECTION(24, TRUE);
    AFPX.SET_FIELD_PROTECTION(25, TRUE);
    AFPX.SET_FIELD_PROTECTION(27, TRUE);
    AFPX.SET_FIELD_PROTECTION(29, TRUE);
    AFPX.SET_FIELD_PROTECTION(31, TRUE);
  end PROTECT_DATA;

  -- Title, data protection,
  procedure PREPARE (EDIT_TYPE : in EDIT_LIST) is
  begin
    AFPX.USE_DESCRIPTOR(3);
    case EDIT_TYPE is
      when CREATE =>
        AFPX.ENCODE_FIELD(7, (0, 0), "creation");
      when COPY =>
        AFPX.ENCODE_FIELD(7, (0, 0), "copy");
      when MODIFY =>
        AFPX.ENCODE_FIELD(7, (0, 0), "modification");
      when VIEW =>
        AFPX.ENCODE_FIELD(7, (0, 0), "visualisation");
        PROTECT_DATA;
      when DELETE =>
        AFPX.ENCODE_FIELD(7, (0, 0), "deletion");
        PROTECT_DATA;
    end case;
  end PREPARE;

  procedure SET_BUTTONS(KIND : in OPER_DEF.KIND_LIST; STATUS : in OPER_DEF.STATUS_LIST) is
    use type OPER_DEF.KIND_LIST, OPER_DEF.STATUS_LIST;
  begin
    for K in OPER_DEF.KIND_LIST loop
      if K = KIND then
        AFPX.SET_FIELD_COLORS(KIND_BUTTONS(K), FOREGROUND => CURRENT_BUTTON_COLOR);
      else
        AFPX.SET_FIELD_COLORS(KIND_BUTTONS(K), FOREGROUND => OTHER_BUTTON_COLOR);
      end if;
    end loop;
    for S in OPER_DEF.STATUS_LIST loop
      if S = STATUS then
        AFPX.SET_FIELD_COLORS(STATUS_BUTTONS(S), FOREGROUND => CURRENT_BUTTON_COLOR);
      else
        AFPX.SET_FIELD_COLORS(STATUS_BUTTONS(S), FOREGROUND => OTHER_BUTTON_COLOR);
      end if;
    end loop;
  end SET_BUTTONS;

  -- Set unit button according to current unit
  procedure SET_UNIT is
    use type UNIT_FORMAT.UNITS_LIST;
  begin
    if UNIT_FORMAT.GET_CURRENT_UNIT = UNIT_FORMAT.EUROS then
      AFPX.ENCODE_FIELD(16, (0, 1), "e");
    else
      AFPX.ENCODE_FIELD(16, (0, 1), "F");
    end if;
  end SET_UNIT;

  procedure ENCODE_OPER (OPER : in OPER_DEF.OPER_REC) is
    DATE_STR : UNIT_FORMAT.SHORT_DATE_STR;
  begin
    -- Date
    DATE_STR := UNIT_FORMAT.SHORT_DATE_IMAGE(OPER.DATE);
    AFPX.ENCODE_FIELD( 9, (0, 0), DATE_STR(1 .. 2));
    AFPX.ENCODE_FIELD(11, (0, 0), DATE_STR(4 .. 5));
    AFPX.ENCODE_FIELD(13, (0, 0), DATE_STR(7 .. 8));
    -- Amount
    AFPX.ENCODE_FIELD(15, (0, 0), UNIT_FORMAT.IMAGE(OPER.AMOUNT));
    SET_UNIT;
    -- Kind and status
    SET_BUTTONS(OPER.KIND, OPER.STATUS);
    -- 3 strings
    AFPX.ENCODE_FIELD(27, (0, 0), OPER.DESTINATION);
    AFPX.ENCODE_FIELD(29, (0, 0), OPER.COMMENT);
    AFPX.ENCODE_FIELD(31, (0, 0), OPER.REFERENCE);
  end ENCODE_OPER;

  -- Check data. If OK, write/delete it and return 0, otherwise return the field of error
  function VALIDATE (EDIT_TYPE : in EDIT_LIST) return AFPX.ABSOLUTE_FIELD_RANGE is
    OPER : OPER_DEF.OPER_REC;
    DATE_STR : UNIT_FORMAT.DATE_STR;
    FIELD : AFPX.ABSOLUTE_FIELD_RANGE := 1;
  begin
    if EDIT_TYPE = VIEW then
      -- Nothing modified
      return 0;
    elsif EDIT_TYPE = DELETE then
      -- Nothing modified. Delete item.
      if OPER_LIST_MNG.GET_POSITION(OPER_LIST) /= OPER_LIST_MNG.LIST_LENGTH(OPER_LIST) then
        OPER_LIST_MNG.DELETE(OPER_LIST, OPER_LIST_MNG.NEXT);
      else
        OPER_LIST_MNG.DELETE(OPER_LIST, OPER_LIST_MNG.PREV);
      end if;
      ACCOUNT_SAVED := FALSE;
      COMPUTE_AMOUNTS;
      return 0;
    end if;

    -- Check data, return error field
    -- Date
    FIELD := 9;
    DATE_STR := AFPX.DECODE_FIELD( 9,0) & '/'
              & AFPX.DECODE_FIELD(11,0) & '/'
              & "20" & AFPX.DECODE_FIELD(13,0);
    OPER.DATE := UNIT_FORMAT.DATE_VALUE(DATE_STR);
    -- Amount
    FIELD := 15;
    OPER.AMOUNT := UNIT_FORMAT.VALUE(AFPX.DECODE_FIELD(15, 0));
    -- @@@

    -- Data ok
    if EDIT_TYPE = CREATE or else EDIT_TYPE = COPY then
      -- Insert after and go back to current (for next copy)
      OPER_LIST_MNG.INSERT(OPER_LIST, OPER);
      if OPER_LIST_MNG.LIST_LENGTH(OPER_LIST) /= 1 then
        OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.PREV);
      end if;
    elsif EDIT_TYPE = MODIFY then
      OPER_LIST_MNG.MODIFY(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
    end if;
    ACCOUNT_SAVED := FALSE;
    COMPUTE_AMOUNTS;
    return 0;
  exception
    when UNIT_FORMAT.FORMAT_ERROR =>
      return FIELD;
  end VALIDATE;

  -- Do the edition  
  procedure EDIT (EDIT_TYPE : in EDIT_LIST) is
    OPER : OPER_DEF.OPER_REC;
    -- Afpx put_then_get stuff
    CURSOR_FIELD : AFPX.ABSOLUTE_FIELD_RANGE := 0;
    CURSOR_COL   : CON_IO.COL_RANGE := 0;
    PTG_RESULT   : AFPX.RESULT_REC;
    REDISPLAY    : BOOLEAN := FALSE;
    use type AFPX.ABSOLUTE_FIELD_RANGE;
  begin

    -- Set title, fields protections & buttons
    PREPARE(EDIT_TYPE);
    ALL_EDIT:
    loop
      -- Set data
      if EDIT_TYPE = CREATE then
        -- Default operation
        OPER.DATE := OPER_DEF.CURRENT_DATE;
        OPER.AMOUNT := 0.0;
        OPER.KIND := OPER_DEF.CREDIT;
        OPER.STATUS := OPER_DEF.DEFERED;
        OPER.REFERENCE := (others => ' ');
        OPER.DESTINATION := (others => ' ');
        OPER.COMMENT := (others => ' ');
      else
        -- Current operation
        OPER_LIST_MNG.READ(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
        if EDIT_TYPE = COPY then
          OPER.DATE := OPER_DEF.CURRENT_DATE;
          OPER.STATUS := OPER_DEF.NOT_ENTERED;
        end if;
      end if;
      -- Encode data
      ENCODE_OPER(OPER);
      CURSOR_FIELD := AFPX.NEXT_CURSOR_FIELD(0);
      -- Ptg
      ONE_EDIT:
      loop
        AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
        REDISPLAY := FALSE;
        case PTG_RESULT.EVENT is

          when AFPX.KEYBOARD =>
            case PTG_RESULT.KEYBOARD_KEY is
              when AFPX.RETURN_KEY =>
                -- Return = OK
                CURSOR_FIELD := VALIDATE(EDIT_TYPE);
                exit ALL_EDIT when CURSOR_FIELD = 0;
              when AFPX.ESCAPE_KEY =>
                -- Escape = Cancel
                exit ALL_EDIT;
              when AFPX.BREAK_KEY =>
                -- Break = Cancel
                exit ALL_EDIT;
            end case;

          when AFPX.MOUSE_BUTTON =>
            case PTG_RESULT.FIELD_NO is
              -- @@@ kind status unit buttons
              when 32 =>
                -- OK
                CURSOR_FIELD := VALIDATE(EDIT_TYPE);
                exit ALL_EDIT when CURSOR_FIELD = 0;
              when 33 =>
                -- OK_and_AGAIN
                CURSOR_FIELD := VALIDATE(EDIT_TYPE);
                exit ONE_EDIT when CURSOR_FIELD = 0;
              when 34 =>
                -- CANCEL
                exit ALL_EDIT;
              when others =>
                null;
            end case;

          when AFPX.REFRESH =>
            REDISPLAY := TRUE;
        end case;

      end loop ONE_EDIT;
    end loop ALL_EDIT;

    -- Sort and move to end
    if EDIT_TYPE = CREATE or else EDIT_TYPE = COPY or else EDIT_TYPE = MODIFY then
      -- Order may be affected except by view and delete
      SORT(OPER_LIST);
    end if;
    if EDIT_TYPE /= VIEW and then not OPER_LIST_MNG.IS_EMPTY(OPER_LIST) then
      -- Account may be modified except by view
      OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.PREV, 0, FALSE);
    end if;

  end EDIT;

end EDITION;

