WIth CON_IO;
separate (MNG)
package body EDITION is

  -- For checking unicity of reference
  function SAME_REF (OP1, OP2 : OPER_DEF.OPER_REC) return BOOLEAN is
  begin
    return OP1.REFERENCE = OP2.REFERENCE;
  end SAME_REF;
  procedure SEARCH_REF is new OPER_LIST_MNG.SEARCH(SAME_REF);


  -- Affectation of kind and status buttons
  KIND_BUTTONS : constant array (OPER_DEF.KIND_LIST) of AFPX.FIELD_RANGE :=
    (OPER_DEF.CHEQUE => 22,
     OPER_DEF.CREDIT => 23,
     OPER_DEF.TRANSFER => 24,
     OPER_DEF.WITHDRAW => 25);

  STATUS_BUTTONS : constant array (OPER_DEF.STATUS_LIST) of AFPX.FIELD_RANGE :=
    (OPER_DEF.ENTERED => 27,
     OPER_DEF.NOT_ENTERED => 28,
     OPER_DEF.DEFERED => 29);

  -- Protect get and button fields &  set colors.
  procedure PROTECT_DATA is
  begin
    AFPX.SET_FIELD_PROTECTION(13, TRUE);
    AFPX.SET_FIELD_COLORS(13, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION(15, TRUE);
    AFPX.SET_FIELD_COLORS(15, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION(17, TRUE);
    AFPX.SET_FIELD_COLORS(17, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION(19, TRUE);
    AFPX.SET_FIELD_COLORS(19, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION(20, TRUE);
    AFPX.SET_FIELD_COLORS(20, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION(22, TRUE);
    AFPX.SET_FIELD_COLORS(22, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION(23, TRUE);
    AFPX.SET_FIELD_COLORS(23, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION(24, TRUE);
    AFPX.SET_FIELD_COLORS(24, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION(25, TRUE);
    AFPX.SET_FIELD_COLORS(25, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION(27, TRUE);
    AFPX.SET_FIELD_COLORS(27, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION(28, TRUE);
    AFPX.SET_FIELD_COLORS(28, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION(29, TRUE);
    AFPX.SET_FIELD_COLORS(29, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION(31, TRUE);
    AFPX.SET_FIELD_COLORS(31, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION(33, TRUE);
    AFPX.SET_FIELD_COLORS(33, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION(35, TRUE);
    AFPX.SET_FIELD_COLORS(35, CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK);
  end PROTECT_DATA;


  -- Title, data protection,
  procedure PREPARE (EDIT_TYPE : in EDIT_LIST) is
  begin
    AFPX.USE_DESCRIPTOR(3);
    SCREEN.ENCODE_FILE_NAME(TEXT_HANDLER.VALUE(ACCOUNT_NAME));
    SCREEN.ENCODE_NB_OPER(OPER_LIST_MNG.LIST_LENGTH(OPER_LIST),
                          SEL_LIST_MNG.LIST_LENGTH(SEL_LIST));
    SCREEN.ENCODE_SAVED(ACCOUNT_SAVED);
    case EDIT_TYPE is
      when CREATE =>
        AFPX.ENCODE_FIELD(9, (0, 0), "creation");
        -- Disable No
        AFPX.SET_FIELD_ACTIVATION(10, FALSE);
        AFPX.SET_FIELD_ACTIVATION(11, FALSE);
        -- Enable copy if list not empty
        AFPX.SET_FIELD_ACTIVATION(36, not OPER_LIST_MNG.IS_EMPTY(OPER_LIST));
      when MODIFY =>
        AFPX.ENCODE_FIELD(9, (0, 0), "modification");
        AFPX.SET_FIELD_ACTIVATION(36, FALSE);
      when VIEW =>
        AFPX.ENCODE_FIELD(9, (0, 0), "visualisation");
        AFPX.SET_FIELD_ACTIVATION(36, FALSE);
        PROTECT_DATA;
      when DELETE =>
        AFPX.ENCODE_FIELD(9, (0, 0), "deletion");
        AFPX.SET_FIELD_ACTIVATION(36, FALSE);
        PROTECT_DATA;
    end case;
  end PREPARE;

  -- Set buttons colors according to current kind/status
  procedure SET_BUTTONS(ALLOW_EDIT : in BOOLEAN;
                        KIND : in OPER_DEF.KIND_LIST;
                        STATUS : in OPER_DEF.STATUS_LIST) is

    -- Set one button color according to active or not and modifiable or not
    procedure SET_ACTIVE (BUTTON : in AFPX.FIELD_RANGE;
                          ACTIVE : in BOOLEAN) is
    begin
      -- Default (except unselected and view/delete)
      AFPX.SET_FIELD_ACTIVATION(BUTTON, TRUE);
      if ALLOW_EDIT then
        if ACTIVE then
          -- Active and modifiable
          AFPX.SET_FIELD_COLORS(BUTTON,
                                FOREGROUND => CON_IO.MAGENTA);
        else
          -- Inactive and modifiable
          AFPX.SET_FIELD_COLORS(BUTTON,
                                FOREGROUND => CON_IO.CYAN);
        end if;
      else
        if ACTIVE then
          -- Inactive and modifiable
          AFPX.SET_FIELD_COLORS(BUTTON,
                                FOREGROUND => CON_IO.LIGHT_GRAY);
        else
          AFPX.SET_FIELD_ACTIVATION(BUTTON, FALSE);
        end if;
      end if;
    end SET_ACTIVE;

    use type OPER_DEF.KIND_LIST, OPER_DEF.STATUS_LIST;
  begin
    for K in OPER_DEF.KIND_LIST loop
      SET_ACTIVE(KIND_BUTTONS(K), K = KIND);
    end loop;
    for S in OPER_DEF.STATUS_LIST loop
      SET_ACTIVE(STATUS_BUTTONS(S), S = STATUS);
    end loop;
    -- Allow defered (check done in update_buttons)
    if STATUS = OPER_DEF.DEFERED
    and then not OPER_DEF.KIND_CAN_BE_DEFERED(KIND) then
      raise PROGRAM_ERROR;
    end if;
    AFPX.SET_FIELD_ACTIVATION(STATUS_BUTTONS(OPER_DEF.DEFERED),
                              OPER_DEF.KIND_CAN_BE_DEFERED(KIND));
  end SET_BUTTONS;

  -- Update buttons after a click
  procedure UPDATE_BUTTONS(FIELD  : in AFPX.FIELD_RANGE;
                           KIND   : in out OPER_DEF.KIND_LIST;
                           STATUS : in out OPER_DEF.STATUS_LIST) is
    use type AFPX.FIELD_RANGE, OPER_DEF.KIND_LIST, OPER_DEF.STATUS_LIST;
  begin
    for K in OPER_DEF.KIND_LIST loop
      if FIELD = KIND_BUTTONS(K) then
        -- New kind
        KIND := K;
        -- Update Status if defered and not allowed
        if STATUS = OPER_DEF.DEFERED
        and then not OPER_DEF.KIND_CAN_BE_DEFERED(KIND) then
          STATUS := OPER_DEF.NOT_ENTERED;
        elsif STATUS = OPER_DEF.NOT_ENTERED
        and then OPER_DEF.KIND_CAN_BE_DEFERED(KIND) then
          STATUS := OPER_DEF.DEFERED;
        end if;
        SET_BUTTONS(TRUE, KIND, STATUS);
        return;
      end if;
    end loop;
    for S in OPER_DEF.STATUS_LIST loop
      if FIELD = STATUS_BUTTONS(S) then
        -- New status
        STATUS := S;
        SET_BUTTONS(TRUE, KIND, STATUS);
        return;
      end if;
    end loop;
    -- Never reached
    raise PROGRAM_ERROR;
  end UPDATE_BUTTONS;

  procedure PROTECT_MOVEMENTS (EDIT_TYPE : in EDIT_LIST) is
  begin
    if EDIT_TYPE = CREATE then
      -- Always and only allow OK, Cancel, and ok_and_next
      AFPX.SET_FIELD_ACTIVATION(38, FALSE);
      AFPX.SET_FIELD_ACTIVATION(39, FALSE);
      AFPX.SET_FIELD_ACTIVATION(43, FALSE);
      return;
    end if;

    -- Now list cannot be empty: protect out-of-list
    AFPX.SET_FIELD_ACTIVATION(38, SEL_LIST_MNG.GET_POSITION(SEL_LIST) /= 1);
    AFPX.SET_FIELD_ACTIVATION(39, SEL_LIST_MNG.GET_POSITION(SEL_LIST) /= 1);
    AFPX.SET_FIELD_ACTIVATION(42, SEL_LIST_MNG.GET_POSITION(SEL_LIST)
                               /= SEL_LIST_MNG.LIST_LENGTH(SEL_LIST));
    AFPX.SET_FIELD_ACTIVATION(43, SEL_LIST_MNG.GET_POSITION(SEL_LIST)
                               /= SEL_LIST_MNG.LIST_LENGTH(SEL_LIST));

    if EDIT_TYPE = VIEW then
      -- Only allow OK
      AFPX.SET_FIELD_ACTIVATION(39, FALSE);
      AFPX.SET_FIELD_ACTIVATION(41, FALSE);
      AFPX.SET_FIELD_ACTIVATION(43, FALSE);
    elsif EDIT_TYPE = DELETE then
      -- Only allow back
      AFPX.SET_FIELD_ACTIVATION(38, FALSE);
      AFPX.SET_FIELD_ACTIVATION(39, FALSE);
      AFPX.SET_FIELD_ACTIVATION(42, FALSE);
      AFPX.SET_FIELD_ACTIVATION(43, FALSE);
    end if;
  end PROTECT_MOVEMENTS;

  -- Set unit button according to current unit
  procedure SET_UNIT is
    use type UNIT_FORMAT.UNITS_LIST;
  begin
    if UNIT_FORMAT.GET_CURRENT_UNIT = UNIT_FORMAT.EUROS then
      AFPX.ENCODE_FIELD(19, (0, 1), "e");
    else
      AFPX.ENCODE_FIELD(19, (0, 1), "F");
    end if;
  end SET_UNIT;

  -- Encode operation
  procedure ENCODE_OPER (EDIT_TYPE : in EDIT_LIST;
                         OPER : in OPER_DEF.OPER_REC;
                         DELETED : in BOOLEAN) is
    DATE_STR : UNIT_FORMAT.SHORT_DATE_STR;
    use type OPER_DEF.AMOUNT_RANGE;
  begin
    if EDIT_TYPE /= CREATE then
      -- No
      AFPX.ENCODE_FIELD(11, (0, 0),
          NORMAL(OPER_LIST_MNG.GET_POSITION(OPER_LIST), 4));
    end if;
    -- Date
    DATE_STR := UNIT_FORMAT.SHORT_DATE_IMAGE(OPER.DATE);
    AFPX.ENCODE_FIELD(13, (0, 0), DATE_STR(1 .. 2));
    AFPX.ENCODE_FIELD(15, (0, 0), DATE_STR(4 .. 5));
    AFPX.ENCODE_FIELD(17, (0, 0), DATE_STR(7 .. 8));
    -- Amount
    if OPER.AMOUNT /= 0.0 then
      AFPX.ENCODE_FIELD(20, (0, 0), UNIT_FORMAT.IMAGE(OPER.AMOUNT, TRUE));
    else
      AFPX.CLEAR_FIELD(20);
    end if;
    SET_UNIT;
    -- Kind and status (modifiable or not)
    SET_BUTTONS(EDIT_TYPE in CREATE .. MODIFY, OPER.KIND, OPER.STATUS);
    -- 3 strings
    AFPX.ENCODE_FIELD(31, (0, 0), OPER.DESTINATION);
    AFPX.ENCODE_FIELD(33, (0, 0), OPER.COMMENT);
    AFPX.ENCODE_FIELD(35, (0, 0), OPER.REFERENCE);
    -- Deleted
    AFPX.SET_FIELD_ACTIVATION(37, DELETED);
  end ENCODE_OPER;

  procedure UPDATE is
  begin
    ACCOUNT_SAVED := FALSE;
    SCREEN.ENCODE_NB_OPER(OPER_LIST_MNG.LIST_LENGTH(OPER_LIST)
                             - DELETION.GET_NB_DELETED,
                          SEL_LIST_MNG.LIST_LENGTH(SEL_LIST)
                             - DELETION.GET_NB_DELETED);
    SCREEN.ENCODE_SAVED(ACCOUNT_SAVED);

    COMPUTE_AMOUNTS;
    LIST_UTIL.MOVE_TO_CURRENT;
  end UPDATE;

  -- Cancel (a deletion)
  procedure CANCEL (EDIT_TYPE : in EDIT_LIST) is
  begin
    if EDIT_TYPE = DELETE then
      DELETION.FLAG_UNDELETED;
      UPDATE;
    end if;
  end CANCEL;

  -- Check data. If OK, write/delete it and return 0,
  --   otherwise return the field of error
  function VALIDATE (EDIT_TYPE : in EDIT_LIST;
                     KIND : OPER_DEF.KIND_LIST;
                     STATUS : OPER_DEF.STATUS_LIST)
           return AFPX.ABSOLUTE_FIELD_RANGE is

    OPER, SAVED_OPER : OPER_DEF.OPER_REC;
    SAVED_MOVEMENT : OPER_LIST_MNG.MOVEMENT;
    DATE_STR : UNIT_FORMAT.DATE_STR;
    FIELD : AFPX.ABSOLUTE_FIELD_RANGE := 0;
    POS : POSITIVE;
    SEL : SEL_REC;
    use type OPER_DEF.STATUS_LIST, OPER_DEF.KIND_LIST,
             AFPX.ABSOLUTE_FIELD_RANGE;
  begin
    if EDIT_TYPE = VIEW then
      return 0;
    elsif EDIT_TYPE = DELETE then
      -- Flag selected operation as deleted
      DELETION.FLAG_DELETED;
      UPDATE;
      return 0;
    end if;

    -- Check data, return error field
    -- Date
    FIELD := 13;
    DATE_STR := AFPX.DECODE_FIELD(13,0) & '/'
              & AFPX.DECODE_FIELD(15,0) & '/'
       & "20" & AFPX.DECODE_FIELD(17,0);
    OPER.DATE := UNIT_FORMAT.DATE_VALUE(DATE_STR);
    -- Amount
    FIELD := 20;
    OPER.AMOUNT := UNIT_FORMAT.VALUE(AFPX.DECODE_FIELD(20, 0));
    -- Kind and status
    OPER.KIND := KIND;
    OPER.STATUS := STATUS;
    if OPER.STATUS = OPER_DEF.DEFERED
    and then not OPER_DEF.KIND_CAN_BE_DEFERED(OPER.KIND) then
      -- Should be protected at buttons level
      raise PROGRAM_ERROR;
    end if;

    -- Strings
    OPER.DESTINATION := AFPX.DECODE_FIELD(31,0);
    OPER.COMMENT     := AFPX.DECODE_FIELD(33,0);
    OPER.REFERENCE   := AFPX.DECODE_FIELD(35,0);

    -- Non empty reference unique for cheque
    FIELD := 35;
    if OPER.KIND = OPER_DEF.CHEQUE
    and then OPER.REFERENCE /= OPER_DEF.REFERENCE_STR'(others => ' ') then

      -- Remove current temporaly (just for search)
      if EDIT_TYPE = MODIFY then
        -- Save current oper and position before deletion
        POS := OPER_LIST_MNG.GET_POSITION(OPER_LIST);
        OPER_LIST_MNG.READ(OPER_LIST, SAVED_OPER, OPER_LIST_MNG.CURRENT);
        -- Remove current oper so we won't find it
        --  move to next if possible.
        if OPER_LIST_MNG.LIST_LENGTH(OPER_LIST) = 1
        or else OPER_LIST_MNG.GET_POSITION(OPER_LIST)
             /= OPER_LIST_MNG.LIST_LENGTH(OPER_LIST) then
          -- Last item of the list or not end of list
          SAVED_MOVEMENT := OPER_LIST_MNG.NEXT;
        else
          SAVED_MOVEMENT := OPER_LIST_MNG.PREV;
        end if;
        OPER_LIST_MNG.DELETE(OPER_LIST, SAVED_MOVEMENT);
        if OPER_LIST_MNG.IS_EMPTY(OPER_LIST) then
          -- List becomes empty
          SAVED_MOVEMENT := OPER_LIST_MNG.CURRENT;
        end if;
      end if;

      -- Search
      begin
        SEARCH_REF(OPER_LIST, OPER, FROM_CURRENT => FALSE);
        -- oh, oh. Found another one
      exception
        when OPER_LIST_MNG.NOT_IN_LIST =>
          -- Ok. Ref is unique
          FIELD := 0;
      end;

      -- Restore 
      if EDIT_TYPE = MODIFY then
        case SAVED_MOVEMENT is
          when OPER_LIST_MNG.NEXT =>
            -- Not last elem was removed, and we moved to next
            -- Move to this element and insert before
            OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.NEXT, POS-1, FALSE);
            OPER_LIST_MNG.INSERT(OPER_LIST, SAVED_OPER, OPER_LIST_MNG.PREV);
          when OPER_LIST_MNG.PREV =>
            -- Last elem was removed, and we moved to previous
            -- Insert ad end
            OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.PREV, 0, FALSE);
            OPER_LIST_MNG.INSERT(OPER_LIST, SAVED_OPER, OPER_LIST_MNG.NEXT);
          when OPER_LIST_MNG.CURRENT =>
            -- List is empty
            OPER_LIST_MNG.INSERT(OPER_LIST, SAVED_OPER, OPER_LIST_MNG.NEXT);
        end case;
      end if;
      if FIELD /= 0 then
        -- Generate error
        raise UNIT_FORMAT.FORMAT_ERROR;
      end if;
    end if; -- Cheque ref is unique

    -- Data ok: insert or modify
    if EDIT_TYPE = MODIFY then
      OPER_LIST_MNG.MODIFY(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
    else
      -- Insert at the end to keep selection accurate
      if not OPER_LIST_MNG.IS_EMPTY(OPER_LIST) then
        OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.PREV, 0 , FALSE);
      end if;
      OPER_LIST_MNG.INSERT(OPER_LIST, OPER);
      -- Insert at the end of selection and go back to current (for next copy)
      if not SEL_LIST_MNG.IS_EMPTY(SEL_LIST) then
        LIST_UTIL.SAVE_POS;
        SEL_LIST_MNG.MOVE_TO(SEL_LIST, SEL_LIST_MNG.PREV, 0 , FALSE);
        SEL_LIST_MNG.INSERT(SEL_LIST, (NO => OPER_LIST_MNG.LIST_LENGTH(OPER_LIST),
                                       DELETED => FALSE) );
        LIST_UTIL.RESTORE_POS;
        LIST_UTIL.MOVE_TO_CURRENT;
      else
        SEL_LIST_MNG.INSERT(SEL_LIST, (NO => OPER_LIST_MNG.LIST_LENGTH(OPER_LIST),
                                       DELETED => FALSE) );
      end if;
    end if;
    UPDATE;
    return 0;
  exception
    when UNIT_FORMAT.FORMAT_ERROR =>
      return FIELD;
  end VALIDATE;

  -- Do the edition  
  procedure EDIT (EDIT_TYPE : in EDIT_LIST) is
    -- Original unit to restore
    ORIG_UNIT : constant UNIT_FORMAT.UNITS_LIST
              := UNIT_FORMAT.GET_CURRENT_UNIT;
    OPER : OPER_DEF.OPER_REC;
    -- Afpx put_then_get stuff
    CURSOR_FIELD : AFPX.ABSOLUTE_FIELD_RANGE := 0;
    CURSOR_COL   : CON_IO.COL_RANGE := 0;
    PTG_RESULT   : AFPX.RESULT_REC;
    REDISPLAY    : BOOLEAN := FALSE;
    -- Current Kind and Status
    KIND : OPER_DEF.KIND_LIST;
    STATUS : OPER_DEF.STATUS_LIST;
    -- Deleted flag read from selection list
    SEL : SEL_REC;
    DELETED : BOOLEAN;
    -- Is OK_and_NEXT active when keyboard Return
    OKNEXT_ACTIVE : BOOLEAN;
    use type AFPX.ABSOLUTE_FIELD_RANGE, OPER_DEF.KIND_LIST;
  begin

    -- Set title, fields protections & buttons
    PREPARE(EDIT_TYPE);
    ALL_EDIT:
    loop
      -- Move to current for copy, edit, view, delete
      if not SEL_LIST_MNG.IS_EMPTY(SEL_LIST) then
        LIST_UTIL.MOVE_TO_CURRENT;
      end if;
      -- Set data
      if EDIT_TYPE = CREATE then
        -- Default operation, credit defered.
        OPER.DATE := OPER_DEF.CURRENT_DATE;
        OPER.AMOUNT := 0.0;
        OPER.KIND := OPER_DEF.CREDIT;
        OPER.STATUS := OPER_DEF.DEFERED;
        OPER.REFERENCE := (others => ' ');
        OPER.DESTINATION := (others => ' ');
        OPER.COMMENT := (others => ' ');
        DELETED := FALSE;
      else
        SEL_LIST_MNG.READ(SEL_LIST, SEL, SEL_LIST_MNG.CURRENT);
        DELETED := SEL.DELETED;
        -- Current operation
        OPER_LIST_MNG.READ(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
      end if;
      -- Encode data
      ENCODE_OPER(EDIT_TYPE, OPER, DELETED);
      KIND := OPER.KIND;
      STATUS := OPER.STATUS;
      -- Prepare PTG
      CURSOR_FIELD := AFPX.NEXT_CURSOR_FIELD(0);
      if CURSOR_FIELD = 0 then
        -- No get field
        CURSOR_FIELD := 1;
      else
        CURSOR_COL := 0;
      end if;
      -- Protect movements
      PROTECT_MOVEMENTS(EDIT_TYPE);

      -- PTGs
      ONE_EDIT:
      loop
        AFPX.PUT_THEN_GET(CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
        REDISPLAY := FALSE;
        case PTG_RESULT.EVENT is

          when AFPX.KEYBOARD =>
            case PTG_RESULT.KEYBOARD_KEY is
              when AFPX.RETURN_KEY =>
                -- OK and back or next
                CURSOR_FIELD := VALIDATE(EDIT_TYPE, KIND, STATUS);
                CURSOR_COL := 0;
                if CURSOR_FIELD = 0 then
                  -- Check that OK_AND_NEXT button is active
		  AFPX.GET_FIELD_ACTIVATION(42, OKNEXT_ACTIVE);
                  if not OKNEXT_ACTIVE then
                    -- Ok and back
                    exit ALL_EDIT;
                  else
                    if EDIT_TYPE /= CREATE then
                      -- Next oper
                      SEL_LIST_MNG.MOVE_TO(SEL_LIST, SEL_LIST_MNG.NEXT);
                    end if;
                    exit ONE_EDIT;
                  end if;
                end if;
                SCREEN.RING(TRUE);
                -- Return = OK
                SCREEN.RING(TRUE);
              when AFPX.ESCAPE_KEY =>
                -- Escape = Cancel
                CANCEL(EDIT_TYPE);
                exit ALL_EDIT;
              when AFPX.BREAK_KEY =>
                -- Break = Cancel
                exit ALL_EDIT;
            end case;

          when AFPX.MOUSE_BUTTON =>
            case PTG_RESULT.FIELD_NO is
              when 19 =>
                -- Change unit
                UNIT_FORMAT.SWITCH_UNIT;
                SET_UNIT;

              when 22 .. 25 | 27 .. 29 =>
                -- Kind and status buttons
                UPDATE_BUTTONS(PTG_RESULT.FIELD_NO, KIND, STATUS);
                if EDIT_TYPE = CREATE then
                  CURSOR_FIELD := AFPX.NEXT_CURSOR_FIELD(PTG_RESULT.FIELD_NO);
                  CURSOR_COL := 0;
                end if;
              when 36 =>
                -- Copy when create
                OPER_LIST_MNG.READ(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
                if OPER.KIND = OPER_DEF.CREDIT then
                  OPER.STATUS := OPER_DEF.DEFERED;
                else
                  OPER.STATUS := OPER_DEF.NOT_ENTERED;
                end if;
                ENCODE_OPER(EDIT_TYPE, OPER, FALSE);
                KIND := OPER.KIND;
                STATUS := OPER.STATUS;
                CURSOR_FIELD := AFPX.NEXT_CURSOR_FIELD(0);
                CURSOR_COL := 0;

              when 38 =>
                -- OK and prev
                CURSOR_FIELD := VALIDATE(EDIT_TYPE, KIND, STATUS);
                CURSOR_COL := 0;
                if CURSOR_FIELD = 0 then
                  -- Prev oper
                  SEL_LIST_MNG.MOVE_TO(SEL_LIST, SEL_LIST_MNG.PREV);
                  exit ONE_EDIT;
                end if;
                SCREEN.RING(TRUE);
              when 39 =>
                -- Cancel and prev
                CANCEL(EDIT_TYPE);
                SEL_LIST_MNG.MOVE_TO(SEL_LIST, SEL_LIST_MNG.PREV);
                exit ONE_EDIT;
              when 40 =>
                -- OK and back
                CURSOR_FIELD := VALIDATE(EDIT_TYPE, KIND, STATUS);
                CURSOR_COL := 0;
                exit ALL_EDIT when CURSOR_FIELD = 0;
                SCREEN.RING(TRUE);
              when 41 =>
                -- Cancel and back
                CANCEL(EDIT_TYPE);
                exit ALL_EDIT;
              when 42 =>
                -- OK and next
                CURSOR_FIELD := VALIDATE(EDIT_TYPE, KIND, STATUS);
                CURSOR_COL := 0;
                if CURSOR_FIELD = 0 then
                  if EDIT_TYPE /= CREATE then
                    -- Next oper
                    SEL_LIST_MNG.MOVE_TO(SEL_LIST, SEL_LIST_MNG.NEXT);
                  end if;
                  exit ONE_EDIT;
                end if;
                SCREEN.RING(TRUE);
              when 43 =>
                -- Cancel and next
                CANCEL(EDIT_TYPE);
                SEL_LIST_MNG.MOVE_TO(SEL_LIST, SEL_LIST_MNG.NEXT);
                exit ONE_EDIT;

              when others =>
                null;
            end case;

          when AFPX.REFRESH =>
            REDISPLAY := TRUE;
          when AFPX.FD_EVENT =>
            null;
        end case;

      end loop ONE_EDIT;

    end loop ALL_EDIT;

    if EDIT_TYPE = DELETE then
      DELETION.COMMIT_DELETIONS;
    elsif EDIT_TYPE = CREATE
    and then not SEL_LIST_MNG.IS_EMPTY(SEL_LIST) then
      -- Move to bottom
      SEL_LIST_MNG.MOVE_TO(SEL_LIST, SEL_LIST_MNG.PREV, 0, FALSE);
    end if;

    -- Restore original unit
    UNIT_FORMAT.SET_UNIT_TO(ORIG_UNIT);

  end EDIT;

end EDITION;

