with STRING_MNG, NORMAL, CON_IO, AFPX;
package body SCREEN is
  type MODES_LIST is (DEFAULT, CONFIRM, ACK);

  EDIT_ALLOWED : BOOLEAN := FALSE;
  SUBLIST_ACTIVE : BOOLEAN := FALSE;

  procedure UPDATE_TO_UNIT is
    use type UNIT_FORMAT.UNITS_LIST;
  begin
    if UNIT_FORMAT.GET_CURRENT_UNIT = UNIT_FORMAT.EUROS then
      AFPX.RESET_FIELD(36);
    else
      AFPX.CLEAR_FIELD(36);
      AFPX.SET_FIELD_COLORS(36, FOREGROUND => CON_IO.BLUE);
      AFPX.ENCODE_FIELD(36, (0, 1), "TO EUROS");
    end if;
  end UPDATE_TO_UNIT;

  procedure SET_MODE (MODE : in MODES_LIST) is
  begin
    -- List unprotected in default
    AFPX.SET_FIELD_PROTECTION(AFPX.LIST_FIELD_NO, MODE /= DEFAULT);
    -- Oper buttons
    for F in AFPX.FIELD_RANGE'(23) .. 24 loop
      AFPX.SET_FIELD_ACTIVATION(F, MODE = DEFAULT);
    end loop;
    for F in AFPX.FIELD_RANGE'(25) .. 28 loop
      AFPX.SET_FIELD_ACTIVATION(F, MODE = DEFAULT and then EDIT_ALLOWED);
    end loop;
    if SUBLIST_ACTIVE then
      AFPX.ENCODE_FIELD(29, (0, 1), "UN SEL");
    else
      AFPX.RESET_FIELD(29, RESET_COLORS => FALSE);
    end if;
    AFPX.SET_FIELD_ACTIVATION(29, MODE = DEFAULT and then EDIT_ALLOWED);
    AFPX.SET_FIELD_ACTIVATION(30, MODE = DEFAULT and then SUBLIST_ACTIVE);
    -- Account buttons
    for F in AFPX.FIELD_RANGE'(31) .. 38 loop
       AFPX.SET_FIELD_ACTIVATION(F, MODE = DEFAULT);
    end loop;
    -- To francs/Euros button
    if MODE = DEFAULT then
      UPDATE_TO_UNIT;
    end if;
    -- Message
    AFPX.CLEAR_FIELD(39);
    -- Confirm Ack
    AFPX.SET_FIELD_ACTIVATION(40, MODE /= DEFAULT);
    if MODE = CONFIRM then
      AFPX.RESET_FIELD(40, RESET_COLORS => FALSE);
    elsif MODE = ACK then
      AFPX.ENCODE_FIELD(40, (0, 1), "ACK");
    end if;

    AFPX.SET_FIELD_ACTIVATION(41, MODE = CONFIRM);
  end SET_MODE;

  procedure ALLOW_EDIT (ALLOW : in BOOLEAN) is
  begin
    EDIT_ALLOWED := ALLOW;
    for F in AFPX.FIELD_RANGE'(25) .. 29 loop
      AFPX.SET_FIELD_ACTIVATION(F, EDIT_ALLOWED);
    end loop;
  end ALLOW_EDIT;

  procedure SUBLIST (ACTIVE : in BOOLEAN) is
  begin
    SUBLIST_ACTIVE := ACTIVE;
    if SUBLIST_ACTIVE then
      AFPX.ENCODE_FIELD(29, (0, 1), "UN SEL");
    else
       AFPX.RESET_FIELD(29, RESET_COLORS => FALSE);
    end if;
    AFPX.SET_FIELD_ACTIVATION(30, SUBLIST_ACTIVE);
  end SUBLIST;

  -- Reset to default screen
  procedure RESET is
  begin
    AFPX.USE_DESCRIPTOR(1);
    AFPX.LINE_LIST_MNG.DELETE_LIST(AFPX.LINE_LIST);
    SET_MODE(DEFAULT);
  end RESET;


  procedure ENCODE_FILE_NAME (FILE_NAME : in STRING) is
  begin
    AFPX.ENCODE_FIELD(1, (0, 0),
         STRING_MNG.PROCUSTE(FILE_NAME,
                           AFPX.GET_FIELD_WIDTH(1)));
  end ENCODE_FILE_NAME;

  procedure ENCODE_NB_OPER (OPER : in NATURAL; SELECTED : in NATURAL) is
  begin
    -- Set account number
    AFPX.ENCODE_FIELD(3, (0, 0),
        NORMAL(OPER, AFPX.GET_FIELD_WIDTH(3)));
    if OPER <= 1 then
      AFPX.ENCODE_FIELD(4, (0, 0), "operation ");
    else
      AFPX.ENCODE_FIELD(4, (0, 0), "operations");
    end if;
    AFPX.SET_FIELD_ACTIVATION(6, SUBLIST_ACTIVE);
    AFPX.ENCODE_FIELD(6, (0, 0),
               NORMAL(SELECTED, AFPX.GET_FIELD_WIDTH(6)));
    AFPX.SET_FIELD_ACTIVATION(7, SUBLIST_ACTIVE);
  end ENCODE_NB_OPER;

  procedure ENCODE_SAVED (SAVED : in BOOLEAN) is
  begin
     if SAVED then
       AFPX.RESET_FIELD(5, RESET_COLORS => TRUE, RESET_STRING => TRUE);
     else
       AFPX.ENCODE_FIELD(5, (0, 0), "NOT SAVED");
       AFPX.SET_FIELD_COLORS (5, CON_IO.RED);
     end if;
  end ENCODE_SAVED;

  procedure ENCODE_SUMMARY(REAL_AMOUNT, ACCOUNT_AMOUNT,
                           DEFERED_AMOUNT, MARGIN_AMOUNT :
                                    in OPER_DEF.AMOUNT_RANGE) is
  begin
    AFPX.ENCODE_FIELD (10, (0, 0), UNIT_FORMAT.IMAGE(REAL_AMOUNT, TRUE));
    AFPX.ENCODE_FIELD (12, (0, 0), UNIT_FORMAT.IMAGE(ACCOUNT_AMOUNT, TRUE));
    AFPX.ENCODE_FIELD (14, (0, 0), UNIT_FORMAT.IMAGE(DEFERED_AMOUNT, TRUE));
    AFPX.ENCODE_FIELD (16, (0, 0), UNIT_FORMAT.IMAGE(MARGIN_AMOUNT, TRUE));
  end ENCODE_SUMMARY;

  function MY_PTG return BOOLEAN is
    -- Afpx put_then_get stuff
    CURSOR_FIELD : AFPX.ABSOLUTE_FIELD_RANGE := 1;
    CURSOR_COL   : CON_IO.COL_RANGE := 0;
    PTG_RESULT   : AFPX.RESULT_REC;
    REDISPLAY    : BOOLEAN := FALSE;
  begin
    loop
      AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
      REDISPLAY := FALSE;
      case PTG_RESULT.EVENT is
        when AFPX.KEYBOARD =>
          case PTG_RESULT.KEYBOARD_KEY is
            when AFPX.RETURN_KEY =>
              return TRUE;
            when AFPX.ESCAPE_KEY =>
              return FALSE;
            when AFPX.BREAK_KEY =>
              null;
          end case;
        when AFPX.MOUSE_BUTTON =>
          case PTG_RESULT.FIELD_NO is
            when 40 =>
              return TRUE;
            when 41 =>
              return FALSE;
            when others =>
              null;
          end case;
        when AFPX.REFRESH =>
          REDISPLAY := TRUE;
        when AFPX.FD_EVENT | AFPX.TIMER_EVENT =>
          REDISPLAY := TRUE;
      end case;
    end loop;
  end MY_PTG;

  function CONFIRM_ACTION (ACTION : ACTION_LIST) return BOOLEAN is
    RESULT : BOOLEAN;
  begin
    SET_MODE(CONFIRM);
    case ACTION is
      when OVERWRITE_ACCOUNT =>
        RING(FALSE);
        AFPX.ENCODE_FIELD (39, (0, 0),
          "Account is not saved and will be overwritten. Confirm?");
      when OVERWRITE_FILE =>
        AFPX.ENCODE_FIELD (39, (0, 0),
          "Account file exists and will be overwritten. Confirm?");
      when QUIT_UNSAVED =>
        RING(FALSE);
        AFPX.ENCODE_FIELD (39, (0, 0),
          "Account is not saved and will be lost. Confirm?");
    end case;
    -- Get answer
    RESULT := MY_PTG;
    SET_MODE(DEFAULT);
    return RESULT;
  end CONFIRM_ACTION;

  procedure ACK_ERROR (ERROR : in ERROR_LIST) is
  begin
    SET_MODE(ACK);
    RING(TRUE);
    case ERROR is
      when FILE_ACCESS =>
        AFPX.ENCODE_FIELD (39, (0, 0),
          "File not found or not an account or access denied");
      when FILE_IO =>
        AFPX.ENCODE_FIELD (39, (0, 0), "File read or write error");
      when FILE_READ_ONLY =>
        AFPX.ENCODE_FIELD (39, (0, 0), "File is read-only");
      when FILE_NAME_TOO_LONG =>
        AFPX.ENCODE_FIELD (39, (0, 0), "File name too long");
      when ACCOUNT_FULL =>
        AFPX.ENCODE_FIELD (39, (0, 0), "Sorry, the account is full");
      when NOT_IMPLEMENTED =>
        AFPX.ENCODE_FIELD (39, (0, 0), "Sorry, not implmeneted yet");
      when INTERNAL_ERROR =>
        AFPX.ENCODE_FIELD (39, (0, 0), "Internal error. Saving in Tmp");
    end case;
    -- Loop until ack
    while not MY_PTG loop
      null;
    end loop;
    SET_MODE(DEFAULT);
  end ACK_ERROR;
 
  -- Ring alarm / question bell
  procedure RING (ALARM : in BOOLEAN) is
  begin
    if ALARM then
      CON_IO.BELL(3);
    else
      CON_IO.BELL(1);
    end if;
  end RING;

end SCREEN;

