with TEXT_IO;
with TEXT_HANDLER, DYNAMIC_LIST, DIRECTORY, AFPX, SELECT_FILE, NORMAL, SYS_CALLS;
with FILE_MNG, OPER_LIST_MNG, SCREEN, UNIT_FORMAT;

-- Manage the whole acount status
package body MNG is

  OPER_LIST : OPER_LIST_MNG.LIST_TYPE;
  procedure SORT is new OPER_LIST_MNG.SORT (OPER_DEF.LESS_THAN);

  ACCOUNT_NAME : TEXT_HANDLER.TEXT(DIRECTORY.MAX_DIR_NAME_LEN);

  ACCOUNT_SAVED : BOOLEAN := TRUE;

  -- The one in first record of file
  ROOT_AMOUNT : OPER_DEF.AMOUNT_RANGE;

  -- The ones computed
  REAL_AMOUNT, ACCOUNT_AMOUNT, DEFERED_AMOUNT,
               MARGIN_AMOUNT : OPER_DEF.AMOUNT_RANGE;

  LOADING : BOOLEAN;

  procedure INIT_SELECT_FILE is
  begin
    AFPX.CLEAR_FIELD(1);
    if LOADING then
      AFPX.ENCODE_FIELD(1, (0, 0), "Loading an account");
    else
      AFPX.ENCODE_FIELD(1, (0, 0), "Saving an account");
    end if;
  end INIT_SELECT_FILE;
  function ACCOUNT_SELECT_FILE is new SELECT_FILE(INIT_SELECT_FILE);

  -- Set current in oper list from AFPX selected
  procedure SET_CURRENT (NO : in OPER_NB_RANGE) is
  begin
    if NO in OPER_RANGE then
      OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.NEXT, NO-1, FALSE);
    end if;
  end SET_CURRENT;
      

  -- Builds the AFPX line from oper
  function OPER_TO_LINE (NO : OPER_RANGE;
                         OPER : OPER_DEF.OPER_REC) return AFPX.LINE_REC is
    LINE : AFPX.LINE_REC;
    SEP : constant CHARACTER := '|';
  begin
    LINE.LEN := AFPX.GET_FIELD_WIDTH(6);
    LINE.STR(1 .. 71) :=
                NORMAL(NO, 4) & SEP
              & UNIT_FORMAT.SHORT_DATE_IMAGE(OPER.DATE) & SEP
              & UNIT_FORMAT.SHORT_IMAGE(OPER.AMOUNT) & SEP
              & UNIT_FORMAT.SHORT_STATUS_IMAGE(OPER.STATUS) & SEP
              & ' ' & UNIT_FORMAT.SHORT_KIND_IMAGE(OPER.KIND) & SEP
              & OPER.DESTINATION & SEP
              & OPER.COMMENT(1 .. 15) & SEP
              & OPER.REFERENCE;
    return LINE;
  end OPER_TO_LINE;

  package LIST_UTIL is
    -- These work only if list is not mofified between calls
    procedure SAVE_POS (MOVE_TO_FIRST : in BOOLEAN := TRUE);
    procedure RESTORE_POS;

    -- These don't affect saved pos
    procedure INSERT_AMOUNT (AMOUNT : in OPER_DEF.AMOUNT_RANGE);
    function GET_AMOUNT return OPER_DEF.AMOUNT_RANGE;
  end LIST_UTIL;

  package body LIST_UTIL is
    LOC_POS : OPER_RANGE;
    procedure SAVE_POS (MOVE_TO_FIRST : in BOOLEAN := TRUE) is
    begin
      LOC_POS := OPER_LIST_MNG.GET_POSITION(OPER_LIST);
      if MOVE_TO_FIRST then
        OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.NEXT, 0, FALSE);
      end if;
    end SAVE_POS;

    procedure RESTORE_POS is
    begin
      OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.NEXT, LOC_POS-1, FALSE);
    end RESTORE_POS;

    procedure INSERT_AMOUNT (AMOUNT : in OPER_DEF.AMOUNT_RANGE) is
      LOC_LOC_POS : NATURAL;
      OPER : OPER_DEF.OPER_REC;
    begin
      OPER.AMOUNT := AMOUNT;
      if OPER_LIST_MNG.IS_EMPTY(OPER_LIST) then
        LOC_LOC_POS := 0;
      else
        LOC_LOC_POS := OPER_LIST_MNG.GET_POSITION(OPER_LIST);
        OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.NEXT, 0, FALSE);
      end if;
      OPER_LIST_MNG.INSERT(OPER_LIST, OPER, OPER_LIST_MNG.PREV);
      if LOC_LOC_POS /= 0 then
        -- Restore position: we have added a record
        OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.NEXT,
                              LOC_LOC_POS, FALSE);
      end if;
    end INSERT_AMOUNT;

    function GET_AMOUNT return OPER_DEF.AMOUNT_RANGE is
      LOC_LOC_POS : POSITIVE;
      OPER : OPER_DEF.OPER_REC;
    begin
      LOC_LOC_POS := OPER_LIST_MNG.GET_POSITION(OPER_LIST);
      OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.NEXT, 0, FALSE);
      OPER_LIST_MNG.GET(OPER_LIST, OPER, OPER_LIST_MNG.NEXT);
      if not OPER_LIST_MNG.IS_EMPTY(OPER_LIST) then
        -- Restore position: we have removed a record
        OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.NEXT,
                              LOC_LOC_POS-2, FALSE);
      end if;
      return OPER.AMOUNT;
    end GET_AMOUNT;
      
  end LIST_UTIL;
      

  -- Reset the AFPX list from the oper list
  procedure RESET_LIST is
    OPER : OPER_DEF.OPER_REC;
    INDEX : OPER_RANGE;
  begin
    AFPX.LINE_LIST_MNG.DELETE_LIST(AFPX.LINE_LIST);
    if OPER_LIST_MNG.IS_EMPTY(OPER_LIST) then
      return;
    end if;

    LIST_UTIL.SAVE_POS;
    INDEX := 1;
    loop
      OPER_LIST_MNG.READ(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
      AFPX.LINE_LIST_MNG.INSERT (AFPX.LINE_LIST, OPER_TO_LINE(INDEX, OPER));
      exit when OPER_LIST_MNG.GET_POSITION(OPER_LIST)
              = OPER_LIST_MNG.LIST_LENGTH(OPER_LIST);
      OPER_LIST_MNG.MOVE_TO(OPER_LIST);
      INDEX := INDEX + 1;
    end loop;
    LIST_UTIL.RESTORE_POS;
  end RESET_LIST;


  procedure COMPUTE_AMOUNTS is
    OPER : OPER_DEF.OPER_REC;
    use type OPER_DEF.AMOUNT_RANGE;
  begin
    REAL_AMOUNT := ROOT_AMOUNT;
    ACCOUNT_AMOUNT := ROOT_AMOUNT;
    DEFERED_AMOUNT := 0.0;
    MARGIN_AMOUNT  := 0.0;
    if OPER_LIST_MNG.IS_EMPTY(OPER_LIST) then
      return;
    end if;
    LIST_UTIL.SAVE_POS;
    loop
      OPER_LIST_MNG.READ(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
      REAL_AMOUNT := REAL_AMOUNT + OPER.AMOUNT;
      case OPER.STATUS is
        when OPER_DEF.ENTERED =>
          ACCOUNT_AMOUNT := ACCOUNT_AMOUNT + OPER.AMOUNT;
        when OPER_DEF.NOT_ENTERED =>
          if OPER.AMOUNT < 0.0 then
            MARGIN_AMOUNT := MARGIN_AMOUNT + OPER.AMOUNT;
          end if;
        when OPER_DEF.DEFERED =>
          DEFERED_AMOUNT := DEFERED_AMOUNT + OPER.AMOUNT;
          if OPER.AMOUNT < 0.0 then
            MARGIN_AMOUNT := MARGIN_AMOUNT + OPER.AMOUNT;
          end if;
      end case;
      exit when OPER_LIST_MNG.GET_POSITION(OPER_LIST)
              = OPER_LIST_MNG.LIST_LENGTH(OPER_LIST);
      OPER_LIST_MNG.MOVE_TO(OPER_LIST);
    end loop;
    LIST_UTIL.RESTORE_POS;
  end COMPUTE_AMOUNTS;

  procedure ENCODE_AMOUNTS is
  begin
    SCREEN.ENCODE_SUMMARY(REAL_AMOUNT, ACCOUNT_AMOUNT,
                          DEFERED_AMOUNT, MARGIN_AMOUNT);
  end ENCODE_AMOUNTS;

  -- Refresh all
  procedure REFRESH_SCREEN is
  begin
    SCREEN.ENCODE_FILE_NAME(TEXT_HANDLER.VALUE(ACCOUNT_NAME));
    SCREEN.ENCODE_NB_OPER(OPER_LIST_MNG.LIST_LENGTH(OPER_LIST));
    SCREEN.ENCODE_SAVED(ACCOUNT_SAVED);
    RESET_LIST;
    ENCODE_AMOUNTS;
    SCREEN.UPDATE_TO_UNIT;
    SCREEN.ALLOW_EDIT(not OPER_LIST_MNG.IS_EMPTY(OPER_LIST));
  end REFRESH_SCREEN;


  -- Modify the account
  procedure LOAD (FILE_NAME : in STRING) is
    OPER : OPER_DEF.OPER_REC;
  begin
    if not ACCOUNT_SAVED
    and then not SCREEN.CONFIRM_ACTION(SCREEN.OVERWRITE_ACCOUNT) then
      -- User discards overwritting current account
      return;
    end if;

    if FILE_NAME /= "" then
      -- Store file name
      begin
        TEXT_HANDLER.SET(ACCOUNT_NAME, FILE_NAME);
      exception
        when CONSTRAINT_ERROR =>
          SCREEN.ACK_ERROR(SCREEN.FILE_NAME_TOO_LONG);
          ACCOUNT_SAVED := FALSE;
          REFRESH_SCREEN;
          return;
      end;
    else
      -- Let user select file
      LOADING := TRUE;
      TEXT_HANDLER.SET(ACCOUNT_NAME, ACCOUNT_SELECT_FILE(2, "", TRUE));
      SCREEN.RESET;
      REFRESH_SCREEN;
    end if;

    -- In case of load error
    SCREEN.ENCODE_FILE_NAME(TEXT_HANDLER.VALUE(ACCOUNT_NAME));
    if not TEXT_HANDLER.EMPTY(ACCOUNT_NAME) then
      -- Load
      begin
        FILE_MNG.LOAD(TEXT_HANDLER.VALUE(ACCOUNT_NAME), OPER_LIST);
      exception
        when FILE_MNG.F_ACCESS_ERROR =>
          SCREEN.ACK_ERROR(SCREEN.FILE_ACCESS);
          CLEAR;
          return;
        when FILE_MNG.F_IO_ERROR =>
          SCREEN.ACK_ERROR(SCREEN.FILE_IO);
          CLEAR;
          return;
      end;
      -- Get root amount
      ROOT_AMOUNT := LIST_UTIL.GET_AMOUNT;
      -- Move to end
      if not OPER_LIST_MNG.IS_EMPTY(OPER_LIST) then
        OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.PREV, 0, FALSE);
      end if;
      -- Set data
      ACCOUNT_SAVED := TRUE;
      COMPUTE_AMOUNTS;
      -- Set screen
      REFRESH_SCREEN;
    else
      -- User cancelled selection
      CLEAR;
    end if;

  end LOAD;

  procedure SAVE (RESCUE : in BOOLEAN := FALSE) is
    TMP_NAME : TEXT_HANDLER.TEXT(DIRECTORY.MAX_DIR_NAME_LEN);
  begin
    if RESCUE then
      LIST_UTIL.INSERT_AMOUNT(ROOT_AMOUNT);
      FILE_MNG.SAVE("Tmp", OPER_LIST);
      ROOT_AMOUNT := LIST_UTIL.GET_AMOUNT;
      return;
    end if;
      
    -- Confirm file overwritting
    --  or select file
    LOADING := FALSE;
    if TEXT_HANDLER.EMPTY(ACCOUNT_NAME)
    or else not SCREEN.CONFIRM_ACTION(SCREEN.OVERWRITE_FILE) then
      TEXT_HANDLER.SET(TMP_NAME, ACCOUNT_SELECT_FILE(2, "", FALSE));
      SCREEN.RESET;
      REFRESH_SCREEN;
      if TEXT_HANDLER.EMPTY(TMP_NAME) then
        -- User discards
        return;
      end if;
      TEXT_HANDLER.SET(ACCOUNT_NAME, TMP_NAME);
      SCREEN.ENCODE_FILE_NAME(TEXT_HANDLER.VALUE(ACCOUNT_NAME));
    end if;
    -- Insert root amount
    LIST_UTIL.INSERT_AMOUNT(ROOT_AMOUNT);
    -- Save
    begin
      FILE_MNG.SAVE(TEXT_HANDLER.VALUE(ACCOUNT_NAME), OPER_LIST);
    exception
      when FILE_MNG.F_ACCESS_ERROR =>
        SCREEN.ACK_ERROR(SCREEN.FILE_ACCESS);
        ROOT_AMOUNT := LIST_UTIL.GET_AMOUNT;
        return;
      when FILE_MNG.F_IO_ERROR =>
        SCREEN.ACK_ERROR(SCREEN.FILE_IO);
        ROOT_AMOUNT := LIST_UTIL.GET_AMOUNT;
        return;
    end;
    ROOT_AMOUNT := LIST_UTIL.GET_AMOUNT;
    -- Update data and screen
    ACCOUNT_SAVED := TRUE;
    REFRESH_SCREEN;
  end SAVE;

  procedure CLEAR is
    OPER : OPER_DEF.OPER_REC;
  begin
    if not ACCOUNT_SAVED
    and then not SCREEN.CONFIRM_ACTION(SCREEN.OVERWRITE_ACCOUNT) then
      return;
    end if;
    -- Set data
    TEXT_HANDLER.EMPTY(ACCOUNT_NAME);
    OPER_LIST_MNG.DELETE_LIST(OPER_LIST);
    ROOT_AMOUNT := 0.0;
    ACCOUNT_SAVED := TRUE;
    COMPUTE_AMOUNTS;
    -- Set screen
    REFRESH_SCREEN;
  end CLEAR;

  -- Print account
  procedure PRINT is
    use TEXT_IO;
    PFN : constant STRING := "Printed.lpt";
    PF : FILE_TYPE;
    OPER : OPER_DEF.OPER_REC;
    SEP : constant CHARACTER := '|';
    INDEX : OPER_RANGE;
  begin
    begin
      CREATE(PF, OUT_FILE, PFN);
    exception
      when others =>
        SCREEN.ACK_ERROR(SCREEN.FILE_ACCESS);
        REFRESH_SCREEN;
        return;
    end;
    PUT_LINE(PF, "Account: " & TEXT_HANDLER.VALUE(ACCOUNT_NAME)
               & "     at: " & UNIT_FORMAT.DATE_IMAGE(OPER_DEF.CURRENT_DATE));
    if not OPER_LIST_MNG.IS_EMPTY(OPER_LIST) then
      LIST_UTIL.SAVE_POS;
      INDEX := 1;
      loop
        OPER_LIST_MNG.READ(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
        PUT_LINE(PF, "  " & NORMAL(INDEX, 4) & SEP
                   & UNIT_FORMAT.DATE_IMAGE(OPER.DATE) & SEP
                   & UNIT_FORMAT.IMAGE(OPER.AMOUNT) & SEP
                   & UNIT_FORMAT.SHORT_STATUS_IMAGE(OPER.STATUS) & SEP
                   & UNIT_FORMAT.SHORT_KIND_IMAGE(OPER.KIND) & SEP
                   & OPER.REFERENCE & SEP
                   & OPER.DESTINATION & SEP
                   & OPER.COMMENT);
        exit when OPER_LIST_MNG.GET_POSITION(OPER_LIST)
                = OPER_LIST_MNG.LIST_LENGTH(OPER_LIST);
        OPER_LIST_MNG.MOVE_TO(OPER_LIST);
        INDEX := INDEX + 1;
      end loop;
      LIST_UTIL.RESTORE_POS;
    end if;
    -- Print summary
    PUT_LINE(PF, "Real: "     &  UNIT_FORMAT.IMAGE(REAL_AMOUNT)
               & " Account: " &  UNIT_FORMAT.IMAGE(ACCOUNT_AMOUNT)
               & " Defered: " &  UNIT_FORMAT.IMAGE(DEFERED_AMOUNT)
               & " Margin: "  &  UNIT_FORMAT.IMAGE(MARGIN_AMOUNT));
    NEW_PAGE(PF);
    FLUSH(PF);
    
    -- Print
    declare
      DUMMY : INTEGER;
      SET, TRUNC : BOOLEAN;
      VAL : STRING(1 .. 256);
      LEN : NATURAL;
    begin
      SYS_CALLS.GETENV("ACCOUNT_LPR_COMMAND", SET, TRUNC, VAL, LEN);
      if not SET or else LEN = 0 then 
        LEN := 3;
        VAL(1 .. LEN) := "lpr";
      end if;
      DUMMY := SYS_CALLS.CALL_SYSTEM (VAL(1..LEN) & " " & PFN); 
    end;

    -- Delete & close
    DELETE(PF);

  exception
    when others =>
      SCREEN.ACK_ERROR(SCREEN.FILE_IO);
      REFRESH_SCREEN;
      return;
  end PRINT; 

  -- Update the displayed amounts
  procedure CHANGE_UNIT is
    use type UNIT_FORMAT.UNITS_LIST;
  begin
    if UNIT_FORMAT.GET_CURRENT_UNIT = UNIT_FORMAT.EUROS then
      UNIT_FORMAT.SET_UNIT_TO(UNIT_FORMAT.FRANCS);
    else
      UNIT_FORMAT.SET_UNIT_TO(UNIT_FORMAT.EUROS);
    end if;
    -- Redisplay
    REFRESH_SCREEN;
  end CHANGE_UNIT;

  -- Modify operations

  -- The generic edition of an operation
  package EDITION is
    type EDIT_LIST is (CREATE, COPY, MODIFY, VIEW, DELETE);
    procedure EDIT (EDIT_TYPE : in EDIT_LIST);
  end EDITION;
  package body EDITION is separate;

  -- Update status of operation
  procedure UPDATE_STATE is
    OPER : OPER_DEF.OPER_REC;
  begin
    OPER_LIST_MNG.READ(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
    case OPER.STATUS is
      when OPER_DEF.ENTERED =>
        OPER.STATUS := OPER_DEF.NOT_ENTERED;
      when OPER_DEF.NOT_ENTERED | OPER_DEF.DEFERED =>
        OPER.STATUS := OPER_DEF.ENTERED;
    end case;
    OPER_LIST_MNG.MODIFY(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
    COMPUTE_AMOUNTS;
    REFRESH_SCREEN;
  end UPDATE_STATE;

  -- Create a new operation
  procedure ADD_OPER is
  begin
    if not OPER_LIST_MNG.IS_EMPTY(OPER_LIST)
    and then SCREEN.CONFIRM_ACTION(SCREEN.ADD_COPY) then
      EDITION.EDIT(EDITION.COPY);
    else
      EDITION.EDIT(EDITION.CREATE);
    end if;
    SCREEN.RESET;
    COMPUTE_AMOUNTS;
    REFRESH_SCREEN;
  end ADD_OPER;

  -- Edit an operation
  procedure EDIT_OPER is
  begin
    EDITION.EDIT(EDITION.MODIFY);
    SCREEN.RESET;
    REFRESH_SCREEN;
  end EDIT_OPER;

  -- View an operation
  procedure VIEW_OPER is
  begin
    EDITION.EDIT(EDITION.VIEW);
    SCREEN.RESET;
    REFRESH_SCREEN;
  end VIEW_OPER;

  -- Delete an operation
  procedure DEL_OPER is
  begin
    EDITION.EDIT(EDITION.DELETE);
    SCREEN.RESET;
    REFRESH_SCREEN;
  end DEL_OPER;

  -- Remove all entered operations up to current
  -- Update root amount
  procedure GARBAGE_COLLECT is
    POS : POSITIVE;
    OPER : OPER_DEF.OPER_REC;
    use type OPER_DEF.STATUS_LIST, OPER_DEF.AMOUNT_RANGE;
  begin
    if OPER_LIST_MNG.IS_EMPTY(OPER_LIST) then
      return;
    end if;
    -- Get number of oper to check and start from the beginning
    POS := OPER_LIST_MNG.GET_POSITION(OPER_LIST);
    OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.NEXT, 0, FALSE);
    -- Check  up to pos included
    for I in 1 .. POS loop
      OPER_LIST_MNG.READ(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
      -- Remove if entered
      if OPER.STATUS = OPER_DEF.ENTERED then
        ROOT_AMOUNT := ROOT_AMOUNT + OPER.AMOUNT;
        if OPER_LIST_MNG.GET_POSITION(OPER_LIST) /= OPER_LIST_MNG.LIST_LENGTH(OPER_LIST) then
          -- Delete and move to next
          OPER_LIST_MNG.DELETE(OPER_LIST);
        else
          -- Delete and stop
          OPER_LIST_MNG.DELETE(OPER_LIST, OPER_LIST_MNG.PREV);
          exit;
        end if;
      else
        if I /= POS then
          -- Move to next
          OPER_LIST_MNG.MOVE_TO(OPER_LIST);
        end if;
      end if;
    end loop;
    -- Move to end
    if not OPER_LIST_MNG.IS_EMPTY(OPER_LIST) then
      OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.PREV, 0, FALSE);
    end if;
    ACCOUNT_SAVED := FALSE;
    COMPUTE_AMOUNTS;
    REFRESH_SCREEN;
  end GARBAGE_COLLECT;

  -- Make a sub selection of operations
  procedure SEARCH is
  begin
    -- @@@
    SCREEN.ACK_ERROR(SCREEN.NOT_IMPLEMENTED);
  end SEARCH;

  -- Reset selection to the full list
  procedure SHOW_ALL is
  begin
    -- @@@
    SCREEN.ACK_ERROR(SCREEN.NOT_IMPLEMENTED);
  end SHOW_ALL;

  -- Get data
  function IS_SAVED return BOOLEAN is
  begin
    return ACCOUNT_SAVED;
  end IS_SAVED;

end MNG;

