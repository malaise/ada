with TEXT_IO;
with TEXT_HANDLER, DYNAMIC_LIST, DIRECTORY, AFPX, SELECT_FILE, NORMAL, SYS_CALLS;
with FILE_MNG, OPER_LIST_MNG, SCREEN, UNIT_FORMAT;

-- Manage the whole acount status
package body MNG is

  -- Sorted operations
  OPER_LIST : OPER_LIST_MNG.LIST_TYPE;
  procedure SORT is new OPER_LIST_MNG.SORT (OPER_DEF.BEFORE);

  -- Name and status of current account
  ACCOUNT_NAME : TEXT_HANDLER.TEXT(DIRECTORY.MAX_DIR_NAME_LEN);
  ACCOUNT_SAVED : BOOLEAN := TRUE;

  -- Are we working with sublist or all selection
  IN_SUBLIST : BOOLEAN := FALSE;

  -- The one in first record of file
  ROOT_AMOUNT : OPER_DEF.AMOUNT_RANGE;

  -- The ones computed
  REAL_AMOUNT, ACCOUNT_AMOUNT, DEFERED_AMOUNT,
               MARGIN_AMOUNT : OPER_DEF.AMOUNT_RANGE;

  -- Callback for selection when Loading/saving file
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

  -- Selection list
  type SEL_REC is record
    NO : OPER_RANGE;
    DELETED : BOOLEAN := FALSE;
  end record;
  package SEL_LIST_MNG is new DYNAMIC_LIST(SEL_REC);
  SEL_LIST : SEL_LIST_MNG.LIST_TYPE;

  -- Set current in sel list from AFPX selected
  procedure SET_CURRENT (NO : in OPER_NB_RANGE) is
  begin
    if NO in OPER_RANGE then
      SEL_LIST_MNG.MOVE_TO(SEL_LIST, SEL_LIST_MNG.NEXT, NO - 1, FALSE);
    end if;
  end SET_CURRENT;
      

  -- Builds the AFPX line from oper
  function OPER_TO_LINE (NO : OPER_RANGE;
                         OPER : OPER_DEF.OPER_REC) return AFPX.LINE_REC is
    LINE : AFPX.LINE_REC;
    SEP : constant CHARACTER := '|';
  begin
    LINE.LEN := AFPX.GET_FIELD_WIDTH(8);
    LINE.STR(1 .. 71) :=
                NORMAL(NO, 4) & SEP
              & UNIT_FORMAT.SHORT_DATE_IMAGE(OPER.DATE) & SEP
              & UNIT_FORMAT.SHORT_IMAGE(OPER.AMOUNT) & SEP
              & ' ' & UNIT_FORMAT.SHORT_STATUS_IMAGE(OPER.STATUS) & SEP
              & UNIT_FORMAT.SHORT_KIND_IMAGE(OPER.KIND) & SEP
              & OPER.DESTINATION(1 .. 10) & SEP
              & OPER.COMMENT(1 .. 15) & SEP
              & OPER.REFERENCE;
    return LINE;
  end OPER_TO_LINE;

  package LIST_UTIL is
    -- Build initial selection with all opers
    procedure RESET_SELECTION;

    -- Move in oper list to currently selected in sel list
    procedure MOVE_TO_CURRENT;

    -- These work only if lists are not modified between calls
    procedure SAVE_POS (MOVE_TO_FIRST : in BOOLEAN := TRUE);
    procedure RESTORE_POS;

    -- Don't use selection list between insert and get
    procedure INSERT_AMOUNT (AMOUNT : in OPER_DEF.AMOUNT_RANGE);
    function GET_AMOUNT return OPER_DEF.AMOUNT_RANGE;
  end LIST_UTIL;

  package body LIST_UTIL is

    procedure RESET_SELECTION is
    begin
      SEL_LIST_MNG.DELETE_LIST(SEL_LIST, DEALLOCATE => FALSE);
      for I in 1 .. OPER_LIST_MNG.LIST_LENGTH(OPER_LIST) loop
        SEL_LIST_MNG.INSERT(SEL_LIST, (NO => I, DELETED => FALSE));
      end loop;
      IN_SUBLIST := FALSE;
      SCREEN.SUBLIST(IN_SUBLIST);
    end RESET_SELECTION;


    procedure MOVE_TO_CURRENT is
      SEL : SEL_REC;
    begin
      SEL_LIST_MNG.READ(SEL_LIST, SEL, SEL_LIST_MNG.CURRENT);
      OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.NEXT, SEL.NO - 1, FALSE);
    end MOVE_TO_CURRENT;


    LOC_POS : OPER_RANGE;

    procedure SAVE_POS (MOVE_TO_FIRST : in BOOLEAN := TRUE) is
    begin
      LOC_POS := SEL_LIST_MNG.GET_POSITION(SEL_LIST);
      if MOVE_TO_FIRST then
        SEL_LIST_MNG.MOVE_TO(SEL_LIST, SEL_LIST_MNG.NEXT, 0, FALSE);
      end if;
    end SAVE_POS;

    procedure RESTORE_POS is
    begin
      SEL_LIST_MNG.MOVE_TO(SEL_LIST, SEL_LIST_MNG.NEXT, LOC_POS-1, FALSE);
    end RESTORE_POS;


    procedure INSERT_AMOUNT (AMOUNT : in OPER_DEF.AMOUNT_RANGE) is
      OPER : OPER_DEF.OPER_REC;
    begin
      OPER.AMOUNT := AMOUNT;
      if not OPER_LIST_MNG.IS_EMPTY(OPER_LIST) then
        OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.NEXT, 0, FALSE);
      end if;
      OPER_LIST_MNG.INSERT(OPER_LIST, OPER, OPER_LIST_MNG.PREV);
    end INSERT_AMOUNT;

    function GET_AMOUNT return OPER_DEF.AMOUNT_RANGE is
      OPER : OPER_DEF.OPER_REC;
    begin
      OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.NEXT, 0, FALSE);
      OPER_LIST_MNG.GET(OPER_LIST, OPER, OPER_LIST_MNG.NEXT);
      return OPER.AMOUNT;
    end GET_AMOUNT;
      
  end LIST_UTIL;
      

  -- Reset the AFPX list from the sel list
  procedure RESET_LIST is
    OPER : OPER_DEF.OPER_REC;
  begin
    AFPX.LINE_LIST_MNG.DELETE_LIST(AFPX.LINE_LIST);
    if SEL_LIST_MNG.IS_EMPTY(SEL_LIST) then
      return;
    end if;

    -- Save pos and move to beginning of selection
    LIST_UTIL.SAVE_POS;
    loop
      LIST_UTIL.MOVE_TO_CURRENT;
      OPER_LIST_MNG.READ(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
      AFPX.LINE_LIST_MNG.INSERT(AFPX.LINE_LIST,
                  OPER_TO_LINE(OPER_LIST_MNG.GET_POSITION(OPER_LIST),
                  OPER));
      exit when SEL_LIST_MNG.GET_POSITION(SEL_LIST)
              = SEL_LIST_MNG.LIST_LENGTH(SEL_LIST);
      SEL_LIST_MNG.MOVE_TO(SEL_LIST);
    end loop;
    LIST_UTIL.RESTORE_POS;
    AFPX.LINE_LIST_MNG.MOVE_TO(AFPX.LINE_LIST,
                     AFPX.LINE_LIST_MNG.NEXT,
                     SEL_LIST_MNG.GET_POSITION(SEL_LIST) - 1, FALSE);
  end RESET_LIST;

  -- Compute amounts from all account operations
  procedure COMPUTE_AMOUNTS is
    OPER : OPER_DEF.OPER_REC;
    use type OPER_DEF.AMOUNT_RANGE;
  begin
    -- Initial values
    REAL_AMOUNT := ROOT_AMOUNT;
    ACCOUNT_AMOUNT := ROOT_AMOUNT;
    DEFERED_AMOUNT := 0.0;
    MARGIN_AMOUNT  := ROOT_AMOUNT;
    if OPER_LIST_MNG.IS_EMPTY(OPER_LIST) then
      return;
    end if;

    -- All operations
    OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.NEXT, 0, FALSE);
    loop
      OPER_LIST_MNG.READ(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
      REAL_AMOUNT := REAL_AMOUNT + OPER.AMOUNT;
      case OPER.STATUS is
        when OPER_DEF.ENTERED =>
          ACCOUNT_AMOUNT := ACCOUNT_AMOUNT + OPER.AMOUNT;
          MARGIN_AMOUNT := MARGIN_AMOUNT + OPER.AMOUNT;
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
  end COMPUTE_AMOUNTS;

  -- Ecode amounts values
  procedure ENCODE_AMOUNTS is
  begin
    SCREEN.ENCODE_SUMMARY(REAL_AMOUNT, ACCOUNT_AMOUNT,
                          DEFERED_AMOUNT, MARGIN_AMOUNT);
  end ENCODE_AMOUNTS;

  -- Refresh all, to be called each time an oper or the account changes
  type LIST_UPDATE_LIST is (BOTTOM, CENTER, UNCHANGED);
  procedure REFRESH_SCREEN (LIST_UPDATE : in LIST_UPDATE_LIST) is
  begin
    SCREEN.ENCODE_FILE_NAME(TEXT_HANDLER.VALUE(ACCOUNT_NAME));
    SCREEN.ENCODE_NB_OPER(OPER_LIST_MNG.LIST_LENGTH(OPER_LIST),
                          SEL_LIST_MNG.LIST_LENGTH(SEL_LIST));
    SCREEN.ENCODE_SAVED(ACCOUNT_SAVED);
    RESET_LIST;
    if LIST_UPDATE = BOTTOM then
      AFPX.UPDATE_LIST(AFPX.BOTTOM);
    elsif LIST_UPDATE = CENTER then
      AFPX.UPDATE_LIST(AFPX.CENTER);
    end if;
    ENCODE_AMOUNTS;
    SCREEN.UPDATE_TO_UNIT;
    SCREEN.ALLOW_EDIT(not SEL_LIST_MNG.IS_EMPTY(SEL_LIST));
  end REFRESH_SCREEN;


  -- Load from file
  procedure LOAD (FILE_NAME : in STRING) is
    OPER : OPER_DEF.OPER_REC;
    CAN_WRITE : BOOLEAN;
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
          REFRESH_SCREEN(UNCHANGED);
          return;
      end;
    else
      -- Let user select file
      LOADING := TRUE;
      TEXT_HANDLER.SET(ACCOUNT_NAME, ACCOUNT_SELECT_FILE(2, "", TRUE));
      SCREEN.RESET;
      REFRESH_SCREEN(BOTTOM);
    end if;

    -- In case of load error
    SCREEN.ENCODE_FILE_NAME(TEXT_HANDLER.VALUE(ACCOUNT_NAME));
    -- If error occures afeter this point we clear the account
    if not TEXT_HANDLER.EMPTY(ACCOUNT_NAME) then
      -- Load
      begin
        FILE_MNG.LOAD(TEXT_HANDLER.VALUE(ACCOUNT_NAME), OPER_LIST, CAN_WRITE);
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
      -- Build initial selection with all
      SORT(OPER_LIST);
      LIST_UTIL.RESET_SELECTION;
      -- Set data
      ACCOUNT_SAVED := TRUE;
      COMPUTE_AMOUNTS;
      -- Set screen
      REFRESH_SCREEN(BOTTOM);
      if not CAN_WRITE then
        SCREEN.ACK_ERROR(SCREEN.FILE_READ_ONLY);
      end if;
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
      REFRESH_SCREEN(BOTTOM);
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
    REFRESH_SCREEN(CENTER);
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
    SEL_LIST_MNG.DELETE_LIST(SEL_LIST, DEALLOCATE => FALSE);
    OPER_LIST_MNG.DELETE_LIST(OPER_LIST);
    ROOT_AMOUNT := 0.0;
    ACCOUNT_SAVED := TRUE;
    COMPUTE_AMOUNTS;
    -- Set screen
    IN_SUBLIST := FALSE;
    SCREEN.SUBLIST(IN_SUBLIST);
    REFRESH_SCREEN(BOTTOM);
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
        REFRESH_SCREEN(CENTER);
        return;
    end;
    PUT_LINE(PF, "Account: " & TEXT_HANDLER.VALUE(ACCOUNT_NAME)
               & "     at: " & UNIT_FORMAT.DATE_IMAGE(OPER_DEF.CURRENT_DATE));
    --            --1234 123456789  123456789012 1234 1234 12345678901234567890 12345678901234567890 1234567890
    PUT_LINE(PF, "    No|   Date   |   Amount   |Stat|Kind|Destination         |Comment             |Reference");

    if not OPER_LIST_MNG.IS_EMPTY(OPER_LIST) then
      OPER_LIST_MNG.MOVE_TO(OPER_LIST, OPER_LIST_MNG.NEXT, 0, FALSE);
      INDEX := 1;
      loop
        OPER_LIST_MNG.READ(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
        PUT_LINE(PF, "  " & NORMAL(INDEX, 4) & SEP
                   & UNIT_FORMAT.DATE_IMAGE(OPER.DATE) & SEP
                   & UNIT_FORMAT.IMAGE(OPER.AMOUNT, FALSE) & SEP
                   & ' ' & UNIT_FORMAT.SHORT_STATUS_IMAGE(OPER.STATUS) & SEP
                   & UNIT_FORMAT.SHORT_KIND_IMAGE(OPER.KIND) & SEP
                   & OPER.DESTINATION & SEP
                   & OPER.COMMENT & SEP
                   & OPER.REFERENCE);
        exit when OPER_LIST_MNG.GET_POSITION(OPER_LIST)
                = OPER_LIST_MNG.LIST_LENGTH(OPER_LIST);
        OPER_LIST_MNG.MOVE_TO(OPER_LIST);
        INDEX := INDEX + 1;
      end loop;
    end if;
    -- Print summary
    PUT_LINE(PF, "Real: "     &  UNIT_FORMAT.IMAGE(REAL_AMOUNT, FALSE)
               & " Account: " &  UNIT_FORMAT.IMAGE(ACCOUNT_AMOUNT, FALSE)
               & " Defered: " &  UNIT_FORMAT.IMAGE(DEFERED_AMOUNT, FALSE)
               & " Margin: "  &  UNIT_FORMAT.IMAGE(MARGIN_AMOUNT, FALSE));
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
      REFRESH_SCREEN(CENTER);
      return;
  end PRINT; 

  -- Update the displayed amounts
  procedure CHANGE_UNIT is
    use type UNIT_FORMAT.UNITS_LIST;
  begin
    UNIT_FORMAT.SWITCH_UNIT;
    -- Redisplay
    REFRESH_SCREEN(CENTER);
  end CHANGE_UNIT;

  -- Sort
  procedure SORT is
  begin
    SORT(OPER_LIST); 
    LIST_UTIL.RESET_SELECTION;
    REFRESH_SCREEN(BOTTOM);
  end SORT;

  -- Deletion management
  package DELETION is
    -- Flag currently selected operation as deleted or not
    procedure FLAG_DELETED;
    procedure FLAG_UNDELETED;

    -- Get number of flagged operations
    function GET_NB_DELETED return OPER_NB_RANGE;

    -- Delete all flagged operation
    procedure COMMIT_DELETIONS;
  end DELETION;
  package body DELETION is separate;

  -- The generic edition of an operation
  package EDITION is
    type EDIT_LIST is (CREATE, MODIFY, VIEW, DELETE);
    procedure EDIT (EDIT_TYPE : in EDIT_LIST);
  end EDITION;
  package body EDITION is separate;

  -- Update status of operation
  procedure UPDATE_STATE is
    OPER : OPER_DEF.OPER_REC;
  begin
    LIST_UTIL.MOVE_TO_CURRENT;
    OPER_LIST_MNG.READ(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
    case OPER.STATUS is
      when OPER_DEF.ENTERED =>
        if OPER_DEF.KIND_CAN_BE_DEFERED(OPER.KIND) then
          OPER.STATUS := OPER_DEF.DEFERED;
        else
          OPER.STATUS := OPER_DEF.NOT_ENTERED;
        end if;
      when OPER_DEF.NOT_ENTERED | OPER_DEF.DEFERED =>
        OPER.STATUS := OPER_DEF.ENTERED;
    end case;
    OPER_LIST_MNG.MODIFY(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
    ACCOUNT_SAVED := FALSE;
    COMPUTE_AMOUNTS;
    REFRESH_SCREEN(UNCHANGED);
  end UPDATE_STATE;

  -- Create a new operation
  procedure ADD_OPER is
  begin
    EDITION.EDIT(EDITION.CREATE);
    SCREEN.RESET;
    COMPUTE_AMOUNTS;
    REFRESH_SCREEN(BOTTOM);
  end ADD_OPER;

  -- Edit an operation
  procedure EDIT_OPER is
  begin
    EDITION.EDIT(EDITION.MODIFY);
    SCREEN.RESET;
    COMPUTE_AMOUNTS;
    REFRESH_SCREEN(CENTER);
  end EDIT_OPER;

  -- View an operation
  procedure VIEW_OPER is
  begin
    EDITION.EDIT(EDITION.VIEW);
    SCREEN.RESET;
    REFRESH_SCREEN(CENTER);
  end VIEW_OPER;

  -- Delete an operation
  procedure DEL_OPER is
  begin
    EDITION.EDIT(EDITION.DELETE);
    SCREEN.RESET;
    COMPUTE_AMOUNTS;
    REFRESH_SCREEN(CENTER);
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
    POS := SEL_LIST_MNG.GET_POSITION(SEL_LIST);
    SEL_LIST_MNG.MOVE_TO(SEL_LIST, SEL_LIST_MNG.NEXT, 0, FALSE);
    -- Check up to pos included
    for I in 1 .. POS loop
      LIST_UTIL.MOVE_TO_CURRENT;
      OPER_LIST_MNG.READ(OPER_LIST, OPER, OPER_LIST_MNG.CURRENT);
      -- Remove if entered
      if OPER.STATUS = OPER_DEF.ENTERED then
        ROOT_AMOUNT := ROOT_AMOUNT + OPER.AMOUNT;
        DELETION.FLAG_DELETED;
        ACCOUNT_SAVED := FALSE;
      end if;
      -- Done when orig pos is processed
      exit when I = POS;
      -- Move to next
      SEL_LIST_MNG.MOVE_TO(SEL_LIST);
    end loop;
    DELETION.COMMIT_DELETIONS;
    COMPUTE_AMOUNTS;
    REFRESH_SCREEN(CENTER);
  end GARBAGE_COLLECT;

  -- Make a sub selection of operations
  procedure SEARCH is separate;

  -- Reset selection to the full list
  procedure SHOW_ALL is
  begin
    IN_SUBLIST := FALSE;
    SCREEN.SUBLIST(IN_SUBLIST);
    LIST_UTIL.RESET_SELECTION;
    REFRESH_SCREEN(BOTTOM);
  end SHOW_ALL;

  -- Get data
  function IS_SAVED return BOOLEAN is
  begin
    return ACCOUNT_SAVED;
  end IS_SAVED;

end MNG;

