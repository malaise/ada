with SEQUENTIAL_IO;
with DIR_MNG;
with PERS_MNG, STR_MNG, MESU_FIL;

-- Mesure selection management
package body MESU_SEL is
  use AFPX, AFPX.LINE_LIST_MNG;

  SAVED_LIST : AFPX.LINE_LIST_MNG.LIST_TYPE;

  LIST_FILE_NAME : constant STRING := "SELECTIO.LST";


  package LIST_IO is new SEQUENTIAL_IO (MESU_NAM.FILE_NAME_STR);
  LIST_FILE : LIST_IO.FILE_TYPE;

  procedure COPY_LIST (FROM, TO : in out AFPX.LINE_LIST_MNG.LIST_TYPE) is
    POS : POSITIVE;
    LINE : AFPX.LINE_REC;
  begin
    -- Delete dest list
    DELETE_LIST (TO);
    -- Done if list is empty
    if IS_EMPTY (FROM) then
      return;
    end if;
    -- Save pos, move to beginning
    POS := GET_POSITION (FROM);
    MOVE_TO (FROM, NEXT, 0, FALSE);
    -- Copy items
    begin
      loop
        READ (FROM, LINE);
        INSERT (TO, LINE);
      end loop;
    exception
      when NOT_IN_LIST =>
        -- Last item
        READ (FROM, LINE, CURRENT);
        INSERT (TO, LINE);
    end;
    -- Restore pos, set it in saved_list
    MOVE_TO (FROM, NEXT, POS - 1, FALSE);
    MOVE_TO (TO, NEXT, POS - 1, FALSE);
  end COPY_LIST;


  procedure SAVE_LIST is
  begin
    COPY_LIST (FROM => LINE_LIST, TO => SAVED_LIST);
  end SAVE_LIST;

  function DATE_MATCH (DATE, AFTER, BEFORE : MESU_DEF.DATE_STR)
  return BOOLEAN is
  begin
    if STR_MNG.IS_SPACES (AFTER) and then STR_MNG.IS_SPACES (BEFORE) then
      -- No criteria : date matches
      return TRUE;
    elsif STR_MNG.IS_SPACES (AFTER) then
      -- Only before : Date has to be < before
      return DATE < BEFORE;
    elsif STR_MNG.IS_SPACES (BEFORE) then
      -- Only after : date has to be >= after
      return DATE >= AFTER;
    elsif AFTER <= BEFORE then
      -- After <= Before : has to be after <= date < before
      return DATE >= AFTER and then DATE < BEFORE;
    else
      -- After > Before : has to be after >= date or  date < before
      return DATE >= AFTER or else DATE < BEFORE;
    end if;
  end DATE_MATCH;

  function SAME_FILE (L1, L2 : LINE_REC) return BOOLEAN is
    F1, F2 : MESU_NAM.FILE_NAME_STR;
  begin
    STR_MNG.FORMAT_LIST_TO_MESURE (L1, F1);
    STR_MNG.FORMAT_LIST_TO_MESURE (L2, F2);
    return F1 = F2;
  end SAME_FILE;

  function LESS_THAN (L1, L2 : LINE_REC) return BOOLEAN is
    F1, F2 : MESU_NAM.FILE_NAME_STR;
  begin
    STR_MNG.FORMAT_LIST_TO_MESURE (L1, F1);
    STR_MNG.FORMAT_LIST_TO_MESURE (L2, F2);
    return F1 < F2;
  end LESS_THAN;


  procedure FILE_SEARCH is new LINE_LIST_MNG.SEARCH (SAME_FILE);
  procedure FILE_SORT   is new LINE_LIST_MNG.SORT   (LESS_THAN);


  -- Add records to selection
  procedure ADD_SELECTION (CRITERIA : in CRITERIA_REC) is
    SAVED_POS : NATURAL;
    POS       : POSITIVE;
    FIRST_PERS, LAST_PERS : NATURAL;
    THE_FILES : DIR_MNG.FILE_LIST_MNG.LIST_TYPE;
    FILE : DIR_MNG.FILE_ENTRY_REC;
    PERSON : PERS_DEF.PERSON_REC;
    FILE_NAME : MESU_NAM.FILE_NAME_STR;
    DATE_S : MESU_NAM.FILE_DATE_STR;
    NO_S   : MESU_NAM.FILE_NO_STR;
    PID_S  : MESU_NAM.FILE_PID_STR;
    OK     : BOOLEAN;
    MESURE : MESU_DEF.MESURE_REC;
    LINE   : AFPX.LINE_REC;
  begin
    if PERS_DEF.PERSON_LIST_MNG.IS_EMPTY (PERS_DEF.THE_PERSONS) then
      return;
    end if;

    -- Save current position
    if IS_EMPTY (LINE_LIST) then
      SAVED_POS := 0;
    else
      SAVED_POS := GET_POSITION (LINE_LIST);
    end if;
    -- Save list
    SAVE_LIST;

    -- Set first and last person indexes
    if STR_MNG.IS_SPACES (CRITERIA.NAME) then
      -- No name => all persons
      FIRST_PERS := 1;
      LAST_PERS := PERS_DEF.PERSON_LIST_MNG.LIST_LENGTH (PERS_DEF.THE_PERSONS);
    elsif STR_MNG.IS_SPACES (CRITERIA.ACTIVITY) then
      -- Name no activity => select by name
      PERS_MNG.SELECT_BY_NAME (PERS_DEF.THE_PERSONS, CRITERIA.NAME,
                               FIRST_PERS, LAST_PERS);
    else
      -- Name and activity set => one person
      PERS_MNG.SEARCH (PERS_DEF.THE_PERSONS, CRITERIA.NAME, CRITERIA.ACTIVITY,
                       FIRST_PERS);
      LAST_PERS := FIRST_PERS;
    end if;

    -- For each person, list the files
    PERS_DEF.PERSON_LIST_MNG.MOVE_TO (PERS_DEF.THE_PERSONS,
                                      PERS_DEF.PERSON_LIST_MNG.NEXT,
                                      FIRST_PERS - 1, FALSE);
    for I in FIRST_PERS .. LAST_PERS loop
      -- Get person's pid
      PERS_DEF.PERSON_LIST_MNG.READ (PERS_DEF.THE_PERSONS, PERSON,
                                     PERS_DEF.PERSON_LIST_MNG.CURRENT);
      -- Build ????????.<pid>
      FILE_NAME := MESU_NAM.BUILD_FILE_NAME (PID => STR_MNG.PID_STR (PERSON.PID));
      -- Add to file list
      DIR_MNG.LIST_DIR (THE_FILES, "", FILE_NAME);

      -- Next person
      if I /= LAST_PERS then
        PERS_DEF.PERSON_LIST_MNG.MOVE_TO (PERS_DEF.THE_PERSONS);
      end if;
    end loop;

    if DIR_MNG.FILE_LIST_MNG.IS_EMPTY (THE_FILES) then
      -- No new file. Pos not affected and file list is empty.
      return;
    end if;


    -- Add files in line list
    DIR_MNG.FILE_LIST_MNG.MOVE_TO (THE_FILES, DIR_MNG.FILE_LIST_MNG.NEXT,
                                   0, FALSE);
    loop
      DIR_MNG.FILE_LIST_MNG.READ (THE_FILES, FILE,
                                  DIR_MNG.FILE_LIST_MNG.CURRENT);
      FILE_NAME := FILE.NAME (1 .. FILE.LEN);
      MESU_NAM.SPLIT_FILE_NAME (FILE_NAME, DATE_S, NO_S, PID_S);
      -- check date
     OK := DATE_MATCH (DATE_S, CRITERIA.DATE_AFT, CRITERIA.DATE_BEF);

      if OK then
        -- check pairs
        -- Get person & mesure to build afpx line rec
        PERS_MNG.SEARCH (PERS_DEF.THE_PERSONS,
                         PERS_DEF.PID_RANGE'VALUE(PID_S), FIRST_PERS);
        PERS_DEF.PERSON_LIST_MNG.READ (PERS_DEF.THE_PERSONS, PERSON,
                                       PERS_DEF.PERSON_LIST_MNG.CURRENT);
        MESURE := MESU_FIL.LOAD (FILE_NAME);
        STR_MNG.FORMAT_MESURE_TO_LIST (PERSON, MESURE, NO_S, LINE);

        if not IS_EMPTY(LINE_LIST) then
          POS := GET_POSITION (LINE_LIST);
          begin
            FILE_SEARCH (LINE_LIST, LINE, NEXT, 1, FALSE);
            -- Line already exists
            OK := FALSE;
          exception
            when NOT_IN_LIST =>
              -- Line is not in list
              OK := TRUE;
          end;
          MOVE_TO (LINE_LIST, NEXT, POS - 1, FALSE);
        end if;
      end if;

      -- Merge
      if OK then
        INSERT (LINE_LIST, LINE);
      end if;

      -- Next file
      exit when DIR_MNG.FILE_LIST_MNG.GET_POSITION (THE_FILES)
              = DIR_MNG.FILE_LIST_MNG.LIST_LENGTH  (THE_FILES);

      DIR_MNG.FILE_LIST_MNG.MOVE_TO (THE_FILES);
    end loop;

    -- sort by name activity date
    FILE_SORT (LINE_LIST);

    -- restore / set pos
    if SAVED_POS /= 0 then
      MOVE_TO (LINE_LIST, NEXT, SAVED_POS - 1, FALSE);
    elsif not IS_EMPTY (LINE_LIST) then
      -- List was empty, move to first
      MOVE_TO (LINE_LIST, NEXT, 0, FALSE);
    end if;

    -- Delete files list
    DIR_MNG.FILE_LIST_MNG.DELETE_LIST (THE_FILES);
  end ADD_SELECTION;

  -- Remove records from selection
  procedure REM_SELECTION (CRITERIA : in CRITERIA_REC) is
    SAVED_POS, CURR_POS : POSITIVE;
    LINE   : AFPX.LINE_REC;
    OK : BOOLEAN;
    FILE_NAME : MESU_NAM.FILE_NAME_STR;
    DATE_S : MESU_NAM.FILE_DATE_STR;
    NO_S   : MESU_NAM.FILE_NO_STR;
    PID_S  : MESU_NAM.FILE_PID_STR;
    POS_PERS : POSITIVE;
    PERSON : PERS_DEF.PERSON_REC;
  begin
    -- Save current position
    if IS_EMPTY (LINE_LIST) then
      return;
    else
      SAVED_POS := GET_POSITION (LINE_LIST);
    end if;
    -- Save list
    SAVE_LIST;

    -- for each in list
    MOVE_TO (LINE_LIST, NEXT, 0, FALSE);
    loop
      -- Get line, file_name, split
      READ (LINE_LIST, LINE, CURRENT);
      STR_MNG.FORMAT_LIST_TO_MESURE (LINE, FILE_NAME);
      MESU_NAM.SPLIT_FILE_NAME (FILE_NAME, DATE_S, NO_S, PID_S);

      OK := TRUE;
      if not STR_MNG.IS_SPACES (CRITERIA.NAME) then
        -- Person name set : Get person and check names
        PERS_MNG.SEARCH (PERS_DEF.THE_PERSONS, PERS_DEF.PID_RANGE'VALUE(PID_S),
                         POS_PERS);
        PERS_DEF.PERSON_LIST_MNG.READ (PERS_DEF.THE_PERSONS, PERSON,
                                       PERS_DEF.PERSON_LIST_MNG.CURRENT);
        OK := PERSON.NAME = CRITERIA.NAME;
        if OK and then not STR_MNG.IS_SPACES (CRITERIA.ACTIVITY) then
          -- Activity set : check activity
          OK := PERSON.ACTIVITY = CRITERIA.ACTIVITY;
        end if;
      end if;

      -- Check date
      if OK then
        OK := DATE_MATCH (DATE_S, CRITERIA.DATE_AFT, CRITERIA.DATE_BEF);
      end if;

      -- Delete line. Update saved pos if deleting initial current line
      if OK then
        CURR_POS := GET_POSITION (LINE_LIST);
        if CURR_POS /= LIST_LENGTH (LINE_LIST) then
          DELETE (LINE_LIST);
          if CURR_POS < SAVED_POS then
            SAVED_POS := SAVED_POS - 1;
          elsif CURR_POS = SAVED_POS then
            SAVED_POS := GET_POSITION (LINE_LIST);
          end if;
        else
          DELETE (LINE_LIST, PREV);
          if CURR_POS = SAVED_POS and then not IS_EMPTY (LINE_LIST) then
            SAVED_POS := GET_POSITION (LINE_LIST);
          end if;
          exit;
        end if;
      else
        -- Next line except if list empty or end of list
        exit when IS_EMPTY (LINE_LIST) or else
                  GET_POSITION (LINE_LIST) = LIST_LENGTH (LINE_LIST);
        MOVE_TO (LINE_LIST);
      end if;

    end loop;

    -- Restore pos
    if not IS_EMPTY (LINE_LIST) then
      MOVE_TO (LINE_LIST, NEXT, SAVED_POS - 1, FALSE);
    end if;

  end REM_SELECTION;

  -- Add a record to selection
  procedure ADD_SELECTION (NAME : in MESU_NAM.FILE_NAME_STR) is
    LINE   : AFPX.LINE_REC;
    DATE_S : MESU_NAM.FILE_DATE_STR;
    NO_S   : MESU_NAM.FILE_NO_STR;
    PID_S  : MESU_NAM.FILE_PID_STR;
    POS_PERS : POSITIVE;
    PERSON : PERS_DEF.PERSON_REC;
    MESURE : MESU_DEF.MESURE_REC;
  begin

    MESU_NAM.SPLIT_FILE_NAME (NAME, DATE_S, NO_S, PID_S);

    -- Get person and mesure
    PERS_MNG.SEARCH (PERS_DEF.THE_PERSONS,
                     PERS_DEF.PID_RANGE'VALUE(PID_S), POS_PERS);
    PERS_DEF.PERSON_LIST_MNG.READ (PERS_DEF.THE_PERSONS, PERSON,
                                   PERS_DEF.PERSON_LIST_MNG.CURRENT);
    MESURE := MESU_FIL.LOAD (NAME);

    -- Build line
    STR_MNG.FORMAT_MESURE_TO_LIST (PERSON, MESURE, NO_S, LINE);

    -- Insert
    INSERT (LINE_LIST, LINE);

    -- sort by name activity date
    FILE_SORT (LINE_LIST);

    -- Current set to inserted
    FILE_SEARCH (LINE_LIST, LINE, NEXT, 1, FALSE);
  end ADD_SELECTION;

  -- Remove a record from selection
  procedure REM_SELECTION (NAME : in MESU_NAM.FILE_NAME_STR) is
    SAVED_POS, CURR_POS : POSITIVE;
    LINE   : AFPX.LINE_REC;
    DATE_S : MESU_NAM.FILE_DATE_STR;
    NO_S   : MESU_NAM.FILE_NO_STR;
    PID_S  : MESU_NAM.FILE_PID_STR;
    POS_PERS : POSITIVE;
    PERSON : PERS_DEF.PERSON_REC;
    MESURE : MESU_DEF.MESURE_REC;
  begin
    -- Save current position
    if IS_EMPTY (LINE_LIST) then
      return;
    else
      SAVED_POS := GET_POSITION (LINE_LIST);
    end if;

    -- Save list
    SAVE_LIST;

    -- Split file name
    MESU_NAM.SPLIT_FILE_NAME (NAME, DATE_S, NO_S, PID_S);

    -- Get person and mesure
    PERS_MNG.SEARCH (PERS_DEF.THE_PERSONS,
                     PERS_DEF.PID_RANGE'VALUE(PID_S), POS_PERS);
    PERS_DEF.PERSON_LIST_MNG.READ (PERS_DEF.THE_PERSONS, PERSON,
                                   PERS_DEF.PERSON_LIST_MNG.CURRENT);
    MESURE := MESU_FIL.LOAD (NAME);

    -- Build line
    STR_MNG.FORMAT_MESURE_TO_LIST (PERSON, MESURE, NO_S, LINE);

    -- Search record
    FILE_SEARCH (LINE_LIST, LINE, NEXT, 1, FALSE);

    -- Delete line. Update saved pos if deleting initial current line
    CURR_POS := GET_POSITION (LINE_LIST);
    if CURR_POS /= LIST_LENGTH (LINE_LIST) then
      DELETE (LINE_LIST);
      if CURR_POS < SAVED_POS then
        SAVED_POS := SAVED_POS - 1;
      elsif CURR_POS = SAVED_POS then
        SAVED_POS := GET_POSITION (LINE_LIST);
      end if;
    else
      DELETE (LINE_LIST, PREV);
      if CURR_POS = SAVED_POS and then not IS_EMPTY (LINE_LIST) then
        SAVED_POS := GET_POSITION (LINE_LIST);
      end if;
    end if;
    if NOT IS_EMPTY (LINE_LIST) then
      MOVE_TO (LINE_LIST, NEXT, SAVED_POS - 1, FALSE);
    end if;

  end REM_SELECTION;

  -- Remove a record from selection
  procedure REM_SELECTION (LINE : in AFPX.LINE_REC) is
    SAVED_POS, CURR_POS : POSITIVE;
    PERSON : PERS_DEF.PERSON_REC;
    MESURE : MESU_DEF.MESURE_REC;
  begin
    -- Save current position
    if IS_EMPTY (LINE_LIST) then
      return;
    else
      SAVED_POS := GET_POSITION (LINE_LIST);
    end if;

    -- Save list
    SAVE_LIST;

    -- Search record
    FILE_SEARCH (LINE_LIST, LINE, NEXT, 1, FALSE);

    -- Delete line. Update saved pos if deleting initial current line
    CURR_POS := GET_POSITION (LINE_LIST);
    if CURR_POS /= LIST_LENGTH (LINE_LIST) then
      DELETE (LINE_LIST);
      if CURR_POS < SAVED_POS then
        SAVED_POS := SAVED_POS - 1;
      elsif CURR_POS = SAVED_POS then
        SAVED_POS := GET_POSITION (LINE_LIST);
      end if;
    else
      DELETE (LINE_LIST, PREV);
      if CURR_POS = SAVED_POS and then not IS_EMPTY (LINE_LIST) then
        SAVED_POS := GET_POSITION (LINE_LIST);
      end if;
    end if;
    if NOT IS_EMPTY (LINE_LIST) then
      MOVE_TO (LINE_LIST, NEXT, SAVED_POS - 1, FALSE);
    end if;

  end REM_SELECTION;


  procedure CLOSE is
  begin
    LIST_IO.CLOSE (LIST_FILE);
  exception
    when others => null;
  end CLOSE;

  -- Load the selection from file
  procedure LOAD is
    FILE_NAME : MESU_NAM.FILE_NAME_STR;
    use LIST_IO;
  begin
    DELETE_LIST (LINE_LIST);
    -- Open file
    begin
      OPEN (LIST_FILE, IN_FILE, LIST_FILE_NAME);
    exception
      when NAME_ERROR =>
        return;
    end;

    -- Read file
    while not END_OF_FILE (LIST_FILE) loop
      READ (LIST_FILE, FILE_NAME);
      ADD_SELECTION (FILE_NAME);
    end loop;

    CLOSE;

  end LOAD;



  -- Save the selection to file
  procedure SAVE is
    SAVED_POS  : POSITIVE;
    LINE   : AFPX.LINE_REC;
    FILE_NAME : MESU_NAM.FILE_NAME_STR;
    DONE : BOOLEAN;
    use LIST_IO;
  begin
    -- Delete previous file
    begin
      OPEN (LIST_FILE, IN_FILE, LIST_FILE_NAME);
      DELETE (LIST_FILE);
    exception
      when NAME_ERROR => null;
    end;

    -- Create file
    CREATE (LIST_FILE, OUT_FILE, LIST_FILE_NAME);

    -- Save current position
    if IS_EMPTY (LINE_LIST) then
      CLOSE;
      return;
    else
      SAVED_POS := GET_POSITION (LINE_LIST);
    end if;

    MOVE_TO (LINE_LIST, NEXT, 0, FALSE);
    -- Copy items
    loop
      begin
        READ (LINE_LIST, LINE);
        DONE := FALSE;
      exception
        when NOT_IN_LIST =>
          -- Last item
          READ (LINE_LIST, LINE, CURRENT);
          DONE := TRUE;
      end;
      STR_MNG.FORMAT_LIST_TO_MESURE (LINE, FILE_NAME);
      WRITE (LIST_FILE, FILE_NAME);

      exit when DONE;
    end loop;

    -- Restore pos, set it in saved_list
    MOVE_TO (LINE_LIST, NEXT, SAVED_POS - 1, FALSE);

    CLOSE;
  end SAVE;

  -- Undo (if possible) previous action on selection
  procedure UNDO is
  begin
    COPY_LIST (FROM => SAVED_LIST, TO => LINE_LIST);
  end UNDO;

end MESU_SEL;
