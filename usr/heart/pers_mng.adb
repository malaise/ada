with UPPER_STR;
with STR_MNG;
package body PERS_MNG is

  -- Search a person knowing its pid
  procedure SEARCH (LIST : in out PERS_DEF.PERSON_LIST;
                    PID  : in PERS_DEF.PID_RANGE;
                    POS  : out NATURAL) is
    LIST_LENGTH : constant NATURAL
                := PERS_DEF.PERSON_LIST_MNG.LIST_LENGTH (LIST);
    PERSON : PERS_DEF.PERSON_REC;
    LOC_POS : NATURAL;
    use PERS_DEF;

  begin
    POS := 0;
    if LIST_LENGTH = 0 then
      return;
    end if;

    -- Move to beginning
    PERS_DEF.PERSON_LIST_MNG.MOVE_TO (LIST, PERS_DEF.PERSON_LIST_MNG.NEXT,
     0, FALSE);
    -- Read persons, look for PID
    for I in 1 .. LIST_LENGTH loop
      if I /= LIST_LENGTH then
        PERS_DEF.PERSON_LIST_MNG.READ (LIST, PERSON,
         PERS_DEF.PERSON_LIST_MNG.NEXT);
      else
        PERS_DEF.PERSON_LIST_MNG.READ (LIST, PERSON,
         PERS_DEF.PERSON_LIST_MNG.CURRENT);
      end if;

      if PERSON.PID = PID then
        -- Return when PID found
        POS := I;
        LOC_POS := I;
        -- Move to this pos
        PERS_DEF.PERSON_LIST_MNG.MOVE_TO (LIST, PERS_DEF.PERSON_LIST_MNG.NEXT,
         LOC_POS - 1, FALSE);
        return;
      end if;
    end loop;

  end SEARCH;

  -- Search a person knowing its name and activity
  procedure SEARCH (LIST     : in out PERS_DEF.PERSON_LIST;
                    NAME     : in PERS_DEF.PERSON_NAME_STR;
                    ACTIVITY : in PERS_DEF.PERSON_ACTIVITY_STR;
                    POS      : out NATURAL) is
    LIST_LENGTH : constant NATURAL
                := PERS_DEF.PERSON_LIST_MNG.LIST_LENGTH (LIST);
    PERSON : PERS_DEF.PERSON_REC;
    LOC_POS : NATURAL;
  begin
    POS := 0;
    if LIST_LENGTH = 0 then
      return;
    end if;

    -- Move to beginning
    PERS_DEF.PERSON_LIST_MNG.MOVE_TO (LIST, PERS_DEF.PERSON_LIST_MNG.NEXT,
     0, FALSE);
    -- Read persons, look for (NAME, ACTIVITY)
    for I in 1 .. LIST_LENGTH loop
      if I /= LIST_LENGTH then
        PERS_DEF.PERSON_LIST_MNG.READ (LIST, PERSON,
         PERS_DEF.PERSON_LIST_MNG.NEXT);
      else
        PERS_DEF.PERSON_LIST_MNG.READ (LIST, PERSON,
         PERS_DEF.PERSON_LIST_MNG.CURRENT);
      end if;

      if PERSON.NAME = NAME and then PERSON.ACTIVITY = ACTIVITY then
        --Return when NAME and ACTIVITY found
        POS := I;
        LOC_POS := I;
        -- Move to this pos
        PERS_DEF.PERSON_LIST_MNG.MOVE_TO (LIST, PERS_DEF.PERSON_LIST_MNG.NEXT,
         LOC_POS - 1, FALSE);
        return;
      end if;
    end loop;


  end SEARCH;

  -- The EXPAND call affects the current position in list
  -- NAME must not be empty otherwise nothing is expanded
  -- ACTIVITY can be empty or partial
  -- If ACTIVITY is empty:
  --   If NAME allows only one and only one name expansion then
  --     it is expanded if necessary and POS is set to 0
  --   Else
  --     POS is set to -1 (not found or not one possible expansion)
  -- Else (ACTIVITY is set)
  --   If NAME and ACTIVITY allow one and only one expansion, then
  --     NAME is expanded if necessary and POS is set to its position
  --   Else
  --     POS is set to -1
  procedure EXPAND (LIST     : in out PERS_DEF.PERSON_LIST;
                    NAME     : in out PERS_DEF.PERSON_NAME_STR;
                    ACTIVITY : in out PERS_DEF.PERSON_ACTIVITY_STR;
                    POS      : out INTEGER) is
    EMPTY_NAME     : constant PERS_DEF.PERSON_NAME_STR     := (others => ' ');
    EMPTY_ACTIVITY : constant PERS_DEF.PERSON_ACTIVITY_STR := (others => ' ');
    FIRST_NAME, LAST_NAME, LEN_NAME : POSITIVE;
    FIRST_ACTIVITY, LAST_ACTIVITY, LEN_ACTIVITY : POSITIVE;
    LIST_LENGTH : constant NATURAL
                := PERS_DEF.PERSON_LIST_MNG.LIST_LENGTH (LIST);
    PERSON : PERS_DEF.PERSON_REC;
    MATCHING_PERSON : PERS_DEF.PERSON_REC;
    MATCH_ACTIVITY : constant BOOLEAN := ACTIVITY /= EMPTY_ACTIVITY;
    LOC_POS : INTEGER;
  begin
    POS := -1;
    STR_MNG.PARSE (NAME);
    NAME := UPPER_STR (NAME);
    STR_MNG.PARSE (ACTIVITY);
    ACTIVITY := UPPER_STR (ACTIVITY);

    -- No empty list!
    if LIST_LENGTH = 0 then
      return;
    end if;
    -- No empty name
    if NAME = EMPTY_NAME then
      return;
    end if;

    -- Locate end of NAME
    LAST_NAME := NAME'LAST;
    for I in reverse NAME'RANGE loop
      if NAME(I) /= ' ' then
        LAST_NAME := I;
        exit;
      end if;
    end loop;
    FIRST_NAME := NAME'FIRST;
    LEN_NAME := LAST_NAME - FIRST_NAME + 1;
    -- Locate end of ACTIVITY
    LAST_ACTIVITY := ACTIVITY'LAST;
    for I in reverse ACTIVITY'RANGE loop
      if ACTIVITY(I) /= ' ' then
        LAST_ACTIVITY := I;
        exit;
      end if;
    end loop;
    FIRST_ACTIVITY := ACTIVITY'FIRST;
    LEN_ACTIVITY := LAST_ACTIVITY - FIRST_ACTIVITY + 1;

   -- Scan the list
   LOC_POS := -1;
   MATCHING_PERSON.NAME := EMPTY_NAME;
   MATCHING_PERSON.ACTIVITY := EMPTY_ACTIVITY;
   PERS_DEF.PERSON_LIST_MNG.MOVE_TO (LIST, PERS_DEF.PERSON_LIST_MNG.NEXT,
         0, FALSE);
   for I in 1 .. LIST_LENGTH loop
      PERS_DEF.PERSON_LIST_MNG.READ (LIST, PERSON,
         PERS_DEF.PERSON_LIST_MNG.CURRENT);
      -- This person name matches
      if PERSON.NAME(1 .. LEN_NAME) = NAME(FIRST_NAME .. LAST_NAME) then
        -- This person name matches
        if not MATCH_ACTIVITY
        or else PERSON.ACTIVITY(1 .. LEN_ACTIVITY) =
                ACTIVITY(FIRST_ACTIVITY .. LAST_ACTIVITY) then
          -- This person matches
          if LOC_POS = -1 then
            -- First matching person
            MATCHING_PERSON.NAME := PERSON.NAME;
            if MATCH_ACTIVITY then
              -- Same name, same activity, may be ok if sole
              MATCHING_PERSON.ACTIVITY := PERSON.ACTIVITY;
              LOC_POS  := PERS_DEF.PERSON_LIST_MNG.GET_POSITION (LIST);
            else
              LOC_POS := 0;
            end if;
          else
            -- Not first matching : not sole
            LOC_POS := 0;
            -- Another matching person was found. Same name or activity
            if PERSON.NAME /= MATCHING_PERSON.NAME
            or else (MATCH_ACTIVITY
                     and then PERSON.ACTIVITY /= MATCHING_PERSON.ACTIVITY) then
              -- Several different names matching : abort
              LOC_POS := -1;
              return;
            end if;
          end if;
        end if;
      end if;
      if I /= LIST_LENGTH then
        PERS_DEF.PERSON_LIST_MNG.MOVE_TO (LIST);
      end if;
    end loop;

    if LOC_POS /= -1 then
      NAME := MATCHING_PERSON.NAME;
      if LOC_POS /= 0 then
        PERS_DEF.PERSON_LIST_MNG.MOVE_TO (LIST, PERS_DEF.PERSON_LIST_MNG.NEXT,
                                          LOC_POS - 1, FALSE);
        ACTIVITY := MATCHING_PERSON.ACTIVITY;
      end if;
      POS := LOC_POS;
    end if;
  end EXPAND;


  function ORDER_NAME (LEFT, RIGHT : PERS_DEF.PERSON_REC) return BOOLEAN is
  begin
    return LEFT.NAME < RIGHT.NAME;
  end ORDER_NAME;
  procedure SORT_NAME is new PERS_DEF.PERSON_LIST_MNG.SORT (ORDER_NAME);

  -- Get first and last index (in list) of persons with the provided name
  procedure SELECT_BY_NAME (LIST : in out PERS_DEF.PERSON_LIST;
                            NAME : in PERS_DEF.PERSON_NAME_STR;
                            FIRST, LAST : out NATURAL) is
    LIST_LENGTH : constant NATURAL
                := PERS_DEF.PERSON_LIST_MNG.LIST_LENGTH (LIST);
    PERSON : PERS_DEF.PERSON_REC;
    FOUND : BOOLEAN;
    LOC_POS : NATURAL;
    LOC_FIRST : NATURAL;
    LOC_LAST : NATURAL;
  begin
    LOC_FIRST := 0;
    LOC_LAST := 0;
    if LIST_LENGTH = 0 then
      FIRST := LOC_FIRST;
      LAST := LOC_LAST;
      return;
    end if;

    -- Sort list by names
    SORT_NAME (LIST);

    -- Look for first and last persons with matching name
    FOUND := FALSE;
    for I in 1 .. LIST_LENGTH loop
      if I /= LIST_LENGTH then
        PERS_DEF.PERSON_LIST_MNG.READ (LIST, PERSON,
         PERS_DEF.PERSON_LIST_MNG.NEXT);
      else
        PERS_DEF.PERSON_LIST_MNG.READ (LIST, PERSON,
         PERS_DEF.PERSON_LIST_MNG.CURRENT);
      end if;
      if not FOUND then
        if PERSON.NAME = NAME then
          LOC_FIRST := I;
          LOC_POS := I;
          FOUND := TRUE;
        end if;
      elsif PERSON.NAME /= NAME then
        LOC_LAST := I - 1;
        exit;
      end if;
    end loop;
    if FOUND then
      -- Last is last of list
      if LOC_LAST = 0 then
        LOC_LAST := LIST_LENGTH;
      end if;
      -- Move to first found
      PERS_DEF.PERSON_LIST_MNG.MOVE_TO (LIST, PERS_DEF.PERSON_LIST_MNG.NEXT,
       LOC_POS - 1, FALSE);
    end if;
    FIRST := LOC_FIRST;
    LAST := LOC_LAST;
  end SELECT_BY_NAME;


  function ORDER_ACTIVITY (LEFT, RIGHT : PERS_DEF.PERSON_REC) return BOOLEAN is
  begin
    return LEFT.ACTIVITY < RIGHT.ACTIVITY;
  end ORDER_ACTIVITY;
  procedure SORT_ACTIVITY is new PERS_DEF.PERSON_LIST_MNG.SORT (ORDER_ACTIVITY);

  -- Get first and last index (in list) of persons with the provided activity
  procedure SELECT_BY_ACTIVITY (LIST : in out PERS_DEF.PERSON_LIST;
                                ACTIVITY : in PERS_DEF.PERSON_ACTIVITY_STR;
                                FIRST, LAST : out NATURAL) is
    LIST_LENGTH : constant NATURAL
                := PERS_DEF.PERSON_LIST_MNG.LIST_LENGTH (LIST);
    PERSON : PERS_DEF.PERSON_REC;
    FOUND : BOOLEAN;
    LOC_POS : NATURAL;
  begin
    FIRST := 0;
    LAST := 0;
    if LIST_LENGTH = 0 then
      return;
    end if;

    -- Sort list by names
    SORT_NAME (LIST);

    -- Look for first and last persons with matching name
    FOUND := FALSE;
    for I in 1 .. LIST_LENGTH loop
      if I /= LIST_LENGTH then
        PERS_DEF.PERSON_LIST_MNG.READ (LIST, PERSON,
         PERS_DEF.PERSON_LIST_MNG.NEXT);
      else
        PERS_DEF.PERSON_LIST_MNG.READ (LIST, PERSON,
         PERS_DEF.PERSON_LIST_MNG.CURRENT);
      end if;
      if not FOUND then
        if PERSON.ACTIVITY = ACTIVITY then
          FIRST := I;
          LOC_POS := I;
          FOUND := TRUE;
        end if;
      elsif PERSON.ACTIVITY /= ACTIVITY then
        LAST := I - 1;
        -- Move to first found
        PERS_DEF.PERSON_LIST_MNG.MOVE_TO (LIST, PERS_DEF.PERSON_LIST_MNG.NEXT,
         LOC_POS - 1, FALSE);
        return;
      end if;
    end loop;


  end SELECT_BY_ACTIVITY;

  -- The INSERT call affects the order and current position in list
  -- Current pos in list becomes this person's one
  -- The person's pid is not significant ain the IN value,
  --  and set in the OUT value
  -- It may raise NOT_SOLE if the (NAME, ACTIVITY) already exists in list

  -- Insert a new person in the list. (Its NAME+ACTIVITY must be sole)
  procedure INSERT (LIST : in out PERS_DEF.PERSON_LIST;
                    PERSON : in out PERS_DEF.PERSON_REC) is
    LIST_LENGTH : constant NATURAL
                := PERS_DEF.PERSON_LIST_MNG.LIST_LENGTH (LIST);
    CURR_PERSON : PERS_DEF.PERSON_REC;
    PID_ARRAY : array (PERS_DEF.PID_RANGE) of BOOLEAN := (others => FALSE);
    FOUND : BOOLEAN;
  begin
    if LIST_LENGTH /= 0 then
      -- Search if this (NAME, ACTIVITY) is already in LIST
      -- Mark used PIDs
      PERS_DEF.PERSON_LIST_MNG.MOVE_TO (LIST,
       PERS_DEF.PERSON_LIST_MNG.NEXT, 0, FALSE);
      for I in 1 .. LIST_LENGTH loop
        if I /= LIST_LENGTH then
          PERS_DEF.PERSON_LIST_MNG.READ (LIST, CURR_PERSON,
           PERS_DEF.PERSON_LIST_MNG.NEXT);
        else
          PERS_DEF.PERSON_LIST_MNG.READ (LIST, CURR_PERSON,
           PERS_DEF.PERSON_LIST_MNG.CURRENT);
        end if;
        if CURR_PERSON.NAME = PERSON.NAME and then
           CURR_PERSON.ACTIVITY = PERSON.ACTIVITY then
          raise NOT_SOLE_ERROR;
        end if;
        PID_ARRAY (CURR_PERSON.PID) := TRUE;
      end loop;
      -- Look for first free PID
      FOUND := FALSE;
      for I in PERS_DEF.PID_RANGE loop
        if not PID_ARRAY(I) then
          PERSON.PID :=  I;
          FOUND := TRUE;
          exit;
        end if;
      end loop;
      if not FOUND then
        raise LIST_FULL_ERROR;
      end if;

    else
      PERSON.PID := PERS_DEF.PID_RANGE'FIRST;
    end if;

    -- Insert person in list
    PERS_DEF.PERSON_LIST_MNG.INSERT (LIST, PERSON);
  end INSERT;

end PERS_MNG;
