with PERS_DEF;
package PERS_MNG is

  -- All these calls make no assumption about the state of the list

  -- The SEARCH calls affect the current position in list (move to this
  --  person if found, random otherwise)
  -- They return 0 if the person is not found

  -- Search a person knowing its pid
  procedure SEARCH (LIST : in out PERS_DEF.PERSON_LIST;
                    PID  : in PERS_DEF.PID_RANGE;
                    POS  : out NATURAL);

  -- Search a person knowing its name and activity
  procedure SEARCH (LIST     : in out PERS_DEF.PERSON_LIST;
                    NAME     : in PERS_DEF.PERSON_NAME_STR;
                    ACTIVITY : in PERS_DEF.PERSON_ACTIVITY_STR;
                    POS      : out NATURAL);

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
                    POS      : out INTEGER);

  -- The SELECT calls affect the order and current position in list
  --  (position set to first if found)
  -- They return 0, if no person found

  -- Get first and last index (in list) of persons with the provided name
  procedure SELECT_BY_NAME (LIST : in out PERS_DEF.PERSON_LIST;
                            NAME : in PERS_DEF.PERSON_NAME_STR;
                            FIRST, LAST : out NATURAL);

  -- Get first and last index (in list) of persons with the provided activity
  procedure SELECT_BY_ACTIVITY (LIST : in out PERS_DEF.PERSON_LIST;
                                ACTIVITY : in PERS_DEF.PERSON_ACTIVITY_STR;
                                FIRST, LAST : out NATURAL);

  -- The INSERT call affects the order and current position in list
  -- Current pos in list becomes this person's one
  -- The person's pid is not significant in the IN value,
  --  and set in the OUT value
  -- It may raise NOT_SOLE_ERROR if the (NAME, ACTIVITY) already exists in list
  --  or raise LIST_FULL_ERROR if no more PID available

  -- Insert a new person in the list. (Its NAME+ACTIVITY must be sole)
  procedure INSERT (LIST : in out PERS_DEF.PERSON_LIST;
                    PERSON : in out PERS_DEF.PERSON_REC);

  NOT_SOLE_ERROR : exception;
  LIST_FULL_ERROR : exception;

end PERS_MNG;