with DYNAMIC_LIST;
package PERS_DEF is

  -- Name and activity of a person
  subtype PERSON_NAME_STR is STRING (1 .. 20);
  subtype PERSON_ACTIVITY_STR is STRING (1 .. 10);

  -- Unique ID of a person.
  type PID_RANGE is new NATURAL range 000 .. 999;

  -- Heart rate. 0 in person time zones if not set
  type BPM_RANGE is new NATURAL range 0 .. 250;
  subtype SET_BPM_RANGE is BPM_RANGE range
   BPM_RANGE'SUCC(BPM_RANGE'FIRST) .. BPM_RANGE'LAST;

  -- 6 time zones for a person
  type PERSON_TZ_ARRAY is array (1 .. 6) of BPM_RANGE;

  -- A person
  type PERSON_REC is record
    NAME : PERSON_NAME_STR := (others => ' ');
    ACTIVITY : PERSON_ACTIVITY_STR := (others => ' ');
    PID : PID_RANGE := PID_RANGE'FIRST;
    TZ : PERSON_TZ_ARRAY := (others => BPM_RANGE'FIRST);
  end record;

  -- A list of person (all set)
  package PERSON_LIST_MNG is new DYNAMIC_LIST (ELEMENT_TYPE => PERSON_REC);
  subtype PERSON_LIST is PERSON_LIST_MNG.LIST_TYPE;
  THE_PERSONS : PERSON_LIST;

end PERS_DEF;

