with NORMAL, PERPET, NORMAL;
package body STR_MNG is

  -- Is the str only spaces
  function IS_SPACES (STR : STRING) return BOOLEAN is
    SPACES : constant STRING (STR'RANGE) := (others => ' ');
  begin
    return STR = SPACES;
  end IS_SPACES;

  -- Has the str some spaces
  function HAS_SPACES (STR : STRING) return BOOLEAN is
  begin
    for I in STR'RANGE loop
      if STR(I) = ' ' then
        return TRUE;
      end if;
    end loop;
    return FALSE;
  end HAS_SPACES;

  -- Parse spaces from a string
  procedure PARSE (STR : in out STRING) is
   F, O : NATURAL;
   PREV_SPACE : BOOLEAN;
  begin
    if IS_SPACES (STR) then
      return;
    end if;

    O := STR'FIRST - 1;
    -- Skip heading spaces : F first significant char
    for I in STR'RANGE loop
      F := I;
      exit when STR(I) /= ' ';
    end loop;

    -- Skip multi spaces between words
    PREV_SPACE := FALSE;
    for I in F .. STR'LAST loop
      if STR(I) /= ' ' then
        PREV_SPACE := FALSE;
        O := O + 1;
        STR(O) := STR(I);
      else
        if not PREV_SPACE then
          O := O + 1;
          STR(O) := STR(I);
          PREV_SPACE := TRUE;
        end if;
      end if;
    end loop;
    -- O index the last significant char or last space
    if STR(O) = ' ' then
      O := O - 1;
    end if;
    -- Fill tail with spaces
    STR (O + 1 .. STR'LAST) := (others => ' ');
  end PARSE;

  -- True if a parsed string has spaces in the middle
  function HAS_HOLES (STR : STRING) return BOOLEAN is
    SPACE_FOUND : BOOLEAN;
  begin

    SPACE_FOUND := FALSE;
    for I in STR'RANGE loop
      if STR(I) = ' ' then
        SPACE_FOUND := TRUE;
      else
        if SPACE_FOUND then
          -- Not a space and a space found previously
          return TRUE;
        end if;
      end if;
    end loop;
    return FALSE;
  end HAS_HOLES;

  -- 0 <-> spaces
  -- others <-> value
  function TO_STR (BPM : PERS_DEF.BPM_RANGE) return BPM_STR is
    use PERS_DEF;
  begin
    if BPM = PERS_DEF.BPM_RANGE'FIRST then
      return BPM_STR'(others => ' ');
    else
      -- Align right
      return NORMAL(INTEGER(BPM), BPM_STR'LENGTH);
    end if;
  end To_STR;

  function TO_BPM (STR : BPM_STR) return PERS_DEF.BPM_RANGE is
    LOC_STR : BPM_STR := STR;
  begin
    if IS_SPACES (LOC_STR) then
      return 0;
    end if;
    PARSE (LOC_STR);
    return PERS_DEF.BPM_RANGE'VALUE(LOC_STR);
  end TO_BPM;

  function PID_STR (PID : PERS_DEF.PID_RANGE) return MESU_NAM.FILE_PID_STR is
  begin
    return NORMAL (INTEGER(PID), 3, GAP => '0');
  end PID_STR;



--  type DATE_STR_REC is record
--    DAY : STR2;
--    MONTH : STR2;
--    YEAR : STR4;
--  end record;
  -- An input date can be before or after
  -- Check its validity and build date YYyyNnDd
  procedure CHECK_DATE (INPUT  : in DATE_STR_REC;
                        AFTER  : in BOOLEAN;
                        OUTPUT : out MESU_DEF.DATE_STR;
                        VALID  : out BOOLEAN) is
    YEARS  : CALENDAR.YEAR_NUMBER;
    MONTHS : CALENDAR.MONTH_NUMBER;
    DAYS   : CALENDAR.DAY_NUMBER;
    CURRENT_TIME : constant CALENDAR.TIME := CALENDAR.CLOCK;
    DUMMY  : CALENDAR.TIME;
  begin
    -- No space or all spaces in each field
    if      (not IS_SPACES(INPUT.DAY)   and then HAS_SPACES(INPUT.DAY))
    or else (not IS_SPACES(INPUT.MONTH) and then HAS_SPACES(INPUT.MONTH))
    or else (not IS_SPACES(INPUT.YEAR)  and then HAS_SPACES(INPUT.YEAR)) then
      VALID := FALSE;
      return;
    end if;
    -- Parse
    if IS_SPACES (INPUT.YEAR) then
      -- No year => current year
      YEARS := CALENDAR.YEAR (CURRENT_TIME);
      if IS_SPACES (INPUT.MONTH) then
        -- No year no month => current month
        MONTHS := CALENDAR.MONTH(CURRENT_TIME);
      else
        -- Month set
        MONTHS := CALENDAR.MONTH_NUMBER'VALUE(INPUT.MONTH);
      end if;
      if IS_SPACES (INPUT.DAY) then
        if IS_SPACES (INPUT.MONTH) then
          -- ss ss ssss => current day
          DAYS := CALENDAR.DAY (CURRENT_TIME);
        else
          -- ss mm ssss => begin/end of month of current year
          if AFTER then
            DAYS := PERPET.NB_DAYS_MONTH (YEARS, MONTHS);
          else
            DAYS := CALENDAR.DAY_NUMBER'FIRST;
          end if;
        end if;
      else
        -- dd ss ssss  or  dd mm ssss
        DAYS := CALENDAR.DAY_NUMBER'VALUE(INPUT.DAY);
      end if;
    else
      -- Year is set
      YEARS := CALENDAR.YEAR_NUMBER'VALUE(INPUT.YEAR);
      if IS_SPACES (INPUT.MONTH) then
        -- dd ss yyyy forbidden
        if not IS_SPACES (INPUT.DAY) then
          VALID := FALSE;
          return;
        end if;
        -- Year and no month => first/last month
        if AFTER then
          MONTHS := CALENDAR.MONTH_NUMBER'LAST;
        else
          MONTHS := CALENDAR.MONTH_NUMBER'FIRST;
        end if;
      else
        -- Month set
        MONTHS := CALENDAR.MONTH_NUMBER'VALUE(INPUT.MONTH);
      end if;
      if IS_SPACES (INPUT.DAY) then
        -- First/last of date
        if AFTER then
          DAYS := PERPET.NB_DAYS_MONTH (YEARS, MONTHS);
        else
          DAYS := CALENDAR.DAY_NUMBER'FIRST;
        end if;
      else
        -- dd ss ssss  or  dd mm ssss
        DAYS := CALENDAR.DAY_NUMBER'VALUE(INPUT.DAY);
      end if;
    end if;
    -- Does all this stuff make a valid date?
    DUMMY := CALENDAR.TIME_OF (YEARS, MONTHS, DAYS);
    OUTPUT := NORMAL (YEARS,  4, GAP => '0')
            & NORMAL (MONTHS, 2, GAP => '0')
            & NORMAL (DAYS,   2, GAP => '0');
    VALID := TRUE;
  exception
    when others =>
      VALID := FALSE;
  end CHECK_DATE;

  -- Build a rec
  procedure TO_REC (DATE : in MESU_DEF.DATE_STR;
                    REC  : out DATE_STR_REC) is
  begin
    REC.YEAR  := DATE(1 .. 4);
    REC.MONTH := DATE(5 .. 6);
    REC.DAY   := DATE(7 .. 8);
  end TO_REC;

  -- A printed date is Dd/Mm/YYyy
  function TO_PRINTED_STR (DATE : MESU_DEF.DATE_STR) return PRINTED_DATE_STR is
    S : constant STRING(1 .. 10) := DATE(7 .. 8) & '/' & DATE(5 .. 6) & '/' & DATE(1 .. 4);
  begin
    return S;
  end TO_PRINTED_STR;

  function TO_DATE_STR (PRINTED_DATE : PRINTED_DATE_STR)
                        return MESU_DEF.DATE_STR is
    S : constant MESU_DEF.DATE_STR
      := PRINTED_DATE(7 .. 10) & PRINTED_DATE(4 .. 5) & PRINTED_DATE (1 .. 2);
  begin
    return S;
  end TO_DATE_STR;

  function CURRENT_DATE (OFFSET : OFFSET_RANGE := 0)
  return MESU_DEF.DATE_STR is
    CURRENT_TIME : CALENDAR.TIME := CALENDAR.CLOCK;
    YEARS  : CALENDAR.YEAR_NUMBER;
    MONTHS : CALENDAR.MONTH_NUMBER;
    DAYS   : CALENDAR.DAY_NUMBER;
    SECS   : CALENDAR.DAY_DURATION;
    use PERPET;
  begin
    if OFFSET /= 0 then
      CURRENT_TIME := CURRENT_TIME - (YEARS => 0, MONTHS => OFFSET);
    end if;
    CALENDAR.SPLIT (CURRENT_TIME, YEARS, MONTHS, DAYS, SECS);
    return NORMAL (YEARS,  4, GAP => '0')
         & NORMAL (MONTHS, 2, GAP => '0')
         & NORMAL (DAYS,   2, GAP => '0');
  end CURRENT_DATE;

  function CURRENT_DATE_PRINTED (OFFSET : OFFSET_RANGE := 0)
  return PRINTED_DATE_STR is
  begin
    return TO_PRINTED_STR (CURRENT_DATE(OFFSET));
  end CURRENT_DATE_PRINTED;

  procedure CURRENT_DATE_REC (DATE_REC : out DATE_STR_REC;
                              OFFSET : in OFFSET_RANGE := 0) is
    DATE : constant MESU_DEF.DATE_STR := CURRENT_DATE (OFFSET);
  begin
    DATE_REC.YEAR  := DATE(1 .. 4);
    DATE_REC.MONTH := DATE(5 .. 6);
    DATE_REC.DAY   := DATE(7 .. 8);
  end CURRENT_DATE_REC;

  -- From a person rec to person in list
  procedure FORMAT_PERSON_TO_LIST (PERSON    : in PERS_DEF.PERSON_REC;
                                   LIST_PERS : out AFPX.LINE_REC) is
  begin
    LIST_PERS.STR := (others => ' ');
    LIST_PERS.LEN := 32;
    LIST_PERS.STR (01 .. 20) := PERSON.NAME;
    LIST_PERS.STR (22 .. 31) := PERSON.ACTIVITY;
  end FORMAT_PERSON_TO_LIST;

  procedure FORMAT_LIST_TO_PERSON (LIST_PERS : in AFPX.LINE_REC;
                                   PERSON    : out PERS_DEF.PERSON_REC) is
  begin
    PERSON.NAME := LIST_PERS.STR (01 .. 20);
    PERSON.ACTIVITY := LIST_PERS.STR (23 .. 32);
  end FORMAT_LIST_TO_PERSON;


  -- From a mesure rec to person in list
  -- From a mesure rec to person in list
  procedure FORMAT_MESURE_TO_LIST (PERSON    : in PERS_DEF.PERSON_REC;
                                   MESURE    : in MESU_DEF.MESURE_REC;
                                   MESU_NO   : in MESU_NAM.FILE_NO_STR;
                                   LIST_MESU : out AFPX.LINE_REC) is
  begin
    FORMAT_PERSON_TO_LIST (PERSON, LIST_MESU);
    LIST_MESU.LEN := 71;
    LIST_MESU.STR(35 .. 44) := TO_PRINTED_STR (MESURE.DATE);
    LIST_MESU.STR(46 .. 65) := MESURE.COMMENT;
    LIST_MESU.STR(67 .. 69) := PID_STR(PERSON.PID);
    LIST_MESU.STR(70 .. 71) := MESU_NO;
  end FORMAT_MESURE_TO_LIST;

  procedure FORMAT_LIST_TO_MESURE (LIST_MESU : in AFPX.LINE_REC;
                                   FILE_NAME : out MESU_NAM.FILE_NAME_STR) is
    DATE : constant PRINTED_DATE_STR      := LIST_MESU.STR(35 .. 44);
    NO   : constant MESU_NAM.FILE_NO_STR  := LIST_MESU.STR(70 .. 71);
    PID  : constant MESU_NAM.FILE_PID_STR := LIST_MESU.STR(67 .. 69);

  begin
    FILE_NAME :=
     MESU_NAM.BUILD_FILE_NAME (DATE => TO_DATE_STR (DATE),
                               NO   => NO,
                               PID  => PID);
  end FORMAT_LIST_TO_MESURE;


end STR_MNG;
