with CALENDAR;

with AFPX;

with PERS_DEF;
with MESU_DEF;
with MESU_NAM;

package STR_MNG is

  -- Parse spaces from a string
  --  remove heading spaces
  --  remove multi spaces between words
  --  add spaces at the end
  procedure PARSE (STR : in out STRING);

  -- True if the string is only spaces
  function IS_SPACES (STR : STRING) return BOOLEAN;

  -- Has the str some spaces
  function HAS_SPACES (STR : STRING) return BOOLEAN;

  -- True if a parsed string has spaces in the middle
  function HAS_HOLES (STR : STRING) return BOOLEAN;

  subtype BPM_STR is STRING (1 .. 3);
  -- 0 <-> spaces
  -- others <-> value
  function TO_STR (BPM : PERS_DEF.BPM_RANGE) return BPM_STR;
  function TO_BPM (STR : BPM_STR) return PERS_DEF.BPM_RANGE;

  function PID_STR (PID : PERS_DEF.PID_RANGE) return MESU_NAM.FILE_PID_STR;


  subtype STR2 is STRING (1 .. 2);
  subtype STR4 is STRING (1 .. 4);

  type DATE_STR_REC is record
    DAY   : STR2 := (others => ' ');
    MONTH : STR2 := (others => ' ');
    YEAR  : STR4 := (others => ' ');
  end record;

  -- An input date can be before or after
  -- Check its validity and build date YYyyNnDd
  procedure CHECK_DATE (INPUT  : in DATE_STR_REC;
                        AFTER  : in BOOLEAN;
                        OUTPUT : out MESU_DEF.DATE_STR;
                        VALID  : out BOOLEAN);

  -- Build a rec
  procedure TO_REC (DATE : in MESU_DEF.DATE_STR;
                    REC  : out DATE_STR_REC);

  -- A printed date is Dd/Mm/YYyy
  subtype PRINTED_DATE_STR is STRING (1 .. 10);
  function TO_PRINTED_STR (DATE : MESU_DEF.DATE_STR) return PRINTED_DATE_STR;
  function TO_DATE_STR (PRINTED_DATE : PRINTED_DATE_STR)
  return MESU_DEF.DATE_STR;

  -- Current_date - nb month
  subtype OFFSET_RANGE is NATURAL range 0 .. CALENDAR.MONTH_NUMBER'LAST;

  function CURRENT_DATE (OFFSET : OFFSET_RANGE := 0) return MESU_DEF.DATE_STR;
  function CURRENT_DATE_PRINTED (OFFSET : OFFSET_RANGE := 0)
  return PRINTED_DATE_STR;
  procedure CURRENT_DATE_REC (DATE_REC : out DATE_STR_REC;
                              OFFSET   : in OFFSET_RANGE := 0);


  -- From a person rec to person in list
  procedure FORMAT_PERSON_TO_LIST (PERSON    : in PERS_DEF.PERSON_REC;
                                   LIST_PERS : out AFPX.LINE_REC);
  procedure FORMAT_LIST_TO_PERSON (LIST_PERS : in AFPX.LINE_REC;
                                   PERSON    : out PERS_DEF.PERSON_REC);

  -- From a mesure rec to person in list
  procedure FORMAT_MESURE_TO_LIST (PERSON    : in PERS_DEF.PERSON_REC;
                                   MESURE    : in MESU_DEF.MESURE_REC;
                                   MESU_NO   : in MESU_NAM.FILE_NO_STR;
                                   LIST_MESU : out AFPX.LINE_REC);
  procedure FORMAT_LIST_TO_MESURE (LIST_MESU : in AFPX.LINE_REC;
                                   FILE_NAME : out MESU_NAM.FILE_NAME_STR);

end STR_MNG;