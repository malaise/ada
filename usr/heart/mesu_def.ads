with PERS_DEF;
package MESU_DEF is

  -- Date in string YyyyMmDd
  subtype DATE_STR is STRING (1 .. 8);

  -- Delta in seconds between 2 samplings
  type SAMPLING_DELTA_RANGE is new POSITIVE range 1 .. 120;

  -- Comment for a mesure
  subtype COMMENT_STR is STRING (1 .. 20);

  -- Number of samplings
  subtype SAMPLE_NB_RANGE is POSITIVE range 1 .. 100;

  -- Values
  type SAMPLE_ARRAY is array (SAMPLE_NB_RANGE range <>) of
   PERS_DEF.BPM_RANGE;
  subtype MAX_SAMPLE_ARRAY is SAMPLE_ARRAY (SAMPLE_NB_RANGE);

  -- A mesure
  type MESURE_REC is record
    PID : PERS_DEF.PID_RANGE;
    -- YYYYMMDD
    DATE : DATE_STR := (others => ' ');
    SAMPLING_DELTA : SAMPLING_DELTA_RANGE := 120;
    COMMENT : COMMENT_STR := (others => ' ');
    -- Time zones for the mesure
    TZ : PERS_DEF.PERSON_TZ_ARRAY := (others => PERS_DEF.BPM_RANGE'FIRST);
    SAMPLES : MAX_SAMPLE_ARRAY := (others => PERS_DEF.BPM_RANGE'FIRST);
  end record;

end MESU_DEF;
