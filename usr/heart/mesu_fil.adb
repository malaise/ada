with DIRECT_IO;
with NORMAL, TEXT_HANDLER;
with PERS_DEF;
package body MESU_FIL is


  -- A mesure in file (same as in definition but without pid)
  type FILE_REC is record
    SAMPLING_DELTA : MESU_DEF.SAMPLING_DELTA_RANGE := 60;
    COMMENT : MESU_DEF.COMMENT_STR := (others => ' ');
    -- Time zones for the mesure
    TZ : PERS_DEF.PERSON_TZ_ARRAY := (others => PERS_DEF.BPM_RANGE'FIRST);
    SAMPLES : MESU_DEF.MAX_SAMPLE_ARRAY
            := (others => PERS_DEF.BPM_RANGE'FIRST);
  end record;


  -- Direct_io of mesures
  package MESURE_IO is new DIRECT_IO (ELEMENT_TYPE => FILE_REC);
  MESURE_FILE : MESURE_IO.FILE_TYPE;

  procedure OPEN (FILE_NAME : in STRING; CREATE : BOOLEAN := TRUE) is
  begin
    -- Try to open existing file
    begin
      MESURE_IO.OPEN (MESURE_FILE, MESURE_IO.INOUT_FILE, FILE_NAME);
    exception
      when MESURE_IO.NAME_ERROR =>
        if CREATE then
          MESURE_IO.CREATE (MESURE_FILE, MESURE_IO.INOUT_FILE, FILE_NAME);
        else
          raise FILE_NOT_FOUND_ERROR;
        end if;
    end;
  exception
    when FILE_NOT_FOUND_ERROR =>
      raise;
    when others =>
      raise IO_ERROR;
  end OPEN;

  procedure CLOSE is
  begin
    MESURE_IO.CLOSE (MESURE_FILE);
  exception
    when others =>
      null;
  end CLOSE;

  function LOAD (FILE_NAME : MESU_NAM.FILE_NAME_STR)
  return MESU_DEF.MESURE_REC is
    MESURE : MESU_DEF.MESURE_REC;
    TMP : FILE_REC;
    DATE : MESU_NAM.FILE_DATE_STR;
    NO   : MESU_NAM.FILE_NO_STR;
    PID  : MESU_NAM.FILE_PID_STR;
  begin
    MESU_NAM.SPLIT_FILE_NAME (FILE_NAME, DATE, NO, PID);
    if      DATE = MESU_NAM.WILD_DATE_STR
    or else NO   = MESU_NAM.WILD_NO_STR
    or else PID  = MESU_NAM.WILD_PID_STR then
      raise FILE_NAME_ERROR;
    end if;
    OPEN (FILE_NAME, FALSE);
    MESURE_IO.READ (MESURE_FILE, TMP);
    MESURE_IO.CLOSE (MESURE_FILE);
    MESURE := (PID => PERS_DEF.PID_RANGE'VALUE(PID),
               DATE => DATE,
               SAMPLING_DELTA => TMP.SAMPLING_DELTA,
               COMMENT => TMP.COMMENT,
               TZ => TMP.TZ,
               SAMPLES => TMP.SAMPLES);

    return MESURE;
  exception
    when FILE_NOT_FOUND_ERROR =>
      raise;
    when others =>
      CLOSE;
      raise IO_ERROR;
  end LOAD;

  procedure SAVE (FILE_NO : in MESU_NAM.FILE_NO_STR;
                  MESURE  : in MESU_DEF.MESURE_REC) is
    FILE_NAME : MESU_NAM.FILE_NAME_STR;
    TMP : FILE_REC;
  begin
    if FILE_NO = MESU_NAM.WILD_NO_STR then
      raise FILE_NAME_ERROR;
    end if;
    FILE_NAME := MESU_NAM.BUILD_FILE_NAME (MESURE.DATE, FILE_NO,
                          NORMAL(INTEGER(MESURE.PID), 3, GAP => '0'));
    TMP := (SAMPLING_DELTA => MESURE.SAMPLING_DELTA,
            COMMENT => MESURE.COMMENT,
            TZ => MESURE.TZ,
            SAMPLES => MESURE.SAMPLES);
    OPEN (FILE_NAME);
    MESURE_IO.WRITE (MESURE_FILE, TMP);
    MESURE_IO.CLOSE (MESURE_FILE);
  exception
    when others =>
      CLOSE;
      raise IO_ERROR;
  end SAVE;

  -- Delete a mesure file
  -- Pid and date of the mesure are used to build the file name
  procedure DELETE (FILE_NAME : in MESU_NAM.FILE_NAME_STR) is
  begin
    -- No space in file_name
    if TEXT_HANDLER.LOCATE (TEXT_HANDLER.TO_TEXT(FILE_NAME), ' ') /= 0 then
      raise FILE_NAME_ERROR;
    end if;
    OPEN (FILE_NAME);
    MESURE_IO.DELETE (MESURE_FILE);
  exception
    when others =>
      CLOSE;
      raise IO_ERROR;
  end DELETE;

end MESU_FIL;