with CON_IO, AFPX, NORMAL, GET_LINE, TEXT_HANDLER;
with PERS_DEF, PERS_MNG, MESU_DEF, MESU_FIL, STR_MNG;
use AFPX;
-- with MESU_NAM;
-- Edition, Creation, deletion of mesure
package body MESU_EDI is

  subtype IMPORT_FILE_NAME_STR is STRING (1 ..22);

  procedure ENCODE (PERSON : in PERS_DEF.PERSON_REC;
                    MESURE : in MESU_DEF.MESURE_REC) is
    DATE_R : STR_MNG.DATE_STR_REC;
  begin
    -- Person name and activity
    AFPX.ENCODE_FIELD (04, (00, 00), PERSON.NAME);
    AFPX.ENCODE_FIELD (06, (00, 00), PERSON.ACTIVITY);

    -- TZ, Date, comment, sampling
    for I in PERS_DEF.PERSON_TZ_ARRAY'RANGE loop
      AFPX.ENCODE_FIELD (AFPX.FIELD_RANGE(8 + I), (00, 00),
                         STR_MNG.TO_STR(MESURE.TZ(I)));
    end loop;

    STR_MNG.TO_REC (MESURE.DATE, DATE_R);
    AFPX.ENCODE_FIELD (16, (00, 00), DATE_R.DAY);
    AFPX.ENCODE_FIELD (18, (00, 00), DATE_R.MONTH);
    AFPX.ENCODE_FIELD (20, (00, 00), DATE_R.YEAR);
    AFPX.ENCODE_FIELD (22, (00, 00), MESURE.COMMENT);
    AFPX.ENCODE_FIELD (24, (00, 00),
                       NORMAL(INTEGER(MESURE.SAMPLING_DELTA), 3));
    -- Samples
    for I in 1 .. 100 loop
      AFPX.ENCODE_FIELD (AFPX.FIELD_RANGE(24 + I), (00, 00),
                         STR_MNG.TO_STR(MESURE.SAMPLES(I)) );
    end loop;
  end ENCODE;

  procedure PROTECT (FIELD_NO : in AFPX.FIELD_RANGE) is
  begin
    AFPX.SET_FIELD_COLORS (FIELD_NO, FOREGROUND => CON_IO.CYAN,
                                     BACKGROUND => CON_IO.BLACK);
    AFPX.SET_FIELD_PROTECTION (FIELD_NO, TRUE);
  end PROTECT;


  -- Import sample data from ascii file
  procedure IMPORT_SAMPLES (IMPORT_FILE_NAME : in IMPORT_FILE_NAME_STR;
                            MESURE : in out MESU_DEF.MESURE_REC;
                            OK : out BOOLEAN) is
    IMPORT_FILE_NAME_DEF : constant IMPORT_FILE_NAME_STR := (others => ' ');
    subtype IMPORT_FILE_NAME_INDEX is INTEGER range IMPORT_FILE_NAME_STR'RANGE;
    IMPORT_FILE_NAME_LAST : IMPORT_FILE_NAME_INDEX;
    SAMPLES : MESU_DEF.MAX_SAMPLE_ARRAY
            := (others => PERS_DEF.BPM_RANGE'FIRST);
    SAMPLES_INDEX : MESU_DEF.SAMPLE_NB_RANGE := MESU_DEF.SAMPLE_NB_RANGE'FIRST;
    package GET_SAMPLE is new GET_LINE (
                       MAX_WORD_LEN => 100,
                       MAX_WORD_NB => 40,
                       MAX_LINE_LEN => 132);
    SAMPLE_LINE : GET_SAMPLE.LINE_ARRAY;
    SAMPLE_LINE_TXT : GET_SAMPLE.LINE_TXT;
  begin
    OK := FALSE;
    -- Check if file name is empty
    if IMPORT_FILE_NAME = IMPORT_FILE_NAME_DEF then
      return;
    end if;
    -- Check no ':' (file has to be on current drive)
    for I in IMPORT_FILE_NAME'RANGE loop
      if IMPORT_FILE_NAME(I) = ':' then
        return;
      end if;
    end loop;
    -- Remove trailing spaces
    for I in reverse IMPORT_FILE_NAME'RANGE loop
      if IMPORT_FILE_NAME(I) /= ' ' then
        -- Always occures cause IMPORT_FILE_NAME /= IMPORT_FILE_NAME_DEF
        IMPORT_FILE_NAME_LAST := I;
        exit;
      end if;
    end loop;

    -- Open file
    GET_SAMPLE.OPEN(FILE_NAME => IMPORT_FILE_NAME(IMPORT_FILE_NAME'FIRST .. IMPORT_FILE_NAME_LAST));

    -- Read, decode, store in SAMPLES
    loop
      -- Get whole line
      GET_SAMPLE.GET_WHOLE_LINE (SAMPLE_LINE_TXT);

      -- Discard empty lines and comments
      if TEXT_HANDLER.EMPTY (SAMPLE_LINE_TXT)
      or else TEXT_HANDLER.VALUE (SAMPLE_LINE_TXT)(1) /= '#' then

        -- Split line in words
        GET_SAMPLE.GET_WORDS (SAMPLE_LINE);
        -- Not empty nor a comment
        for I in 1 .. GET_SAMPLE.GET_WORD_NUMBER loop
          -- Decode a BPM
          SAMPLES(SAMPLES_INDEX) := PERS_DEF.BPM_RANGE'VALUE (
                                       TEXT_HANDLER.VALUE (SAMPLE_LINE(I)));
          SAMPLES_INDEX := SAMPLES_INDEX + 1;
        end loop;

      end if;

      -- Read next line and exit when end of file
      begin
        GET_SAMPLE.READ_NEXT_LINE;
      exception
        when GET_SAMPLE.NO_MORE_LINE =>
          exit;
      end;
    end loop;

    -- Close file
    GET_SAMPLE.CLOSE;

    -- So far so good... Copy SAMPLES in mesure
    MESURE.SAMPLES := SAMPLES;

    -- Done
    OK := TRUE;
  exception
    when others =>
      -- OK is FALSE. Close file if open.
      begin
        GET_SAMPLE.CLOSE;
      exception
        when others =>
          null;
      end;
  end IMPORT_SAMPLES;


  -- Edit a mesure.
  -- If date or person changes, then the file name may be affected.
  -- If FILE_NAME is empty as input, then it is a creation and file_name
  --  is affected
  procedure EDIT (FILE_NAME : in out MESU_NAM.FILE_NAME_STR;
                  EXIT_PROGRAM : out BOOLEAN) is
    IN_CREATE : BOOLEAN;
    PERSON : PERS_DEF.PERSON_REC;
    POS_PERS : INTEGER;
    DATE_S : MESU_NAM.FILE_DATE_STR;
    NO_S   : MESU_NAM.FILE_NO_STR;
    PID_S  : MESU_NAM.FILE_PID_STR;
    BPM_S  : STR_MNG.BPM_STR;
    MESURE : MESU_DEF.MESURE_REC;
    VALID_DATE : BOOLEAN;
    SPACE_FOUND : BOOLEAN;

    CURSOR_FIELD : AFPX.FIELD_RANGE;
    CURSOR_COL   : CON_IO.COL_RANGE;
    PTG_RESULT   : AFPX.RESULT_REC;

    OK : BOOLEAN;
    REDISPLAY : BOOLEAN;

    use PERS_DEF;

    -- Check a field
    procedure CHECK_FIELD (CURRENT_FIELD : in out AFPX.ABSOLUTE_FIELD_RANGE;
                           FOR_VALID : in BOOLEAN;
                           OK : out BOOLEAN) is
      DATE_R : STR_MNG.DATE_STR_REC;
      DELTA_STR : STRING (1 .. 3);
      LOCOK : BOOLEAN;
      IMPORT_FILE_NAME : IMPORT_FILE_NAME_STR;
    begin
      case CURRENT_FIELD is

        when 04 | 06 =>
          -- In name or activity
          -- Expand name & activity
          PERSON.NAME     := AFPX.DECODE_FIELD (04, 00);
          PERSON.ACTIVITY := AFPX.DECODE_FIELD (06, 00);
          PERS_MNG.EXPAND (PERS_DEF.THE_PERSONS,
                           PERSON.NAME, PERSON.ACTIVITY, POS_PERS);

          AFPX.ENCODE_FIELD (04, (00, 00), PERSON.NAME);
          AFPX.ENCODE_FIELD (06, (00, 00), PERSON.ACTIVITY);
          -- Set pos in case of end
          if POS_PERS > 0 then
            -- Uniq
            PERS_DEF.PERSON_LIST_MNG.MOVE_TO (
                PERS_DEF.THE_PERSONS, PERS_DEF.PERSON_LIST_MNG.NEXT,
                POS_PERS - 1, FALSE);
            PERS_DEF.PERSON_LIST_MNG.READ (PERS_DEF.THE_PERSONS, PERSON,
                PERS_DEF.PERSON_LIST_MNG.CURRENT);
            -- Copy PID
            MESURE.PID := PERSON.PID;
            if not FOR_VALID then
              -- Encode TZ
              AFPX.ENCODE_FIELD (09, (00,00), STR_MNG.TO_STR(PERSON.TZ(1)));
              AFPX.ENCODE_FIELD (10, (00,00), STR_MNG.TO_STR(PERSON.TZ(2)));
              AFPX.ENCODE_FIELD (11, (00,00), STR_MNG.TO_STR(PERSON.TZ(3)));
              AFPX.ENCODE_FIELD (12, (00,00), STR_MNG.TO_STR(PERSON.TZ(4)));
              AFPX.ENCODE_FIELD (13, (00,00), STR_MNG.TO_STR(PERSON.TZ(5)));
              AFPX.ENCODE_FIELD (14, (00,00), STR_MNG.TO_STR(PERSON.TZ(6)));
              -- Go to date
              CURRENT_FIELD := 16;
            else
              -- Next check : TZ
              CURRENT_FIELD := 09;
            end if;
            LOCOK := TRUE;
          elsif POS_PERS = 0 then
            CURRENT_FIELD := 06;
            LOCOK := FALSE;
          else
            CURRENT_FIELD := 04;
            LOCOK := FALSE;
          end if;

        when 09 .. 14 =>
          -- In TZ check it
          BPM_S := AFPX.DECODE_FIELD (CURRENT_FIELD, 00);
          STR_MNG.PARSE(BPM_S);
          LOCOK := not STR_MNG.HAS_HOLES(BPM_S);
          if LOCOK then
            begin
              MESURE.TZ(INTEGER(CURRENT_FIELD - 8)) := STR_MNG.TO_BPM(BPM_S);
            exception
              when others =>
                LOCOK := FALSE;
            end;
          end if;
          if LOCOK then
            if CURRENT_FIELD /= 14 then
              CURRENT_FIELD := CURRENT_FIELD + 1;
            else
              CURRENT_FIELD := 16;
            end if;
          end if;

        when 16 | 18 | 20 =>
          -- In date
          -- Check date : no space
          CURRENT_FIELD := 16;
          DATE_R.DAY   := AFPX.DECODE_FIELD (16, 00);
          LOCOK := not STR_MNG.HAS_SPACES(DATE_R.DAY);
          if LOCOK then
            CURRENT_FIELD := 18;
            DATE_R.MONTH := AFPX.DECODE_FIELD (18, 00);
            LOCOK := not STR_MNG.HAS_SPACES(DATE_R.MONTH);
          end if;
          if LOCOK then
            CURRENT_FIELD := 20;
            DATE_R.YEAR  := AFPX.DECODE_FIELD (20, 00);
            LOCOK := not STR_MNG.HAS_SPACES(DATE_R.YEAR);
          end if;

          -- Check date : valid
          if LOCOK then
            CURRENT_FIELD := 16;
            STR_MNG.CHECK_DATE (DATE_R, TRUE, MESURE.DATE, VALID_DATE);
            LOCOK := VALID_DATE;
          end if;
          if LOCOK then
            CURRENT_FIELD := 22;
          end if;

        when 22 =>
          -- In comment, no check
          MESURE.COMMENT := AFPX.DECODE_FIELD (22, 00);
          CURRENT_FIELD := 24;

        when 24 =>
          -- In sampling delta
          DELTA_STR := AFPX.DECODE_FIELD(24, 00);
          STR_MNG.PARSE (DELTA_STR);
          -- No hole no space
          LOCOK := not STR_MNG.HAS_HOLES (DELTA_STR);
          LOCOK := LOCOK and then not STR_MNG.IS_SPACES (DELTA_STR);
          if LOCOK then
            begin
              MESURE.SAMPLING_DELTA :=
                 MESU_DEF.SAMPLING_DELTA_RANGE'VALUE(DELTA_STR);
            exception
              when others =>
                LOCOK := FALSE;
            end;
          end if;
          if LOCOK then
            CURRENT_FIELD := 25;
          end if;

        when 25 .. 124 =>
          -- In a sample
          BPM_S := AFPX.DECODE_FIELD (CURRENT_FIELD, 0);
          STR_MNG.PARSE (BPM_S);
          -- No holes
          LOCOK := not STR_MNG.HAS_HOLES(BPM_S);
          if LOCOK then
            begin
              MESURE.SAMPLES(MESU_DEF.SAMPLE_NB_RANGE(CURRENT_FIELD - 24)) :=
                 STR_MNG.TO_BPM(BPM_S);
            exception
              when others =>
                LOCOK := FALSE;
            end;
          end if;
          -- No empty
          if LOCOK then
            -- "Next" field
            if CURRENT_FIELD /= 124 then
              CURRENT_FIELD := CURRENT_FIELD + 1;
            else
              CURRENT_FIELD := 04;
            end if;
          end if;

        when 130 =>
          -- In import file name
          IMPORT_FILE_NAME := AFPX.DECODE_FIELD (CURRENT_FIELD, 00);
          -- Import data
          IMPORT_SAMPLES (IMPORT_FILE_NAME, MESURE, LOCOK);
          -- If ok, Encode samples and move to first sample,
          --  otherwise stay here
          if LOCOK then
            for I in 1 .. 100 loop
              AFPX.ENCODE_FIELD (AFPX.FIELD_RANGE(24 + I), (00, 00),
                                 STR_MNG.TO_STR(MESURE.SAMPLES(I)) );
            end loop;
            CURRENT_FIELD := 25;
          end if;

        when others =>
          null;

      end case;

      CURSOR_COL := 0;
      OK := LOCOK;

    end CHECK_FIELD;


  begin
    -- Use descriptor
    AFPX.USE_DESCRIPTOR (3);
    IN_CREATE := STR_MNG.IS_SPACES (FILE_NAME);

    if IN_CREATE then
      -- Set person
      AFPX.ENCODE_FIELD (01, (00,00), "Creation");
      PERSON.NAME := (others => ' ');
      PERSON.ACTIVITY := (others => ' ');
    else
      -- Load person
      AFPX.ENCODE_FIELD (01, (00,00), " Edition");
      MESU_NAM.SPLIT_FILE_NAME (FILE_NAME, DATE_S, NO_S, PID_S);
      PERSON.PID := PERS_DEF.PID_RANGE'VALUE(PID_S);
      PERS_MNG.SEARCH (PERS_DEF.THE_PERSONS, PERSON.PID, POS_PERS);
      PERS_DEF.PERSON_LIST_MNG.READ (PERS_DEF.THE_PERSONS, PERSON,
                                     PERS_DEF.PERSON_LIST_MNG.CURRENT);
      -- Disable import button
      AFPX.SET_FIELD_ACTIVATION (128, FALSE);
      AFPX.SET_FIELD_ACTIVATION (129, FALSE);
      AFPX.SET_FIELD_ACTIVATION (130, FALSE);
    end if;


    -- Set mesure
    if IN_CREATE then
      -- Init date to current
      MESURE.DATE := STR_MNG.CURRENT_DATE;
    else
      -- Load
      MESURE := MESU_FIL.LOAD (FILE_NAME);
    end if;


    ENCODE (PERSON, MESURE);

    if IN_CREATE then
      -- Person name
      CURSOR_FIELD := 04;
    else
      -- Date
      CURSOR_FIELD := 16;
    end if;
    CURSOR_COL := 0;
    REDISPLAY := FALSE;

    -- Loop of PTGs
    loop
      AFPX.ENCODE_FIELD (02, (00, 00), STR_MNG.CURRENT_DATE_PRINTED);
      AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
      REDISPLAY := FALSE;

      case PTG_RESULT.EVENT is
        when REFRESH  =>
          REDISPLAY := TRUE;
        when KEYBOARD =>

          case PTG_RESULT.KEYBOARD_KEY is
            when RETURN_KEY =>
              -- Check field and go to next if OK
              CHECK_FIELD (CURSOR_FIELD, FALSE, OK);
            when ESCAPE_KEY =>
              -- Clear current field
              if CURSOR_FIELD = 04 then
                AFPX.CLEAR_FIELD (04);
                AFPX.CLEAR_FIELD (06);
                CURSOR_FIELD := 04;
              elsif CURSOR_FIELD = 06 then
                AFPX.CLEAR_FIELD (06);
              elsif CURSOR_FIELD = 16 or else CURSOR_FIELD = 18
              or else CURSOR_FIELD = 20 then
                AFPX.CLEAR_FIELD (16);
                AFPX.CLEAR_FIELD (18);
                AFPX.CLEAR_FIELD (20);
                CURSOR_FIELD := 16;
              else
                AFPX.CLEAR_FIELD (CURSOR_FIELD);
              end if;
              CURSOR_COL := 0;
            when BREAK_KEY =>
              EXIT_PROGRAM := TRUE;
              exit;
          end case;

        when MOUSE_BUTTON =>
          if PTG_RESULT.FIELD_NO = 127 then
            -- Exit
            EXIT_PROGRAM := TRUE;
            exit;
          elsif PTG_RESULT.FIELD_NO = 128 then
            -- Import samples
            CURSOR_FIELD := 130;
            CHECK_FIELD (CURSOR_FIELD, FALSE, OK);
          elsif PTG_RESULT.FIELD_NO = 126 then
            -- Cancel
            FILE_NAME := MESU_NAM.FILE_NAME_STR'((others => ' '));
            EXIT_PROGRAM := FALSE;
            exit;
          elsif PTG_RESULT.FIELD_NO = 125 then
            -- Clear import file name
            AFPX.CLEAR_FIELD (129);

            -- Valid check all fields but import file one by one
            CURSOR_FIELD := 04;
            loop
              CHECK_FIELD (CURSOR_FIELD, TRUE, OK);
              exit when not OK or else CURSOR_FIELD = 04;
            end loop;

            -- Check no hole in TZ, crescent
            if OK then
              for I in PERS_DEF.PERSON_TZ_ARRAY'RANGE loop
                CURSOR_FIELD := AFPX.FIELD_RANGE(08 + I);
                if PERS_DEF."=" (MESURE.TZ(I), PERS_DEF.BPM_RANGE'FIRST) then
                  OK := FALSE;
                  exit;
                end if;
                if I /= PERS_DEF.PERSON_TZ_ARRAY'FIRST
                and then MESURE.TZ(I - 1) >= MESURE.TZ(I) then
                  OK := FALSE;
                  exit;
                end if;
              end loop;
            end if;

            -- Check no hole in samples
            if OK then
              SPACE_FOUND := FALSE;
              for I in MESU_DEF.SAMPLE_NB_RANGE loop
                CURSOR_FIELD := AFPX.FIELD_RANGE(24 + I);
                if PERS_DEF."=" (MESURE.SAMPLES(I), PERS_DEF.BPM_RANGE'FIRST)
                then
                  SPACE_FOUND := TRUE;
                elsif SPACE_FOUND then
                  OK := FALSE;
                  exit;
                end if;
              end loop;
            end if;

            -- Delete if needed, then find slot if needed
            if OK then
              -- Check if file to be deleted
              if not IN_CREATE and then
                (        MESURE.DATE /= DATE_S
                 or else PID_S /= STR_MNG.PID_STR(MESURE.PID)) then
                -- Necessity to change file_name. Delete previous.
                MESU_FIL.DELETE (FILE_NAME);
              end if;
              if      IN_CREATE
              or else MESURE.DATE /= DATE_S
              or else PID_S /= STR_MNG.PID_STR(MESURE.PID) then
                -- Necessity to create a new file_name.
                -- Build new file name (find_slot) -> set NO_S
                PID_S := STR_MNG.PID_STR(MESURE.PID);
                NO_S := MESU_NAM.FIND_SLOT (DATE => MESURE.DATE,
                                            PID  => PID_S);
                -- Ok if an empty slot is found. Build file name
                OK := NO_S /= MESU_NAM.WILD_NO_STR;
                if OK then
                  FILE_NAME :=
                    MESU_NAM.BUILD_FILE_NAME (MESURE.DATE, NO_S, PID_S);
                end if;
              end if;
            end if;

            if OK then
              -- Save
              MESU_FIL.SAVE (NO_S, MESURE);
              EXIT_PROGRAM := FALSE;
              exit;
            end if;

          end if; -- PTG_RESULT.FIELD_NO = valid or cancel

      end case; -- RESULT.EVENT

    end loop;

  end EDIT;

  -- Delete a mesure
  procedure DELETE (FILE_NAME : in out MESU_NAM.FILE_NAME_STR;
                    EXIT_PROGRAM : out BOOLEAN) is
    PERSON : PERS_DEF.PERSON_REC;
    POS_PERS : INTEGER;
    DATE_S : MESU_NAM.FILE_DATE_STR;
    NO_S   : MESU_NAM.FILE_NO_STR;
    PID_S  : MESU_NAM.FILE_PID_STR;
    MESURE : MESU_DEF.MESURE_REC;

    CURSOR_FIELD : AFPX.FIELD_RANGE;
    CURSOR_COL   : CON_IO.COL_RANGE;
    PTG_RESULT   : AFPX.RESULT_REC;
    REDISPLAY    : BOOLEAN;
  begin

    -- Load person
    MESU_NAM.SPLIT_FILE_NAME (FILE_NAME, DATE_S, NO_S, PID_S);
    PERSON.PID := PERS_DEF.PID_RANGE'VALUE(PID_S);
    PERS_MNG.SEARCH (PERS_DEF.THE_PERSONS, PERSON.PID, POS_PERS);
    PERS_DEF.PERSON_LIST_MNG.READ (PERS_DEF.THE_PERSONS, PERSON,
                                     PERS_DEF.PERSON_LIST_MNG.CURRENT);
    -- Load mesure
    MESURE := MESU_FIL.LOAD (FILE_NAME);

    -- Use descriptor and encode
    AFPX.USE_DESCRIPTOR (3);
    ENCODE (PERSON, MESURE);

    -- Tittle
    AFPX.ENCODE_FIELD (01, (00,00), "Deletion");

    -- Protect fields
    -- Person name, activity, TZ
    PROTECT (04);
    PROTECT (06);
    for I in PERS_DEF.PERSON_TZ_ARRAY'RANGE loop
      PROTECT (AFPX.FIELD_RANGE(8 + I));
    end loop;
    -- Date, comment, samples
    PROTECT (16);
    PROTECT (18);
    PROTECT (20);
    PROTECT (22);
    PROTECT (24);
    for I in 1 .. 100 loop
      PROTECT (AFPX.FIELD_RANGE(24 + I));
    end loop;

    -- Disable import
    AFPX.SET_FIELD_ACTIVATION (128, FALSE);
    AFPX.SET_FIELD_ACTIVATION (129, FALSE);
    AFPX.SET_FIELD_ACTIVATION (130, FALSE);

    CURSOR_FIELD := 01;
    CURSOR_COL := 0;
    REDISPLAY := FALSE;

    -- Loop of PTGs
    loop
      AFPX.ENCODE_FIELD (02, (00, 00), STR_MNG.CURRENT_DATE_PRINTED);
      AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
      REDISPLAY := FALSE;

      case PTG_RESULT.EVENT is
        when REFRESH =>
          REDISPLAY := TRUE;
        when KEYBOARD =>

          case PTG_RESULT.KEYBOARD_KEY is
            when RETURN_KEY | ESCAPE_KEY =>
              null;
            when BREAK_KEY =>
              EXIT_PROGRAM := TRUE;
              exit;
          end case;

        when MOUSE_BUTTON =>

          if PTG_RESULT.FIELD_NO = 126 then
            -- Cancel
            FILE_NAME := MESU_NAM.FILE_NAME_STR'((others => ' '));
            EXIT_PROGRAM := FALSE;
            exit;
          elsif PTG_RESULT.FIELD_NO = 125 then
            -- Delete
            MESU_FIL.DELETE (FILE_NAME);
            EXIT_PROGRAM := FALSE;
            exit;
          end if; -- PTG_RESULT.FIELD_NO = valid or cancel

      end case; -- RESULT.EVENT

    end loop;

  end DELETE;

end MESU_EDI;
