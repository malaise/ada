with AFPX, CON_IO, NORMAL, UPPER_STR, MATH;
with PERS_DEF, STR_MNG, MESU_MNG, PERS_MNG, PERS_FIL;
package body PERS_LIS is

  procedure BUILD_LIST is
    PERSON : PERS_DEF.PERSON_REC;
    LINE   : AFPX.LINE_REC;
    use PERS_DEF.PERSON_LIST_MNG;
  begin
    -- Encode list of persons
    AFPX.LINE_LIST_MNG.DELETE_LIST (AFPX.LINE_LIST);
    if not PERS_DEF.PERSON_LIST_MNG.IS_EMPTY (PERS_DEF.THE_PERSONS) then
      MOVE_TO (PERS_DEF.THE_PERSONS, PERS_DEF.PERSON_LIST_MNG.NEXT, 0 , FALSE);
      loop
        READ (PERS_DEF.THE_PERSONS, PERSON, PERS_DEF.PERSON_LIST_MNG.CURRENT);
        STR_MNG.FORMAT_PERSON_TO_LIST (PERSON, LINE);
        AFPX.LINE_LIST_MNG.INSERT (AFPX.LINE_LIST, LINE);
        exit when GET_POSITION (PERS_DEF.THE_PERSONS)
                = LIST_LENGTH (PERS_DEF.THE_PERSONS);
        MOVE_TO (PERS_DEF.THE_PERSONS);
      end loop;
      -- End of list
      AFPX.LINE_LIST_MNG.MOVE_TO (AFPX.LINE_LIST, AFPX.LINE_LIST_MNG.PREV,
                                  0, FALSE);
    end if;
  end BUILD_LIST;


  procedure SET_PROTECTION (FIELD : in AFPX.FIELD_RANGE;
                            PROTECT : in BOOLEAN) is
  begin
    AFPX.SET_FIELD_PROTECTION (FIELD, PROTECT);
    if PROTECT then
      AFPX.SET_FIELD_COLORS (FIELD, FOREGROUND => CON_IO.CYAN,
                                    BACKGROUND => CON_IO.BLACK);
    else
      AFPX.RESET_FIELD(FIELD, RESET_COLORS=>TRUE, RESET_STRING=>FALSE);
    end if;
  end SET_PROTECTION;

  procedure LIST (EXIT_PROGRAM : out BOOLEAN) is

    FIRST_FIELD  : AFPX.FIELD_RANGE;

    CURSOR_FIELD : AFPX.FIELD_RANGE;
    CURSOR_COL   : CON_IO.COL_RANGE;
    PTG_RESULT   : AFPX.RESULT_REC;
    REDISPLAY    : BOOLEAN;

    LIST_EMPTY : BOOLEAN;
    type STATE_LIST is (IN_LIST, IN_CREATE, IN_EDIT, IN_DELETE);
    STATE : STATE_LIST;
    ACT : BOOLEAN;

    PERSON : PERS_DEF.PERSON_REC;
    POS : NATURAL;
    OK : BOOLEAN;
    use AFPX;
    use PERS_DEF.PERSON_LIST_MNG;

    procedure ENCODE_PERSON is
    begin
      AFPX.ENCODE_FIELD (11, (00, 00), PERSON.NAME);
      AFPX.ENCODE_FIELD (13, (00, 00), PERSON.ACTIVITY);
      for I in 1 .. 6 loop
        AFPX.ENCODE_FIELD (AFPX.FIELD_RANGE (I + 15), (00, 00),
                           STR_MNG.TO_STR(PERSON.TZ(I)) );
      end loop;
    end ENCODE_PERSON;



    -- Check a field
    procedure CHECK_FIELD (CURRENT_FIELD : in out AFPX.ABSOLUTE_FIELD_RANGE;
                           OK : out BOOLEAN) is
      LOCOK : BOOLEAN;
      TZ_S  : STR_MNG.BPM_STR;
      TZ    : PERS_DEF.BPM_RANGE;
      use PERS_DEF;
    begin
      case CURRENT_FIELD is

        when 11 =>
          -- In name : not empty
          PERSON.NAME := UPPER_STR (AFPX.DECODE_FIELD (11, 00));
          STR_MNG.PARSE (PERSON.NAME);
          LOCOK := not STR_MNG.IS_SPACES (PERSON.NAME);
          if LOCOK then
            AFPX.ENCODE_FIELD (11, (00, 00), PERSON.NAME);
            CURRENT_FIELD := 13;
          end if;

        when 13 =>
          -- In activity : not empty
          PERSON.ACTIVITY := UPPER_STR (AFPX.DECODE_FIELD (13, 00));
          STR_MNG.PARSE (PERSON.ACTIVITY);
          LOCOK := not STR_MNG.IS_SPACES (PERSON.ACTIVITY);
          if LOCOK then
            AFPX.ENCODE_FIELD (13, (00, 00), PERSON.ACTIVITY);
            CURRENT_FIELD := 16;
          end if;

        when 16 | 17 | 18 | 19 | 20 | 21 =>
          TZ_S := AFPX.DECODE_FIELD (CURRENT_FIELD, 00);
          begin
            TZ := STR_MNG.TO_BPM(TZ_S);
          exception
            when others =>
              LOCOK := FALSE;
          end;
          if LOCOK then
            PERSON.TZ (INTEGER(CURRENT_FIELD) - 15) := TZ;
          end if;
          if LOCOK then
            LOCOK := TZ /= PERS_DEF.BPM_RANGE'FIRST;
          end if;
          if LOCOK then
            -- TZ must be crescent
            if CURRENT_FIELD /= 16
            and then TZ /= PERS_DEF.BPM_RANGE'FIRST then
              LOCOK := TZ > PERSON.TZ (INTEGER(CURRENT_FIELD) - 16);
            end if;
          end if;
          if LOCOK then
            AFPX.ENCODE_FIELD (CURRENT_FIELD, (00, 00), STR_MNG.TO_STR(TZ) );
            if CURRENT_FIELD = 21 then
              CURRENT_FIELD := FIRST_FIELD;
            else
              CURRENT_FIELD := CURRENT_FIELD + 1;
            end if;
          end if;

        when others =>
          null;
      end case;

      CURSOR_COL := 0;
      OK := LOCOK;

    end CHECK_FIELD;

    function CHECK_COMPUTE return BOOLEAN is
      subtype TZ_RANGE is INTEGER range PERS_DEF.PERSON_TZ_ARRAY'FIRST ..
                                        PERS_DEF.PERSON_TZ_ARRAY'LAST;

      function GET_TZ (N : TZ_RANGE) return PERS_DEF.BPM_RANGE is
        TZ_S  : STR_MNG.BPM_STR;
      begin
        TZ_S := AFPX.DECODE_FIELD (AFPX.FIELD_RANGE(N) + 15, 00);
        return STR_MNG.TO_BPM(TZ_S);
      end GET_TZ;

      use PERS_DEF;
    begin
      CURSOR_COL := 0;
      -- Last field must not be empty and valid
      CURSOR_FIELD := 21;
      PERSON.TZ(6) := GET_TZ (6);
      if PERSON.TZ(6) = PERS_DEF.BPM_RANGE'FIRST then
        return FALSE;
      end if;

      -- First field must be empty or valid
      CURSOR_FIELD := 16;
      PERSON.TZ(1) := GET_TZ (1);

      -- First value > last value
      if PERSON.TZ(1) >= PERSON.TZ(6) then
        return FALSE;
      end if;

      -- Other fields must be empty
      for I in 2 .. 5 loop
        CURSOR_FIELD := AFPX.FIELD_RANGE(15 + I);
        if GET_TZ(I) /= 0 then
          return FALSE;
        end if;
      end loop;

      return TRUE;

    exception
      when others => return FALSE;
    end CHECK_COMPUTE;



   begin
    EXIT_PROGRAM := FALSE;

    AFPX.USE_DESCRIPTOR(2);

    STATE := IN_LIST;

    CURSOR_FIELD := 01;
    CURSOR_COL := 0;
    REDISPLAY := FALSE;

    BUILD_LIST;

    loop

      LIST_EMPTY := AFPX.LINE_LIST_MNG.LIST_LENGTH(AFPX.LINE_LIST) = 0;
      -- List and menu buttons, only in list
      ACT := STATE = IN_LIST;
      AFPX.SET_FIELD_ACTIVATION (03, ACT);
      AFPX.SET_FIELD_ACTIVATION (04, ACT);
      AFPX.SET_FIELD_ACTIVATION (06, ACT);
      -- Delete/edit if not empty and in list
      ACT := ACT and then not LIST_EMPTY;
      AFPX.SET_FIELD_ACTIVATION (07, ACT);
      AFPX.SET_FIELD_ACTIVATION (08, ACT);
      -- Edit if edit
      ACT := STATE /= IN_LIST;
      for I in AFPX.FIELD_RANGE'(10) .. 23 loop
        AFPX.SET_FIELD_ACTIVATION (I, ACT);
      end loop;
      -- Un protect person name & activity if in create
      ACT := STATE = IN_CREATE;
      SET_PROTECTION (11, not ACT);
      SET_PROTECTION (13, not ACT);
      -- Un protect other fields if in create or edit
      ACT := STATE = IN_CREATE or else STATE = IN_EDIT;
      SET_PROTECTION (16, not ACT);
      SET_PROTECTION (17, not ACT);
      SET_PROTECTION (18, not ACT);
      SET_PROTECTION (19, not ACT);
      SET_PROTECTION (20, not ACT);
      SET_PROTECTION (21, not ACT);
      -- Compute if in edit or create
      AFPX.SET_FIELD_ACTIVATION (24, ACT);
      -- Confirm if Valid
      AFPX.SET_FIELD_ACTIVATION (09, STATE = IN_DELETE);


      AFPX.ENCODE_FIELD (02, (00, 00), STR_MNG.CURRENT_DATE_PRINTED);

      AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
      REDISPLAY := FALSE;

      case PTG_RESULT.EVENT is

        when REFRESH =>
          REDISPLAY := TRUE;
        when KEYBOARD =>

          case PTG_RESULT.KEYBOARD_KEY is
            when RETURN_KEY =>
              -- Check field and go to next if OK
              CHECK_FIELD (CURSOR_FIELD, OK);
            when ESCAPE_KEY =>
              -- Clear current field
              if CURSOR_FIELD = 11  then
                AFPX.CLEAR_FIELD (11);
                AFPX.CLEAR_FIELD (13);
                CURSOR_FIELD := 11;
              else
                AFPX.CLEAR_FIELD (CURSOR_FIELD);
              end if;
              CURSOR_COL := 0;
            when BREAK_KEY =>
              EXIT_PROGRAM := TRUE;
              exit;
          end case;

        when MOUSE_BUTTON =>

          case PTG_RESULT.FIELD_NO is
            when 04 =>
              -- Back to records
              exit;
            when 05 =>
              -- Exit
              EXIT_PROGRAM := TRUE;
              exit;
            when 06 =>
              -- Create
              STATE := IN_CREATE;
              FIRST_FIELD := 11;
              CURSOR_FIELD := FIRST_FIELD;
              CURSOR_COL := 0;
              PERSON.NAME := (others => ' ');
              PERSON.ACTIVITY := (others => ' ');
              PERSON.TZ := (others => PERS_DEF.BPM_RANGE'FIRST);
              ENCODE_PERSON;
            when 08 =>
              -- Edit
              STATE := IN_EDIT;
              FIRST_FIELD := 16;
              CURSOR_FIELD := FIRST_FIELD;
              CURSOR_COL := 0;
              READ (PERS_DEF.THE_PERSONS, PERSON,
                    PERS_DEF.PERSON_LIST_MNG.CURRENT);
              ENCODE_PERSON;
            when 07 =>
              -- Delete
              STATE := IN_DELETE;
              READ (PERS_DEF.THE_PERSONS, PERSON,
                    PERS_DEF.PERSON_LIST_MNG.CURRENT);
              ENCODE_PERSON;
            when 22 =>
              -- Valid
              if STATE /= IN_DELETE then
                CURSOR_FIELD := FIRST_FIELD;
                loop
                  CHECK_FIELD (CURSOR_FIELD, OK);
                  exit when not OK or else CURSOR_FIELD = FIRST_FIELD;
                end loop;
              else
                OK := TRUE;
              end if;
              if OK then
                if STATE = IN_CREATE then
                  -- In create : insert person in list (uniq)
                  begin
                    PERS_MNG.INSERT (PERS_DEF.THE_PERSONS, PERSON);
                    BUILD_LIST;
                  exception
                    when others =>
                      CURSOR_FIELD := FIRST_FIELD;
                      OK := FALSE;
                  end;
                elsif STATE = IN_EDIT then
                  -- In edit : update person in list
                  MODIFY (PERS_DEF.THE_PERSONS, PERSON,
                          PERS_DEF.PERSON_LIST_MNG.CURRENT);

                else
                  -- In delete : delete records, delete person
                  MESU_MNG.DELETE_ALL (PERSON);
                  -- Delete all has changed persons list
                  PERS_MNG.SEARCH (PERS_DEF.THE_PERSONS, PERSON.PID, POS);
                  if GET_POSITION (PERS_DEF.THE_PERSONS)
                   = LIST_LENGTH (PERS_DEF.THE_PERSONS) then
                    DELETE (PERS_DEF.THE_PERSONS, PREV);
                  else
                    DELETE (PERS_DEF.THE_PERSONS);
                  end if;
                  BUILD_LIST;
                end if;
              end if;
              if OK then
                PERS_FIL.SAVE;
                STATE := IN_LIST;
              end if;
            when 23 =>
              -- Cancel
              STATE := IN_LIST;
            when 24 =>
              -- Compute
              OK := CHECK_COMPUTE;
              if OK then
                declare
                  REST_RATE : PERS_DEF.BPM_RANGE;
                  DELTA_RATE : MATH.REAL;
                  PERCENT : MATH.REAL;
                  use MATH;
                  use PERS_DEF;
                begin
                  REST_RATE := PERSON.TZ(1);
                  DELTA_RATE := MATH.REAL (PERSON.TZ(6) - PERSON.TZ(1));
                  -- REST_RATE + 50% .. 90% of DELTA
                  for I in 1 .. 5 loop
                    PERCENT := MATH.REAL (50 + (I - 1) * 10) / 100.0;
                    PERSON.TZ(I) :=
                       PERS_DEF.BPM_RANGE(MATH.TRUNC(DELTA_RATE * PERCENT))
                     + REST_RATE;
                  end loop;
                end;
                -- Decode name & activity, then rencode all
                CURSOR_FIELD := 11;
                CHECK_FIELD (CURSOR_FIELD, OK);
                CURSOR_FIELD := 13;
                CHECK_FIELD (CURSOR_FIELD, OK);
                ENCODE_PERSON;

                CURSOR_FIELD := 16;
                CURSOR_COL := 0;
              end if;
            when others =>
              null;
          end case;
      end case;

    end loop;

    AFPX.LINE_LIST_MNG.DELETE_LIST (AFPX.LINE_LIST);

  end LIST;

end PERS_LIS;
