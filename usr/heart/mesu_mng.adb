with CON_IO, AFPX, NORMAL, PERPET, DIR_MNG;
with MESU_EDI, PERS_MNG, MESU_DEF, MESU_SEL, MESU_NAM, PERS_LIS, MESU_FIL,
     PERS_DEF, MESU_PRT, MESU_GRA;
use AFPX;
package body MESU_MNG is

  procedure LIST_MESURES (NB_MONTH : in STR_MNG.OFFSET_RANGE) is
    PERS_EMPTY   : BOOLEAN;
    LIST_EMPTY   : BOOLEAN;
    ALLOW_UNDO   : BOOLEAN;
    ALLOW_DRAW   : BOOLEAN;
    CURSOR_FIELD : AFPX.ABSOLUTE_FIELD_RANGE;
    CURSOR_COL   : CON_IO.COL_RANGE;
    PTG_RESULT   : AFPX.RESULT_REC;
    OK           : BOOLEAN;
    CRITERIA     : MESU_SEL.CRITERIA_REC;
    LINE         : AFPX.LINE_REC;
    FILE_NAME    : MESU_NAM.FILE_NAME_STR;
    P_FILE_NAME  : MESU_NAM.FILE_NAME_STR;
    EXIT_PROGRAM : BOOLEAN;
    CURRENT_DATE : STR_MNG.DATE_STR_REC;
    REDISPLAY    : BOOLEAN;


    -- Check a field
    procedure CHECK_FIELD (CURRENT_FIELD : in out AFPX.ABSOLUTE_FIELD_RANGE;
                           FOR_VALID : in BOOLEAN;
                           OK : out BOOLEAN) is
      LOCOK : BOOLEAN;
      DATE_AFT_R, DATE_BEF_R : STR_MNG.DATE_STR_REC;
      POS_PERS : INTEGER;
      PERSON : PERS_DEF.PERSON_REC;
      DATE_AFT, DATE_BEF : MESU_DEF.DATE_STR;
    begin
      case CURRENT_FIELD is

        when 07 | 08 =>
          -- In name or activity
          -- Expand name & activity
          PERSON.NAME     := AFPX.DECODE_FIELD (07, 00);
          PERSON.ACTIVITY := AFPX.DECODE_FIELD (08, 00);

          if STR_MNG.IS_SPACES (PERSON.NAME) then
            if STR_MNG.IS_SPACES (PERSON.ACTIVITY) then
              -- Name & activity empty : ok
              CURRENT_FIELD := 09;
              LOCOK := TRUE;
              if FOR_VALID then
                CRITERIA.NAME := (others => ' ');
                CRITERIA.ACTIVITY := (others => ' ');
              end if;
            else
              -- Name emtpy but activity set : err name
              CURRENT_FIELD := 07;
              LOCOK := FALSE;
            end if;
          else
            PERS_MNG.EXPAND (PERS_DEF.THE_PERSONS,
                             PERSON.NAME, PERSON.ACTIVITY, POS_PERS);
            AFPX.ENCODE_FIELD (07, (00, 00), PERSON.NAME);
            AFPX.ENCODE_FIELD (08, (00, 00), PERSON.ACTIVITY);

            -- Set pos in case of end
            if FOR_VALID then
              if POS_PERS >= 0 then
                -- Some person found : ok
                CURRENT_FIELD := 09;
                LOCOK := TRUE;
                CRITERIA.NAME := PERSON.NAME;
                CRITERIA.ACTIVITY := PERSON.ACTIVITY;
              else
                -- Error in name
                CURRENT_FIELD := 07;
                LOCOK := FALSE;
              end if;
            else
              -- not for valid
              if POS_PERS > 0 then
                -- one person found
                CURRENT_FIELD := 09;
                LOCOK := TRUE;
              elsif POS_PERS = 0 then
                -- Several persons found : next field
                if CURRENT_FIELD = 07 then
                  CURRENT_FIELD := 08;
                else
                  CURRENT_FIELD := 09;
                end if;
                LOCOK := TRUE;
              else
                -- Error in name
                CURRENT_FIELD := 07;
                LOCOK := FALSE;
              end if;
            end if;
          end if;


        when 09 | 10 | 11 =>
          -- In date aft
          DATE_AFT_R.DAY   := AFPX.DECODE_FIELD (09, 00);
          DATE_AFT_R.MONTH := AFPX.DECODE_FIELD (10, 00);
          DATE_AFT_R.YEAR  := AFPX.DECODE_FIELD (11, 00);

          if       STR_MNG.IS_SPACES (DATE_AFT_R.DAY)
          and then STR_MNG.IS_SPACES (DATE_AFT_R.MONTH)
          and then STR_MNG.IS_SPACES (DATE_AFT_R.YEAR) then
            DATE_AFT := (others => ' ');
            CURRENT_FIELD := 12;
            LOCOK := TRUE;
          else
            CURRENT_FIELD := 09;
            STR_MNG.CHECK_DATE (DATE_AFT_R, TRUE, DATE_AFT, LOCOK);
            if LOCOK then
              CURRENT_FIELD := 12;
              STR_MNG.TO_REC (DATE_AFT, DATE_AFT_R);
              AFPX.ENCODE_FIELD (09, (00, 00), DATE_AFT_R.DAY);
              AFPX.ENCODE_FIELD (10, (00, 00), DATE_AFT_R.MONTH);
              AFPX.ENCODE_FIELD (11, (00, 00), DATE_AFT_R.YEAR);
            end if;
          end if;
          if LOCOK and then FOR_VALID then
            CRITERIA.DATE_AFT := DATE_AFT;
          end if;

        when 12 | 13 | 14 =>
          -- In date bef
          DATE_BEF_R.DAY   := AFPX.DECODE_FIELD (12, 00);
          DATE_BEF_R.MONTH := AFPX.DECODE_FIELD (13, 00);
          DATE_BEF_R.YEAR  := AFPX.DECODE_FIELD (14, 00);

          if       STR_MNG.IS_SPACES (DATE_BEF_R.DAY)
          and then STR_MNG.IS_SPACES (DATE_BEF_R.MONTH)
          and then STR_MNG.IS_SPACES (DATE_BEF_R.YEAR) then
            DATE_BEF := (others => ' ');
            CURRENT_FIELD := 07;
            LOCOK := TRUE;
          else
            CURRENT_FIELD := 12;
            STR_MNG.CHECK_DATE (DATE_BEF_R, TRUE, DATE_BEF, LOCOK);
            if LOCOK then
              CURRENT_FIELD := 07;
              STR_MNG.TO_REC (DATE_BEF, DATE_BEF_R);
              AFPX.ENCODE_FIELD (12, (00, 00), DATE_BEF_R.DAY);
              AFPX.ENCODE_FIELD (13, (00, 00), DATE_BEF_R.MONTH);
              AFPX.ENCODE_FIELD (14, (00, 00), DATE_BEF_R.YEAR);
            end if;
          end if;
          if LOCOK and then FOR_VALID then
            CRITERIA.DATE_BEF := DATE_BEF;
          end if;

        when others =>
          null;
      end case;

      CURSOR_COL := 0;
      OK := LOCOK;

    end CHECK_FIELD;

  begin
    AFPX.USE_DESCRIPTOR(1);
    CURSOR_FIELD := 07;
    CURSOR_COL := 0;
    MESU_SEL.LOAD;
    AFPX.UPDATE_LIST (AFPX.CENTER);

    if NB_MONTH /= 0 then
      STR_MNG.CURRENT_DATE_REC (CURRENT_DATE, NB_MONTH);
      if AFPX.LINE_LIST_MNG.LIST_LENGTH(AFPX.LINE_LIST) = 0 then
        -- List empty : Set Aft to current date - offset
        AFPX.ENCODE_FIELD (09, (00, 00), CURRENT_DATE.DAY);
        AFPX.ENCODE_FIELD (10, (00, 00), CURRENT_DATE.MONTH);
        AFPX.ENCODE_FIELD (11, (00, 00), CURRENT_DATE.YEAR);
      else
        -- List not empty : Set Bef to current date - offset
        AFPX.ENCODE_FIELD (12, (00, 00), CURRENT_DATE.DAY);
        AFPX.ENCODE_FIELD (13, (00, 00), CURRENT_DATE.MONTH);
        AFPX.ENCODE_FIELD (14, (00, 00), CURRENT_DATE.YEAR);
      end if;
    end if;



    LIST:
    loop
      ALLOW_UNDO := FALSE;
      REDISPLAY := TRUE;

      PTG:
      loop
        PERS_EMPTY := PERS_DEF.PERSON_LIST_MNG.IS_EMPTY (PERS_DEF.THE_PERSONS);
        LIST_EMPTY := AFPX.LINE_LIST_MNG.LIST_LENGTH(AFPX.LINE_LIST) = 0;
        ALLOW_DRAW := not PERS_EMPTY and then
                      not LIST_EMPTY and then
                      AFPX.LINE_LIST_MNG.LIST_LENGTH(AFPX.LINE_LIST)
                            <= MESU_GRA.MAX_NB_MESURE;
        ALLOW_UNDO := ALLOW_UNDO and then not PERS_EMPTY;
        -- Tittles
        AFPX.SET_FIELD_ACTIVATION (01, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (03, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (04, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (05, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (06, not PERS_EMPTY);
        -- Pers, date
        AFPX.SET_FIELD_ACTIVATION (07, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (08, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (09, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (10, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (11, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (12, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (13, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (14, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (15, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (16, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (17, ALLOW_UNDO);
        -- Buttons
        AFPX.SET_FIELD_ACTIVATION (22, not PERS_EMPTY and then not LIST_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (23, ALLOW_DRAW);
        AFPX.SET_FIELD_ACTIVATION (24, not PERS_EMPTY and then not LIST_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (25, not PERS_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (26, not PERS_EMPTY and then not LIST_EMPTY);
        AFPX.SET_FIELD_ACTIVATION (27, not PERS_EMPTY and then not LIST_EMPTY);

        AFPX.ENCODE_FIELD (20, (0, 0),
          NORMAL(AFPX.LINE_LIST_MNG.LIST_LENGTH(AFPX.LINE_LIST), 5) );

        AFPX.ENCODE_FIELD (02, (00, 00), STR_MNG.CURRENT_DATE_PRINTED);
        AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
        REDISPLAY := FALSE;

        case PTG_RESULT.EVENT is
          when REFRESH =>
            REDISPLAY := TRUE;
          when FD_EVENT | AFPX.TIMER_EVENT =>
            null;
          when KEYBOARD =>

            case PTG_RESULT.KEYBOARD_KEY is
              when RETURN_KEY =>
                -- Check field and go to next if OK
                CHECK_FIELD (CURSOR_FIELD, FALSE, OK);
              when ESCAPE_KEY =>
                -- Clear current field
                if CURSOR_FIELD = 07  then
                  AFPX.CLEAR_FIELD (07);
                  AFPX.CLEAR_FIELD (08);
                  CURSOR_FIELD := 07;
                elsif CURSOR_FIELD = 08 then
                  AFPX.CLEAR_FIELD (08);
                elsif CURSOR_FIELD = 09 or else CURSOR_FIELD = 10
                or else CURSOR_FIELD = 11 then
                  AFPX.CLEAR_FIELD (09);
                  AFPX.CLEAR_FIELD (10);
                  AFPX.CLEAR_FIELD (11);
                  CURSOR_FIELD := 09;
                elsif CURSOR_FIELD = 12 or else CURSOR_FIELD = 13
                or else CURSOR_FIELD = 14 then
                  AFPX.CLEAR_FIELD (12);
                  AFPX.CLEAR_FIELD (13);
                  AFPX.CLEAR_FIELD (14);
                  CURSOR_FIELD := 12;
                else
                  AFPX.CLEAR_FIELD (CURSOR_FIELD);
                end if;
                CURSOR_COL := 0;
              when BREAK_KEY =>
                exit LIST;
            end case;

          when MOUSE_BUTTON =>

            if PTG_RESULT.FIELD_NO = 15 or else PTG_RESULT.FIELD_NO = 16 then
              -- Add/Rem selec : check all fields one by one
              CURSOR_FIELD := 07;
              loop
                CHECK_FIELD (CURSOR_FIELD, TRUE, OK);
                exit when not OK or else CURSOR_FIELD = 07;
              end loop;
              if OK then
                if PTG_RESULT.FIELD_NO = 15 then
                  MESU_SEL.ADD_SELECTION (CRITERIA);
                else
                  MESU_SEL.REM_SELECTION (CRITERIA);
                end if;
                ALLOW_UNDO := TRUE;
              end if;
            elsif PTG_RESULT.FIELD_NO = 17 then
              -- Undo
              MESU_SEL.UNDO;
              ALLOW_UNDO := FALSE;
            elsif PTG_RESULT.FIELD_NO = 18 then
              -- Activiy DB
              MESU_SEL.SAVE;
              PERS_LIS.LIST (EXIT_PROGRAM);
              MESU_SEL.LOAD;
              AFPX.UPDATE_LIST(AFPX.CENTER);
              if EXIT_PROGRAM then
                exit LIST;
              end if;
              exit PTG;
            elsif PTG_RESULT.FIELD_NO = 19 then
              -- Exit
              exit LIST;
            elsif PTG_RESULT.FIELD_NO = 22 then
              -- Unselect
              AFPX.LINE_LIST_MNG.READ (AFPX.LINE_LIST, LINE,
                                       AFPX.LINE_LIST_MNG.CURRENT);
              STR_MNG.FORMAT_LIST_TO_MESURE (LINE, FILE_NAME);
              MESU_SEL.REM_SELECTION (FILE_NAME);
              ALLOW_UNDO := TRUE;
            elsif PTG_RESULT.FIELD_NO = 23 then
              -- Draw
              MESU_GRA.GRAPHIC(EXIT_PROGRAM);
              if EXIT_PROGRAM then
                exit LIST;
              end if;
              exit PTG;
            elsif PTG_RESULT.FIELD_NO = 24 then
              -- Print
              MESU_PRT.PRINT;
              exit PTG;
            elsif PTG_RESULT.FIELD_NO = 25 then
              -- Create
              FILE_NAME := (others => ' ');
              MESU_EDI.EDIT (FILE_NAME, EXIT_PROGRAM);
              if EXIT_PROGRAM then
                exit LIST;
              end if;
              if not STR_MNG.IS_SPACES (FILE_NAME) then
                MESU_SEL.ADD_SELECTION (FILE_NAME);
              end if;
              -- Edit screen called
              exit PTG;
            elsif (PTG_RESULT.FIELD_NO = 0
                   or else PTG_RESULT.FIELD_NO = 26) then
              -- Edit
              AFPX.LINE_LIST_MNG.READ (AFPX.LINE_LIST, LINE,
                                       AFPX.LINE_LIST_MNG.CURRENT);
              STR_MNG.FORMAT_LIST_TO_MESURE (LINE, FILE_NAME);

              -- Edit
              P_FILE_NAME := FILE_NAME;
              MESU_EDI.EDIT (FILE_NAME, EXIT_PROGRAM);
              if EXIT_PROGRAM then
                exit LIST;
              end if;
              if not STR_MNG.IS_SPACES (FILE_NAME) then
                MESU_SEL.REM_SELECTION (LINE);
                MESU_SEL.ADD_SELECTION (FILE_NAME);
              end if;
              -- Edit screen called
              exit PTG;
            elsif PTG_RESULT.FIELD_NO = 27 then
              -- Delete
              AFPX.LINE_LIST_MNG.READ (AFPX.LINE_LIST, LINE,
                                       AFPX.LINE_LIST_MNG.CURRENT);
              STR_MNG.FORMAT_LIST_TO_MESURE (LINE, FILE_NAME);

              -- Delete
              P_FILE_NAME := FILE_NAME;
              MESU_EDI.DELETE (FILE_NAME, EXIT_PROGRAM);

              if EXIT_PROGRAM then
                exit LIST;
              end if;
              if not STR_MNG.IS_SPACES (FILE_NAME) then
                MESU_SEL.REM_SELECTION (LINE);
              end if;
              -- Edit screen called
              exit PTG;
            end if; -- Test of buttons
        end case;

      end loop PTG;
      -- Another screen called
      AFPX.USE_DESCRIPTOR(1);
    end loop LIST;

    MESU_SEL.SAVE;
  end LIST_MESURES;

  procedure DELETE_ALL (PERSON : in PERS_DEF.PERSON_REC) is
    FILE_NAME : MESU_NAM.FILE_NAME_STR;
    THE_FILES : DIR_MNG.FILE_LIST_MNG.LIST_TYPE;
    FILE : DIR_MNG.FILE_ENTRY_REC;
  begin
    -- Build ????????.<pid>
    FILE_NAME := MESU_NAM.BUILD_FILE_NAME (PID => STR_MNG.PID_STR (PERSON.PID));
    -- Set files lin ist
    DIR_MNG.LIST_DIR (THE_FILES, "", FILE_NAME);

    if DIR_MNG.FILE_LIST_MNG.IS_EMPTY (THE_FILES) then
      -- No file
      return;
    end if;
    MESU_SEL.LOAD;

    -- Remove entries from selection, then files
    DIR_MNG.FILE_LIST_MNG.MOVE_TO (THE_FILES, DIR_MNG.FILE_LIST_MNG.NEXT,
                                   0, FALSE);
    loop
      DIR_MNG.FILE_LIST_MNG.READ (THE_FILES, FILE,
                                  DIR_MNG.FILE_LIST_MNG.CURRENT);
      FILE_NAME := FILE.NAME(1 .. FILE.LEN);
      begin
        MESU_SEL.REM_SELECTION (FILE_NAME);
      exception
        when AFPX.LINE_LIST_MNG.NOT_IN_LIST =>
          -- This file was not selected
          null;
      end;
      MESU_FIL.DELETE (FILE_NAME);
      -- Next file
      exit when DIR_MNG.FILE_LIST_MNG.GET_POSITION (THE_FILES)
              = DIR_MNG.FILE_LIST_MNG.LIST_LENGTH  (THE_FILES);

      DIR_MNG.FILE_LIST_MNG.MOVE_TO (THE_FILES);
    end loop;

    DIR_MNG.FILE_LIST_MNG.DELETE_LIST (THE_FILES);
    MESU_SEL.SAVE;
  end DELETE_ALL;

end MESU_MNG;

