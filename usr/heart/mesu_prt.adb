with TEXT_IO, NORMAL, DOS, AFPX, SYS_CALLS;
with STR_MNG, MESU_FIL, PERS_DEF, MESU_DEF, MESU_NAM, PERS_MNG;
package body MESU_PRT is

  PRINTER_NAME : constant STRING := "PRN";
  PRINTER_COMMAND : constant STRING := "heart_print";
  PRINTER      : TEXT_IO.FILE_TYPE;

  procedure PRINT_REC (PERSON : in PERS_DEF.PERSON_REC;
                       MESURE : in MESU_DEF.MESURE_REC) is
    LAST_OF_LINE : BOOLEAN;
    use TEXT_IO;
    use PERS_DEF;
  begin
    if not IS_OPEN (PRINTER) then
      CREATE (PRINTER, OUT_FILE, PRINTER_NAME);
    end if;
    PUT_LINE (PRINTER, "Person: " & PERSON.NAME & " " & PERSON.ACTIVITY
                      & "  Date: " & STR_MNG.TO_PRINTED_STR(MESURE.DATE));
    PUT (PRINTER, "Comment: " & MESURE.COMMENT
                & "  Delta: " & NORMAL(INTEGER(MESURE.SAMPLING_DELTA), 3)
                & "  TZ: ");
    for I in PERS_DEF.PERSON_TZ_ARRAY'RANGE loop
      PUT (PRINTER, STR_MNG.TO_STR(MESURE.TZ(I)) & " ");
    end loop;
    NEW_LINE (PRINTER);
    LAST_OF_LINE := FALSE;
    for I in MESU_DEF.SAMPLE_NB_RANGE loop
      exit when MESURE.SAMPLES(I) = PERS_DEF.BPM_RANGE'FIRST;
      PUT (PRINTER, STR_MNG.TO_STR(MESURE.SAMPLES(I)));
      LAST_OF_LINE := I mod 20 = 0;
      if LAST_OF_LINE then
        -- After last of row
        NEW_LINE (PRINTER);
      else
        PUT (PRINTER, " ");
      end if;
    end loop;
    if not LAST_OF_LINE then
      NEW_LINE (PRINTER);
    end if;
  end PRINT_REC;

  procedure PRINT_SEPARATOR is
  begin
    TEXT_IO.PUT_LINE (PRINTER,
"- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -");
  end PRINT_SEPARATOR;

  procedure FORM_FEED is
  begin
    TEXT_IO.NEW_PAGE(PRINTER);
    TEXT_IO.CLOSE(PRINTER);
  end FORM_FEED;

  procedure PRINT is
    SAVED_POS : NATURAL;
    LINE      : AFPX.LINE_REC;
    FILE_NAME : MESU_NAM.FILE_NAME_STR;
    DATE_S    : MESU_NAM.FILE_DATE_STR;
    NO_S      : MESU_NAM.FILE_NO_STR;
    PID_S     : MESU_NAM.FILE_PID_STR;
    POS_PERS  : NATURAL;
    PERSON    : PERS_DEF.PERSON_REC;
    MESURE    : MESU_DEF.MESURE_REC;
    DUMMY     : INTEGER;

    use AFPX.LINE_LIST_MNG;
  begin
    -- List is not empty
    SAVED_POS := GET_POSITION (AFPX.LINE_LIST);

    -- for each in list
    MOVE_TO (AFPX.LINE_LIST, NEXT, 0, FALSE);

    AFPX.USE_DESCRIPTOR (4);
    AFPX.PUT;

    PRINT:
    loop
      -- Get line, file_name, split
      READ (AFPX.LINE_LIST, LINE, CURRENT);
      STR_MNG.FORMAT_LIST_TO_MESURE (LINE, FILE_NAME);
      MESU_NAM.SPLIT_FILE_NAME (FILE_NAME, DATE_S, NO_S, PID_S);
      -- Get person
      PERS_MNG.SEARCH (PERS_DEF.THE_PERSONS, PERS_DEF.PID_RANGE'VALUE(PID_S),
                       POS_PERS);
      PERS_DEF.PERSON_LIST_MNG.READ (PERS_DEF.THE_PERSONS, PERSON,
                                     PERS_DEF.PERSON_LIST_MNG.CURRENT);
      -- Get mesure
      MESURE := MESU_FIL.LOAD (FILE_NAME);

      PRINT_REC (PERSON, MESURE);

      -- Next line except if list empty or end of list
      exit when IS_EMPTY (AFPX.LINE_LIST) or else
                GET_POSITION (AFPX.LINE_LIST) = LIST_LENGTH (AFPX.LINE_LIST);

      MOVE_TO (AFPX.LINE_LIST);
      PRINT_SEPARATOR;
    end loop PRINT;

    FORM_FEED;

    -- Print
    DUMMY := SYS_CALLS.CALL_SYSTEM(PRINTER_COMMAND & " " & PRINTER_NAME);

    -- Restore pos
    MOVE_TO (AFPX.LINE_LIST, NEXT, SAVED_POS - 1, FALSE);

  exception
    when others =>
      MOVE_TO (AFPX.LINE_LIST, NEXT, SAVED_POS - 1, FALSE);
  end PRINT;

end MESU_PRT;
