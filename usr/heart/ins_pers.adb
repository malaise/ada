with TEXT_HANDLER, ARGUMENT, MY_IO, NORMAL;
with PERS_DEF, PERS_FIL, PERS_MNG;

procedure INS_PERS is
  PERSON, GOT_PERSON : PERS_DEF.PERSON_REC;
  NN, NA, NT : POSITIVE;
  NAME_TXT : TEXT_HANDLER.TEXT (PERSON.NAME'LENGTH);
  ACTI_TXT : TEXT_HANDLER.TEXT (PERSON.ACTIVITY'LENGTH);
  POS : NATURAL;
  C : CHARACTER;
  LIST_LENGTH : NATURAL;

  procedure PUT (PERSON : in PERS_DEF.PERSON_REC;
                 DISPLAY_PID : in BOOLEAN := FALSE) is
  begin
    MY_IO.PUT ("NAME: >" & PERSON.NAME & "<");
    MY_IO.PUT ("  ACTIVITY: >" & PERSON.ACTIVITY & "<");
    if DISPLAY_PID then
      MY_IO.PUT_LINE ("  PID: " & NORMAL(INTEGER(PERSON.PID), 3) );
    else
      MY_IO.NEW_LINE;
    end if;
    MY_IO.PUT (" Training Zones: ");
    for I in PERSON.TZ'RANGE loop
      MY_IO.PUT (NORMAL(INTEGER(PERSON.TZ(I)), 4));
    end loop;
    MY_IO.NEW_LINE;
  end PUT;

begin

  if ARGUMENT.GET_NBRE_ARG = 1 and then ARGUMENT.GET_PARAMETER = "-l" then
    -- Load list
    PERS_FIL.LOAD;
    if PERS_DEF.PERSON_LIST_MNG.IS_EMPTY (PERS_DEF.THE_PERSONS) then
      MY_IO.PUT_LINE ("The list is empty.");
    else
      LIST_LENGTH :=
       PERS_DEF.PERSON_LIST_MNG.LIST_LENGTH (PERS_DEF.THE_PERSONS);
      for I in 1 .. LIST_LENGTH loop
        if I /= LIST_LENGTH then
          PERS_DEF.PERSON_LIST_MNG.READ (PERS_DEF.THE_PERSONS, PERSON);
        else
          -- Do not move after reading last person
          PERS_DEF.PERSON_LIST_MNG.READ (PERS_DEF.THE_PERSONS, PERSON,
           PERS_DEF.PERSON_LIST_MNG.CURRENT);
        end if;
        PUT(PERSON, TRUE);
      end loop;
    end if;
    MY_IO.NEW_LINE;
    MY_IO.PUT_LINE ("Done.");
    return;
  end if;

  -- Parse arguments in person record
  NN := ARGUMENT.GET_POSITION (PARAM_KEY => "n");
  if NN /= 1 then
    raise CONSTRAINT_ERROR;
  end if;
  NA := ARGUMENT.GET_POSITION (PARAM_KEY => "a");
  NT := ARGUMENT.GET_POSITION (PARAM_KEY => "t");
  if      ARGUMENT.GET_PARAMETER (PARAM_KEY => "n") /= ""
  or else ARGUMENT.GET_PARAMETER (PARAM_KEY => "a") /= ""
  or else ARGUMENT.GET_PARAMETER (PARAM_KEY => "t") /= "" then
    raise CONSTRAINT_ERROR;
  end if;

  if NT < NA or else NT /= ARGUMENT.GET_NBRE_ARG - PERSON.TZ'LENGTH then
    raise CONSTRAINT_ERROR;
  end if;
  for I in 2 .. NA - 1 loop
    TEXT_HANDLER.APPEND (NAME_TXT, STRING'(ARGUMENT.GET_PARAMETER (I)));
    if I /= NA - 1 then
      TEXT_HANDLER.APPEND (NAME_TXT, ' ');
    end if;
  end loop;
  PERSON.NAME(1 .. TEXT_HANDLER.LENGTH(NAME_TXT)) := TEXT_HANDLER.VALUE (NAME_TXT);
  for I in NA + 1 .. NT - 1 loop
    TEXT_HANDLER.APPEND (ACTI_TXT, STRING'(ARGUMENT.GET_PARAMETER (I)));
    if I /= NT - 1 then
      TEXT_HANDLER.APPEND (ACTI_TXT, ' ');
    end if;
  end loop;
  PERSON.ACTIVITY(1 .. TEXT_HANDLER.LENGTH(ACTI_TXT)) := TEXT_HANDLER.VALUE (ACTI_TXT);
  for I in NT + 1 .. ARGUMENT.GET_NBRE_ARG loop
    PERSON.TZ(I-NT) := PERS_DEF.BPM_RANGE'VALUE(ARGUMENT.GET_PARAMETER(I));
  end loop;

  -- Load list
  PERS_FIL.LOAD;
  -- Check wether this person exists
  PERS_MNG.SEARCH (PERS_DEF.THE_PERSONS, PERSON.NAME, PERSON.ACTIVITY, POS);
  if POS = 0 then
    -- Display and ask confirm
    PUT (PERSON);
    MY_IO.PUT ("This person will be inserted. OK? (Y/N) : ");
    MY_IO.GET (C);
    if C /= 'Y' and then C /= 'y' then
      MY_IO.PUT_LINE ("Aborted");
      return;
    end if;
    -- Insert
    PERS_MNG.INSERT (PERS_DEF.THE_PERSONS, PERSON);
    MY_IO.NEW_LINE;
    MY_IO.PUT_LINE ("Done. PID is " & NORMAL(INTEGER(PERSON.PID), 3) );
  else
    PERS_DEF.PERSON_LIST_MNG.READ (PERS_DEF.THE_PERSONS, GOT_PERSON,
     PERS_DEF.PERSON_LIST_MNG.CURRENT);
    PERSON.PID := GOT_PERSON.PID;
    MY_IO.PUT_LINE ("The person in list");
    PUT (GOT_PERSON, TRUE);
    MY_IO.NEW_LINE;
    MY_IO.PUT_LINE ("Will be replaced by");
    PUT (PERSON, TRUE);
    MY_IO.PUT ("OK? (Y/N) : ");
    MY_IO.GET (C);
    if C /= 'Y' and then C /= 'y' then
      MY_IO.PUT_LINE ("Aborted");
      return;
    end if;
    PERS_DEF.PERSON_LIST_MNG.MODIFY (PERS_DEF.THE_PERSONS, PERSON,
     PERS_DEF.PERSON_LIST_MNG.CURRENT);
    MY_IO.NEW_LINE;
    MY_IO.PUT_LINE ("Done.");
  end if;
  -- Save list
  PERS_FIL.SAVE;
  -- OK. Display PID.

exception
  when others =>
    MY_IO.PUT_LINE ("USAGE : " & ARGUMENT.GET_PROGRAM_NAME
              & " -n <person name> -a <activity> -t <6 training zones>");
    MY_IO.PUT_LINE (" or   : " & ARGUMENT.GET_PROGRAM_NAME & " -l");
    MY_IO.PUT_LINE ("Each training zone must be between 0 and 250");
    raise;
end INS_PERS;