with TEXT_HANDLER, ARGUMENT, MY_IO, UPPER_CHAR;
use  MY_IO;

procedure T_ARG is

  subtype REP_KEY_RANGE is CHARACTER;
  REP_KEY : REP_KEY_RANGE;

  OCC : NATURAL;

  KEY : TEXT_HANDLER.TEXT(ARGUMENT.MAX_LEN_ARG);
  ARG : TEXT_HANDLER.TEXT(ARGUMENT.MAX_LEN_ARG);

  POS : NATURAL;

  PROG_PATH : STRING (1 .. ARGUMENT.MAX_LEN_ARG);
  PROG_NAME : STRING (1 .. ARGUMENT.MAX_LEN_ARG);
  LEN : NATURAL;

  procedure GET_TXT (TXT : in out TEXT_HANDLER.TEXT) is
    STR : STRING (1 .. TXT.MAX_LEN);
    LEN : NATURAL;
  begin
    SKIP_LINE;
    GET_LINE (STR, LEN);
    TEXT_HANDLER.SET (TXT, STR(1..LEN));
  exception
    when others => raise CONSTRAINT_ERROR;
  end GET_TXT;


begin
  ARGUMENT.GET_PROGRAM_PATH (PROG_PATH, LEN);
  PUT ('>' & PROG_PATH (1 .. LEN) & "< >");
  ARGUMENT.GET_PROGRAM_NAME (PROG_NAME, LEN);
  PUT_LINE (PROG_NAME (1 .. LEN) & '<');

  PUT_LINE (NATURAL'IMAGE(ARGUMENT.GET_NBRE_ARG) & " arguments.");
  loop
    loop
      begin
        PUT_LINE ("E : Exit");
        PUT_LINE ("K : Key");
        PUT_LINE ("Y : anY_key");
        PUT_LINE ("N : Not_key");
        PUT_LINE ("A : Any_arg");
        PUT ("Enter the letter of your choice : ");
        GET (REP_KEY);
        REP_KEY := UPPER_CHAR(REP_KEY);
        exit when REP_KEY = 'E'
        or else REP_KEY = 'K'
        or else REP_KEY = 'Y'
        or else REP_KEY = 'N'
        or else REP_KEY = 'A';
      exception
        when others => null;
      end;
    end loop;

    case REP_KEY is
      when 'E' =>
        exit;
      when 'K' =>
        loop
          begin
            PUT ("Enter KEY ? ");
            GET_TXT (KEY);
            exit;
          exception
            when others => null;
          end;
        end loop;
      when 'Y' =>
        TEXT_HANDLER.SET (KEY, ARGUMENT.ANY_KEY);
      when 'N' =>
        TEXT_HANDLER.SET (KEY, ARGUMENT.NOT_KEY);
      when 'A' =>
        TEXT_HANDLER.SET (KEY, ARGUMENT.ANY_ARG);
      when others =>
        null;
    end case;


    loop
      begin
        PUT ("Enter OCCURENCE ? ");
        GET (OCC);
        exit;
      exception
        when others => null;
      end;
    end loop;
    begin
      ARGUMENT.GET_PARAM_AND_POS (ARG, POS, OCC, TEXT_HANDLER.VALUE(KEY));
      PUT_LINE ("Argument : >" & TEXT_HANDLER.VALUE(ARG) & "<");
      for I in 1 .. TEXT_HANDLER.LENGTH(ARG) loop
        PUT_LINE ("Char ->" & TEXT_HANDLER.VALUE(ARG)(I) & "< " &
         INTEGER'IMAGE (CHARACTER'POS(TEXT_HANDLER.VALUE(ARG)(I)) ) );
      end loop;

      PUT (" found at position "); PUT_LINE (POS, 3);
    exception
      when ARGUMENT.ARGUMENT_NOT_FOUND =>
        PUT_LINE ("Exception ARGUMENT_NOT_FOUND raised.");
      when ARGUMENT.ARGUMENT_TOO_LONG =>
        PUT_LINE ("Exception ARGUMENT_TOO_LONG raised.");
      when others =>
        PUT_LINE ("Exception raised.");
    end;
    NEW_LINE;
  end loop;
end T_ARG;
