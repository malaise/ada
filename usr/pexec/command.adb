with MY_IO, TEXT_HANDLER, ARGUMENT;
package body COMMAND is

  -- Pexec options definitions
  NBRE_MAX_OPT : constant := 6;
  subtype INDEX_OPT is POSITIVE range 1 .. NBRE_MAX_OPT;
  OPT_KEY : constant array (INDEX_OPT) of CHARACTER
          := ('a', 'd', 'c', 'f', 'l', 'i');

  -- Parse already called...
  PARSED : BOOLEAN := FALSE;

  -- Result of parsing : set NBRE_PARAM and PARAM array
  MAX_NBRE_PARAM : constant := 16;
  MAX_LEN_PARAM : constant := 132;

  subtype T_PARAM is TEXT_HANDLER.TEXT (MAX_LEN_PARAM);

  NBRE_PARAM : NATURAL;
  PARAM : array (1 .. MAX_NBRE_PARAM) of T_PARAM;

  procedure PRINT_USAGE is
  begin
    MY_IO.PUT_LINE ("Usage : pexec [options] command [{;command}]");
    MY_IO.PUT_LINE (" options : -[a][d][c][f][l][i]");
    MY_IO.PUT_LINE ("  a for do not print actions.");
    MY_IO.PUT_LINE ("  d for do not print name of each dir.");
    MY_IO.PUT_LINE ("  c for don't do in current dir.");
    MY_IO.PUT_LINE ("  f for stop after 1st level of sub dir.");
    MY_IO.PUT_LINE ("  l for do in leaves only (dirs with no subdir).");
    MY_IO.PUT_LINE ("  i for ignore command errors.");
  end PRINT_USAGE;

  procedure PARSE (
   NO_ACTION,      NO_NAME_OF_DIR,
   NOT_IN_CURRENT, FIRST_LEVEL_ONLY, LEAVES_ONLY,
   NO_STOP_ON_ERROR : out BOOLEAN) is

    -- Is 1st argument a pexec option
    PEXEC_OPTIONS : BOOLEAN;

    -- To get arguments
    FIRST_POS : NATURAL;
    ARG : TEXT_HANDLER.TEXT (MAX_LEN_PARAM);
    -- String copy of ARG
    STR : STRING (1 .. MAX_LEN_PARAM);
    LEN : POSITIVE;

    -- POS is index of first available position in STR
    --  string concatenation of pexec arguments which are not pexec option
    POS : POSITIVE;

    -- Local copies of out parameters
    LA, LD, LC, LF, LL, LI : BOOLEAN;


  begin
    -- Set out default values
    LA := FALSE;
    LD := FALSE;
    LC := FALSE;
    LF := FALSE;
    LL := FALSE;
    LI := FALSE;
    NO_ACTION        := LA;
    NO_NAME_OF_DIR   := LD;
    NOT_IN_CURRENT   := LC;
    FIRST_LEVEL_ONLY := LF;
    LEAVES_ONLY      := LF;
    NO_STOP_ON_ERROR := LI;

    -- Check that not already parsed
    if PARSED then
      raise ALREADY_PARSED;
    end if;

    -- Search and check wether first argument is a pexec option
    --  sets the out parameters and PEXEC_OPTIONS
    declare
      -- If any error is detected in pexec option
      WRONG_PEXEC_OPT : exception;
    begin

      begin
        -- Search a key
        ARGUMENT.GET_PARAM_AND_POS (PARAMETER => ARG, POSITION => FIRST_POS,
         OCCURENCE => 1, PARAM_KEY => "");
      exception
        when ARGUMENT.ARGUMENT_NOT_FOUND =>
          raise WRONG_PEXEC_OPT;
      end;

      -- Key must be first, not empty and not more than NBRE_MAX_OPT
      if FIRST_POS /= 1 or else
       TEXT_HANDLER.LENGTH(ARG) = 0 or else
       TEXT_HANDLER.LENGTH(ARG) > NBRE_MAX_OPT then
        raise WRONG_PEXEC_OPT;
      end if;

      -- A key at first. May be an option. Store as string.
      LEN := TEXT_HANDLER.LENGTH(ARG);
      STR (1 .. LEN) := TEXT_HANDLER.VALUE(ARG);

      -- Each option must appear once or not.
      -- check that any letter is a pexec option
      for I in 1 .. LEN loop
        -- Check a letter
        declare
          OK_SO_FAR : BOOLEAN := FALSE;
        begin
          -- Check that STR(I) is a pexec option
          for J in INDEX_OPT loop
            if STR(I) = OPT_KEY(J) then
              -- Character is found within pexec options
              OK_SO_FAR := TRUE;
              exit;
            end if;
          end loop;
          if not OK_SO_FAR then raise WRONG_PEXEC_OPT; end if;
          -- Current letter is an option. Must appear once.
          if I /= LEN then
            for J in I+1 .. LEN loop
              if STR(J) = STR(I) then
                -- Character appears twice
                OK_SO_FAR := FALSE;
                exit;
              end if;
            end loop;
          end if;
          if not OK_SO_FAR then raise WRONG_PEXEC_OPT; end if;
        end;

        -- set out params
        if    STR(I) = OPT_KEY(1) then LA := TRUE;
        elsif STR(I) = OPT_KEY(2) then LD := TRUE;
        elsif STR(I) = OPT_KEY(3) then LC := TRUE;
        elsif STR(I) = OPT_KEY(4) then LF := TRUE;
        elsif STR(I) = OPT_KEY(5) then LL := TRUE;
        elsif STR(I) = OPT_KEY(6) then LI := TRUE;
        end if;

      end loop;

      PEXEC_OPTIONS := TRUE;

    exception
      when WRONG_PEXEC_OPT =>
        PEXEC_OPTIONS := FALSE;
        -- not a pexec option
    end;

    -- check that there is at least one argument remaining
    if PEXEC_OPTIONS then
      if ARGUMENT.GET_NBRE_ARG < 2 then raise NO_COMMAND; end if;
    else
      if ARGUMENT.GET_NBRE_ARG < 1 then raise NO_COMMAND; end if;
    end if;


    -- build all the command line : Concatenate all commands in str
    declare
      -- Fist no-pexec_option argument
      FIRST_COM : POSITIVE range 1 .. 2;
    begin
      -- Set Fist no-pexec_option argument
      if PEXEC_OPTIONS then
        FIRST_COM := 2;
      else
        FIRST_COM := 1;
      end if;

      -- Concatenate all the command line in STR
      POS := 1;
      for I in FIRST_COM .. ARGUMENT.GET_NBRE_ARG loop
        ARGUMENT.GET_PARAMETER (PARAMETER => ARG, OCCURENCE => I);
        LEN := TEXT_HANDLER.LENGTH(ARG);
        STR (POS .. POS + LEN) := TEXT_HANDLER.VALUE(ARG) & ' ';
        POS := POS + LEN + 1;
      end loop;
      -- POS is index of first available position
      -- It becomes index of last character
      POS := POS - 2;
      -- remove first and last "
      if STR(1) = '"' and then STR(POS) = '"' then
        STR(1..POS-2) := STR(2..POS-1);
        POS := POS - 2;
      end if;
    end;

    -- Parse (split) the command line in STR
    declare
      -- begin of current command (first char or first after ;)
      FIRST : POSITIVE := 1;
      -- end   of current command
      LAST  : NATURAL := 0;
       -- No of current command
      INDEX : NATURAL := 0;
    begin
      for I in 1 .. POS loop

        -- search separator
        if STR(I) = ';' then

          -- new argument
          if I = FIRST then
            -- first char is ';', or ";;" in string
            null;
          elsif I = FIRST+1 and then STR(I-1) = ' ' then
            -- "; ;"
            null;
          else
            -- at least one siginificant char in FIRST .. I-1
            LAST := I - 1;

            -- Skip leading and tailing spaces
            if STR(FIRST) = ' ' then
              FIRST := FIRST + 1;
            end if;
            if STR(LAST) = ' ' then
              LAST := LAST - 1;
            end if;

            -- store this
            INDEX := INDEX + 1;
            TEXT_HANDLER.SET (PARAM(INDEX),
             TEXT_HANDLER.TO_TEXT (STR(FIRST .. LAST)) );

            -- set first
            FIRST := I + 1;
          end if;

        elsif I = POS then

          -- end of STR
          LAST := I;

          -- Skip leading spaces
          if STR(FIRST) = ' ' then
            FIRST := FIRST + 1;
          end if;

          -- store this
          INDEX := INDEX + 1;
          TEXT_HANDLER.SET (PARAM(INDEX),
           TEXT_HANDLER.TO_TEXT (STR(FIRST .. LAST)));

        end if;

      end loop;

      -- done
      NBRE_PARAM := INDEX;
      PARSED := TRUE;

    end;

    -- set out values
    NO_ACTION        := LA;
    NO_NAME_OF_DIR   := LD;
    NOT_IN_CURRENT   := LC;
    FIRST_LEVEL_ONLY := LF;
    LEAVES_ONLY      := LL;
    NO_STOP_ON_ERROR := LI;

  end PARSE;



  function NBRE_COMMANDS return NATURAL is
  begin
    if not PARSED then
      raise NOT_PARSED;
    end if;
    return NBRE_PARAM;
  end NBRE_COMMANDS;

  function NTH_COMMAND (N : POSITIVE) return STRING is
  begin
    if not PARSED then
      raise NOT_PARSED;
    end if;
    if N <= NBRE_COMMANDS then
      return TEXT_HANDLER.VALUE (PARAM(N));
    else
      return "";
    end if;
  end NTH_COMMAND;

end COMMAND;
