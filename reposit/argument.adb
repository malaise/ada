with LOC_ARG;
use LOC_ARG;
package body ARGUMENT is

  KEY_PREFIX : constant CHARACTER := '-';

  PATH_SEPARATOR : constant CHARACTER := '/';

  STR : STRING (1..MAX_LEN_ARG);

  function NOT_KEY return STRING is
  begin
    return "" & ASCII.DEL;
  end NOT_KEY;

  function ANY_ARG return STRING is
  begin
    return "" & ASCII.NUL;
  end ANY_ARG;

  function ANY_KEY return STRING is
  begin
    return " ";
  end ANY_KEY;


  function GET_PARAMETER (
   OCCURENCE : in NATURAL := 1;
   PARAM_KEY : in STRING := ANY_ARG) return STRING is
    LEN : NATURAL;
  begin
    GET_PARAMETER (STR, LEN, OCCURENCE, PARAM_KEY);
    return STR (1 .. LEN);
  end GET_PARAMETER;

  procedure GET_PARAMETER (
   PARAMETER : out STRING;
   PARAM_LENGTH : out NATURAL;
   OCCURENCE : in NATURAL := 1;
   PARAM_KEY : in STRING := ANY_ARG) is
    POSITION : NATURAL;
  begin
    GET_PARAM_AND_POS (PARAMETER, PARAM_LENGTH, POSITION,
     OCCURENCE, PARAM_KEY);
  end GET_PARAMETER;

  procedure GET_PARAMETER (
   PARAMETER : in out TEXT_HANDLER.TEXT;
   OCCURENCE : in NATURAL := 1;
   PARAM_KEY : in STRING := ANY_ARG) is
    POSITION : NATURAL;
  begin
    GET_PARAM_AND_POS (PARAMETER, POSITION,
     OCCURENCE, PARAM_KEY);
  end;



  procedure GET_PARAM_AND_POS (
   PARAMETER : out STRING;
   PARAM_LENGTH : out NATURAL;
   POSITION : out NATURAL;
   OCCURENCE : in NATURAL := 1;
   PARAM_KEY : in STRING := ANY_ARG) is

    COMFORM_OCCURENCE : NATURAL := 0;
    IN_OCCURENCE : NATURAL := OCCURENCE;
    FIRST_CHAR : POSITIVE;
  begin
    -- Init result for case of error
    PARAM_LENGTH := 0;
    POSITION := 0;

    -- test if occurence is 0
    if IN_OCCURENCE=0 then
      if PARAM_KEY /= ANY_ARG and then PARAM_KEY /= NOT_KEY then
        raise ARGUMENT_NOT_FOUND;
      end if;
      if PARAMETER'LENGTH < DATA(0)'LENGTH then
        raise ARGUMENT_TOO_LONG;
      end if;
      -- affect string
      PARAM_LENGTH := DATA(0)'LENGTH;
      PARAMETER(1 .. DATA(0)'LENGTH) := DATA(0)(1..DATA(0)'LENGTH);
      POSITION := 0;
      return;
    end if;

    -- Compute 1st char of argument to return
    if PARAM_KEY=ANY_ARG or else PARAM_KEY=NOT_KEY then
      FIRST_CHAR := 1;
    elsif PARAM_KEY = ANY_KEY then
      -- any key : start after '-'
      FIRST_CHAR := 2;
    else
      -- specific key : start after -<key>
      FIRST_CHAR := PARAM_KEY'LENGTH + 2;
    end if;

    -- analyse arguments of command line,
    for I in 1 .. COUNT loop
      -- Check if argument conforms
      if PARAM_KEY = ANY_ARG then
        -- any parameter comforms
        COMFORM_OCCURENCE := COMFORM_OCCURENCE + 1;
      elsif PARAM_KEY = NOT_KEY and then DATA(I)(1) /= KEY_PREFIX then
        -- any parameter not preceeded by separator comforms
        COMFORM_OCCURENCE := COMFORM_OCCURENCE + 1;
      elsif PARAM_KEY = ANY_KEY and then DATA(I)(1) = KEY_PREFIX then
        -- any parameter preceeded by separator comforms
        COMFORM_OCCURENCE := COMFORM_OCCURENCE + 1;
      elsif DATA(I)(1) = KEY_PREFIX
       and then DATA(I)'LENGTH >= PARAM_KEY'LENGTH + 1
       and then DATA(I)(2 .. PARAM_KEY'LENGTH+1) = PARAM_KEY then
        -- Check that first char is PREFIX
        -- and that length of parameter is >= than '-'<key>
        -- and that argument after '-' matches PARAM_KEY
        COMFORM_OCCURENCE := COMFORM_OCCURENCE + 1;
      end if;

      if COMFORM_OCCURENCE = IN_OCCURENCE then
        -- Comforming occurence is found. Check length
        if PARAMETER'LENGTH < DATA(I)'LENGTH - (FIRST_CHAR-1) then
          raise ARGUMENT_TOO_LONG;
        else
          -- affect string (FIRST_CHAR..LEN)
          PARAM_LENGTH := DATA(I)'LENGTH - (FIRST_CHAR-1);
          PARAMETER(1 .. DATA(I)'LENGTH-(FIRST_CHAR-1))
           := DATA(I)(FIRST_CHAR..DATA(I)'LENGTH);
          POSITION := I;
          return;
        end if;
      end if;
      -- next argument
    end loop;
    raise ARGUMENT_NOT_FOUND;
  exception
    -- Propagate the cause of problem.
    when ARGUMENT_TOO_LONG | ARGUMENT_NOT_FOUND =>
      raise;
    when others =>
      -- In case of other problem : not found.
      raise ARGUMENT_NOT_FOUND;
  end GET_PARAM_AND_POS;

  procedure GET_PARAM_AND_POS (
   PARAMETER : in out TEXT_HANDLER.TEXT;
   POSITION : out NATURAL;
   OCCURENCE : in NATURAL := 1;
   PARAM_KEY : in STRING := ANY_ARG) is
    STR : STRING (1..PARAMETER.MAX_LEN);
    LEN : NATURAL;
  begin
    GET_PARAM_AND_POS (STR, LEN, POSITION, OCCURENCE, PARAM_KEY);
    TEXT_HANDLER.SET (PARAMETER, STR(1 .. LEN));
  end GET_PARAM_AND_POS;

 function GET_POSITION (
   OCCURENCE : in NATURAL := 1;
   PARAM_KEY : in STRING := ANY_ARG) return NATURAL is
    LEN : NATURAL;
    POS : NATURAL;
  begin
    GET_PARAM_AND_POS (STR, LEN, POS, OCCURENCE, PARAM_KEY);
    return POS;
  end GET_POSITION;

  function GET_NBRE_ARG return NATURAL is
  begin
    return COUNT;
  end GET_NBRE_ARG;

  -- Analyse of argument(0)

  -- Path of program from ARG(0) (wirh last /)
  function LAST_DELIMITER (PATH_PROG : STRING) return NATURAL is
  begin
    for I in reverse PATH_PROG'RANGE loop
      if PATH_PROG(I) = PATH_SEPARATOR then
        return I;
      end if;
    end loop;
    return 0;
  end LAST_DELIMITER;


  function GET_PROGRAM_PATH return STRING is
    LEN : NATURAL;
  begin
    -- program path
    GET_PROGRAM_PATH (STR, LEN);
    return STR (1 .. LEN);
  end GET_PROGRAM_PATH;

  procedure GET_PROGRAM_PATH (PATH : out STRING; PATH_LENGTH : out NATURAL) is
    LEN : NATURAL;
  begin
    -- program path and name
    GET_PARAMETER (STR, LEN, 0, ANY_ARG);
    LEN := LAST_DELIMITER(STR (1 .. LEN));

    PATH_LENGTH := LEN;
    PATH (1 .. LEN) := STR (1 .. LEN);
  end GET_PROGRAM_PATH;

  procedure GET_PROGRAM_PATH (PATH : in out TEXT_HANDLER.TEXT) is
  begin
    TEXT_HANDLER.SET (PATH, GET_PROGRAM_PATH);
  end GET_PROGRAM_PATH;

  -- name of program from ARGUMENT(0)
  function GET_PROGRAM_NAME return STRING is
    LEN : NATURAL;
  begin
    -- program name
    GET_PROGRAM_NAME (STR, LEN);
    return STR (1 .. LEN);
  end GET_PROGRAM_NAME;

  procedure GET_PROGRAM_NAME (NAME : out STRING;
                              NAME_LENGTH : out NATURAL) is
    LEN : NATURAL;
    START : NATURAL;
  begin
    -- program path and name
    GET_PARAMETER (STR, LEN, 0, ANY_ARG);
    START := LAST_DELIMITER(STR (1 .. LEN)) + 1;

    NAME_LENGTH := LEN - START + 1;
    NAME (1 .. LEN - START + 1) := STR (START .. LEN);
  end GET_PROGRAM_NAME;


  procedure GET_PROGRAM_NAME (NAME : in out TEXT_HANDLER.TEXT) is
  begin
    TEXT_HANDLER.SET (NAME, GET_PROGRAM_NAME);
  end GET_PROGRAM_NAME;

end ARGUMENT;

