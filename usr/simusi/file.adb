with TEXT_IO;
with TEXT_HANDLER, ARGUMENT, SYS_CALLS, GET_LINE, GET_FLOAT;
with ARG_PARSING;
package body FILE is

  package COTE_GET_LINE is new GET_LINE (
    MAX_WORD_LEN => 20,
    MAX_WORD_NB  => 4,
    MAX_LINE_LEN => 80,
    COMMENT      => '#');

  LINE : COTE_GET_LINE.LINE_ARRAY; 

  subtype LOC_COTE_RANGE is NATURAL range 0 .. MAX_COTE;
  type COTE_LINE_ARRAY is array (COTE_RANGE) of TEXT_IO.COUNT;
  COTE_LINE : COTE_LINE_ARRAY := (others => 0);

  LOAD_OK : BOOLEAN := FALSE;
  MANUFAS : MANUFA_ARRAY(1 .. MAX_COTE);
  NB_MANUFA : LOC_COTE_RANGE;
  DESIGNS : DESIGN_ARRAY(1 .. MAX_COTE);
  NB_DESIGN : LOC_COTE_RANGE;


  procedure READ_NEXT_SIGNIFICANT_LINE is
  begin
    COTE_GET_LINE.READ_NEXT_LINE;
    COTE_GET_LINE.GET_WORDS (LINE);
  end READ_NEXT_SIGNIFICANT_LINE;

  type RESULT_LIST is (OK, END_OF_FILE, FILE_ACCESS, FILE_FORMAT, FILE_LENGTH,
           LINE_NB, START_STOP, DUPLICATE, NO_COTE);

  procedure ERROR (FILENAME : in STRING; RESULT : in RESULT_LIST;
                   COTE_NO : in LOC_COTE_RANGE := 0; 
                   NO2 : in NATURAL := 0) is
  begin
    if RESULT = OK or else RESULT = END_OF_FILE then
      return;
    end if;
    SYS_CALLS.PUT_ERROR ( "Error in file " & FILENAME);
    if COTE_NO /= 0 then
      SYS_CALLS.PUT_LINE_ERROR (
       " at line " & TEXT_IO.COUNT'IMAGE(COTE_LINE(COTE_NO)));
    else
      SYS_CALLS.NEW_LINE_ERROR;
    end if;
    case RESULT is
      when OK | END_OF_FILE =>
        null;
      when FILE_ACCESS =>
        SYS_CALLS.PUT_LINE_ERROR ("File not found or not readable or empty.");
      when FILE_FORMAT =>
        SYS_CALLS.PUT_LINE_ERROR ("Wrong format.");
      when FILE_LENGTH =>
        SYS_CALLS.PUT_LINE_ERROR ("File too long.");
      when LINE_NB =>
        SYS_CALLS.PUT_LINE_ERROR ("Invalid line number.");
      when START_STOP =>
        SYS_CALLS.PUT_LINE_ERROR ("Start equal stop.");
      when DUPLICATE =>
        SYS_CALLS.PUT_LINE_ERROR ("Cote already exists at line "
          & TEXT_IO.COUNT'IMAGE(COTE_LINE(NO2)));
      when NO_COTE =>
        SYS_CALLS.PUT_LINE_ERROR ("Line " & NATURAL'IMAGE(NO2) & " has no cote");
    end case;
    raise LOAD_ERROR;
  end ERROR;

  procedure OPEN (FILENAME : in STRING) is
  begin
    COTE_GET_LINE.OPEN (FILENAME);
    COTE_GET_LINE.GET_WORDS (LINE);
  exception
    when others =>
      ERROR (FILENAME, FILE_ACCESS);
  end OPEN;
    
  procedure LOAD_COTE (KIND : in COTE_KIND; 
                       NB_COTE : in out LOC_COTE_RANGE;
                       COTE : in out COTE_REC;
                       RESULT : out RESULT_LIST) is
  begin
    -- Except for first (done in open) get new line and parse
    if NB_COTE /= 0 then
      begin
        READ_NEXT_SIGNIFICANT_LINE;
      exception
        when COTE_GET_LINE.NO_MORE_LINE =>
          RESULT := END_OF_FILE;
          return;
      end;
      COTE_GET_LINE.GET_WORDS (LINE);
    end if;
    -- Got a new cote: Check number of cotes
    if NB_COTE = MAX_COTE then
      RESULT := FILE_LENGTH;
      return;
    end if;
    NB_COTE := NB_COTE + 1;
    COTE_LINE(NB_COTE) := COTE_GET_LINE.GET_LINE_NO;
    -- Got a new cote: Check number of words
    if KIND = MANUFA then
      if COTE_GET_LINE.GET_WORD_NUMBER /= 3 then
        RESULT := FILE_FORMAT;
        return;
      end if;
    else
      if COTE_GET_LINE.GET_WORD_NUMBER /= 4 then
        RESULT := FILE_FORMAT;
        return;
      end if;
    end if;
    -- Parse line
    COTE.START := LINE_RANGE'VALUE(TEXT_HANDLER.VALUE(LINE(1)));
    COTE.STOP  := LINE_RANGE'VALUE(TEXT_HANDLER.VALUE(LINE(2)));
    if KIND = DESIGN then
      COTE.VALUE := GET_FLOAT(TEXT_HANDLER.VALUE(LINE(3)));
      COTE.INTER := GET_FLOAT(TEXT_HANDLER.VALUE(LINE(4)));
    else
      COTE.INTER := GET_FLOAT(TEXT_HANDLER.VALUE(LINE(3)));
    end if;
    -- Error if start=stop. Set start < stop
    if COTE.START = COTE.STOP then
      RESULT := START_STOP;
      return;
    elsif COTE.START > COTE.STOP then
      declare
        TMP : COTE_RANGE;
      begin
        TMP := COTE.START;
        COTE.START := COTE.STOP;
        COTE.STOP := TMP;
      end; 
    end if;
    
    RESULT := OK;
  exception
    when others =>
      RESULT := FILE_FORMAT;
  end LOAD_COTE;

  -- Load both sets of cotes from two files
  -- And checks
  procedure LOAD_COTES is
    function ARE_DUP (C1, C2 : in COTE_REC) return BOOLEAN is
    begin
      return  C1.START = C2.START and then C1.STOP = C2.STOP;
    end ARE_DUP;

    LINE_USAGE : array (LINE_RANGE) of BOOLEAN;
    RESULT : RESULT_LIST;
    MANUFA_COTE : MANUFA_COTE_REC;
    DESIGN_COTE : DESIGN_COTE_REC;
  begin
    begin
      ARG_PARSING.CHECK;
    exception
      when others =>
        SYS_CALLS.PUT_LINE_ERROR ("Error. Usage: "
          & ARGUMENT.GET_PROGRAM_NAME & " [ -v ] <specification_file> <manufacturing_file>");
        raise LOAD_ERROR;
    end;
    -- Load and check Manufas
    NB_MANUFA := 0;
    OPEN (ARG_PARSING.MANUFA_FILE_NAME);
    loop
      LOAD_COTE (MANUFA, NB_MANUFA, MANUFA_COTE, RESULT);
      exit when RESULT = END_OF_FILE;
      ERROR (ARG_PARSING.MANUFA_FILE_NAME, RESULT, NB_MANUFA);
      MANUFAS(NB_MANUFA) := MANUFA_COTE;
    end loop;
    COTE_GET_LINE.CLOSE;
        
    -- Load and check Designs
    NB_DESIGN := 0;
    OPEN (ARG_PARSING.DESIGN_FILE_NAME);
    loop
      LOAD_COTE (DESIGN, NB_DESIGN, DESIGN_COTE, RESULT);
      exit when RESULT = END_OF_FILE;
      ERROR (ARG_PARSING.DESIGN_FILE_NAME, RESULT, NB_DESIGN);
      DESIGNS(NB_DESIGN) := DESIGN_COTE;
    end loop;
    COTE_GET_LINE.CLOSE;

    -- Check same number of cotes
    if NB_MANUFA /= NB_DESIGN then
      SYS_CALLS.PUT_LINE_ERROR ( "Error: files " & ARG_PARSING.MANUFA_FILE_NAME
                           & " and " & ARG_PARSING.DESIGN_FILE_NAME
                           & " don't have the same number of cotes.");
      raise LOAD_ERROR;
    end if;

    -- Check duplicates
    for I in 2 .. NB_MANUFA loop
      for J in 1 .. I - 1 loop
        if ARE_DUP (MANUFAS(I), MANUFAS(J)) then
          ERROR (ARG_PARSING.MANUFA_FILE_NAME, DUPLICATE, I, J);
        end if;
      end loop;
    end loop;

    -- Check duplicates
    for I in 2 .. NB_DESIGN loop
      for J in 1 .. I - 1 loop
        if ARE_DUP (DESIGNS(I), DESIGNS(J)) then
          ERROR (ARG_PARSING.DESIGN_FILE_NAME, DUPLICATE, I, J);
        end if;
      end loop;
    end loop;

    -- Check line nos and line usage
    LINE_USAGE := (others => FALSE);
    for I in 1 .. NB_MANUFA loop
      if      MANUFAS(I).START > NB_MANUFA + 1
      or else MANUFAS(I).STOP  > NB_MANUFA + 1 then
        ERROR (ARG_PARSING.MANUFA_FILE_NAME, LINE_NB, I);
      end if;
      LINE_USAGE(MANUFAS(I).START) := TRUE;
      LINE_USAGE(MANUFAS(I).STOP)  := TRUE;
    end loop;
    for I in 1 .. NB_MANUFA + 1 loop
      if not LINE_USAGE(I) then
        ERROR (ARG_PARSING.MANUFA_FILE_NAME, NO_COTE, 0, I);
      end if;
    end loop;

    -- Check line nos and line usage
    LINE_USAGE := (others => FALSE);
    for I in 1 .. NB_DESIGN loop
      if      DESIGNS(I).START > NB_DESIGN + 1
      or else DESIGNS(I).STOP  > NB_DESIGN + 1 then
        ERROR (ARG_PARSING.DESIGN_FILE_NAME, LINE_NB, I);
      end if;
      LINE_USAGE(DESIGNS(I).START) := TRUE;
      LINE_USAGE(DESIGNS(I).STOP)  := TRUE;
    end loop;
    for I in 1 .. NB_DESIGN + 1 loop
      if not LINE_USAGE(I) then
        ERROR (ARG_PARSING.DESIGN_FILE_NAME, NO_COTE, 0, I);
      end if;
    end loop;
  
    LOAD_OK := TRUE;
  end LOAD_COTES;

  function GET_NB_COTE return COTE_RANGE is
  begin
    if not LOAD_OK then
      raise LOAD_ERROR;
    end if;
    return NB_MANUFA;
  end GET_NB_COTE;

  function GET_MANUFA return MANUFA_ARRAY is
  begin
    if not LOAD_OK then
      raise LOAD_ERROR;
    end if;
    return MANUFAS(1 .. NB_MANUFA);
  end GET_MANUFA;

  function GET_DESIGN return DESIGN_ARRAY is
  begin
    if not LOAD_OK then
      raise LOAD_ERROR;
    end if;
    return DESIGNS(1 .. NB_DESIGN);
  end GET_DESIGN;

begin
  LOAD_COTES;
end FILE;
