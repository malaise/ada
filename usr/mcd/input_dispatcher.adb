with TEXT_IO;
with TEXT_HANDLER;
with DEBUG, MCD_MNG;
package body INPUT_DISPATCHER is

  -- Current input flow
  CURR_IS_STDIN : BOOLEAN := TRUE;

  -- Data from stdin
  STR_STDIN : STRING (1 .. MAX_STRING_LG);
  LEN_STDIN : NATURAL := 0;
  IND_STDIN : POSITIVE;

  -- Data from STDIN/SET_INPUT
  CUR_STR : STRING (1 .. MAX_STRING_LG);
  CUR_LEN : POSITIVE;

  STR_PARSED : BOOLEAN;

  -- Extracted from current STR
  WORD : TEXT_HANDLER.TEXT (MAX_STRING_LG);

  -- Get first/next word from a string
  CUR_INDEX : POSITIVE;

  function IS_SEPARATOR (C : in CHARACTER) return BOOLEAN is
  begin
    return C = ' ' or else C = ASCII.HT;
  end IS_SEPARATOR;

  function NEXT_STR_WORD return STRING is
    TMP_INDEX, STOP_INDEX : POSITIVE;
  begin
    -- Skip separators
    while CUR_INDEX <= CUR_LEN and then IS_SEPARATOR(CUR_STR(CUR_INDEX)) loop
      CUR_INDEX := CUR_INDEX + 1;
    end loop;
    if CUR_INDEX > CUR_LEN then
      -- no more word
      return "";
    end if;

    -- Got a start of word
    TMP_INDEX := CUR_INDEX;
    -- Look for seprator
    STOP_INDEX := TMP_INDEX + 1;
    while STOP_INDEX <= CUR_LEN and then not IS_SEPARATOR(CUR_STR(STOP_INDEX)) loop
      STOP_INDEX := STOP_INDEX + 1;
    end loop;

    -- This is the next start
    CUR_INDEX := STOP_INDEX;

    -- Stop is last char of word
    STOP_INDEX := STOP_INDEX - 1;
    return CUR_STR(TMP_INDEX .. STOP_INDEX);
  end NEXT_STR_WORD;

  function FIRST_STR_WORD (STR : STRING := "") return STRING is
  begin
    if STR /= "" then
      CUR_LEN := STR'LENGTH;
      CUR_STR(1 .. CUR_LEN) := STR;
    end if;
    CUR_INDEX := 1;
    return NEXT_STR_WORD;
  end FIRST_STR_WORD;

  -- Set input flow to a new string
  --  or stdin if STR is empty
  procedure SET_INPUT (STR : in STRING) is
  begin
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.INPUT) then
      TEXT_IO.PUT_LINE ("Input_dispacher: Setting input to >"
       & STR & "<");
    end if;
    if STR = "" then
      CURR_IS_STDIN := TRUE;
      CUR_INDEX := IND_STDIN;
      CUR_LEN := LEN_STDIN;
      CUR_STR(1 .. CUR_LEN) := STR_STDIN(1 .. LEN_STDIN);
    else
      if CURR_IS_STDIN then
        IND_STDIN := CUR_INDEX;
      end if;
      CURR_IS_STDIN := FALSE;
      CUR_LEN := STR'LENGTH;
      CUR_STR(1 .. CUR_LEN) := STR;
      STR_PARSED := FALSE;
    end if;
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.INPUT) then
      TEXT_IO.PUT_LINE ("Input_dispacher: Input set to >"
       & CUR_STR(1 .. CUR_LEN) & "< at " & INTEGER'IMAGE(CUR_INDEX)
       & " len " & INTEGER'IMAGE(CUR_LEN));
    end if;
  end SET_INPUT;

  -- Get the ungot words of current string
  -- PROGRAM_ERROR is current input is stdin
  --  or if no word already got from current string
  function GET_REMAINING return STRING is
  begin
    if CURR_IS_STDIN or else not STR_PARSED then
      if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.INPUT) then
        if CURR_IS_STDIN then
          TEXT_IO.PUT_LINE ("Input_dispacher: Remaining on stdin.");
        else
          TEXT_IO.PUT_LINE ("Input_dispacher: Remaining on unparsed.");
        end if;
      end if;
      raise PROGRAM_ERROR;
    end if;
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.INPUT) then
      TEXT_IO.PUT_LINE ("Input_dispacher: Remining is >"
       & CUR_STR(CUR_INDEX .. CUR_LEN) & "<");
    end if;
    return CUR_STR(CUR_INDEX .. CUR_LEN);
  end GET_REMAINING;

  -- Get next word from current input
  -- Empty if end of input flow
  function NEXT_WORD return STRING is
  begin
    if CURR_IS_STDIN then

      loop
        if LEN_STDIN = 0 then
          -- Need to get a new string
          begin
            TEXT_IO.GET_LINE (STR_STDIN, LEN_STDIN);
          exception
            when TEXT_IO.END_ERROR =>
              return "";
          end;
          if LEN_STDIN /= 0 and then STR_STDIN(1) /= '#' then
            -- Str not to discard, parse it
            TEXT_HANDLER.SET(WORD, FIRST_STR_WORD(STR_STDIN(1 .. LEN_STDIN)));
            exit when not TEXT_HANDLER.EMPTY(WORD);
          else
            -- Discard
            LEN_STDIN := 0;
          end if;
        else
          TEXT_HANDLER.SET(WORD, NEXT_STR_WORD);
          exit when not TEXT_HANDLER.EMPTY(WORD);
          -- End of string
          LEN_STDIN := 0;
        end if;
      end loop;
    
    else

      -- In string
      if not STR_PARSED then
        TEXT_HANDLER.SET(WORD, FIRST_STR_WORD);
        STR_PARSED := TRUE;
      else
        TEXT_HANDLER.SET(WORD, NEXT_STR_WORD);
      end if;

    end if;

    return TEXT_HANDLER.VALUE(WORD);
  end NEXT_WORD;


end INPUT_DISPATCHER;

