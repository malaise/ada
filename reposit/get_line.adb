package body GET_LINE is


  F : TEXT_IO.FILE_TYPE;
  CURRENT_LINE : LINE_ARRAY;
  NB_WORDS : WORD_COUNT;
  CURRENT_LINE_NO : TEXT_IO.COUNT;
  CUR : POSITIVE;
  CURRENT_WHOLE_LINE : LINE_TXT;
  FIRST_WORD : LINE_TXT;
  PARSED : BOOLEAN;
  BUFF : STRING (1 .. MAX_LINE_LEN+1);
  WORD : WORD_TXT;
  LAST : NATURAL;

  -- Opens the file. Exceptions are the one of TEXT_IO.OPEN (IN_FILE)
  -- Loads the first line
  procedure OPEN (FILE_NAME : in STRING) is
  begin
    CURRENT_LINE_NO := 0;
    TEXT_IO.OPEN (F, TEXT_IO.IN_FILE, FILE_NAME);
    READ_NEXT_LINE;
  end OPEN;

  procedure CLOSE is
  begin
    TEXT_IO.CLOSE (F);
  end CLOSE;

  -- Next word of BUFF (from CUR). "" if no more word.
  function GET_NEXT_WORD return STRING is
    F, L : POSITIVE;
    IN_WORD : BOOLEAN := TRUE;
  begin
    if CUR > LAST then
      return "";
    end if;
    F := CUR;
    for I in CUR .. LAST loop
      if BUFF(I) = ' ' or BUFF(I) = ASCII.HT then
        if IN_WORD then
          L := I;
          IN_WORD := FALSE;
        end if;
      else
        if not IN_WORD then
            CUR := I;
            return BUFF (F .. L-1);
        end if;
      end if;
    end loop;
    CUR := LAST + 1;
    if IN_WORD then
      return BUFF (F .. LAST);
    else
      return "";
    end if;
  end GET_NEXT_WORD;

  -- Reset CUR and parse leading spaces
  procedure RESET_WORD is
  begin
    NB_WORDS := 0;
    for I in 1 .. LAST loop
      if BUFF(I) = ' ' or BUFF(I) = ASCII.HT then
        null;
      else
        CUR := I;
        return;
      end if;
    end loop;
    CUR := LAST + 1;
  end RESET_WORD;

  -- Current line number
  function GET_LINE_NO return TEXT_IO.POSITIVE_COUNT is
  begin
    if not TEXT_IO.IS_OPEN (F) then
      raise NOT_OPEN;
    end if;
    return CURRENT_LINE_NO;
  end GET_LINE_NO;

  -- Get next line
  procedure READ_NEXT_LINE is
  begin
    PARSED := FALSE;
    if not TEXT_IO.IS_OPEN (F) then
      raise NOT_OPEN;
    end if;

    loop
      -- Get line from file
      begin
        TEXT_IO.GET_LINE (F, BUFF, LAST);
      exception
        when TEXT_IO.END_ERROR =>
          raise NO_MORE_LINE;
      end;

      CURRENT_LINE_NO := TEXT_IO. "+" (CURRENT_LINE_NO, 1);

      -- Check got line length
      if LAST = BUFF'LAST then
        raise LINE_TOO_LONG;
      end if;

      -- Store the line as it is in CURRENT_WHOLE_LINE
      TEXT_HANDLER.SET (CURRENT_WHOLE_LINE, BUFF(1 .. LAST));

      -- Remove trailing spaces
      while LAST > 0
       and then (BUFF(LAST) = ' ' or else BUFF(LAST) = ASCII.HT) loop
        LAST := LAST - 1;
      end loop;

      -- Remove leading spaces
      RESET_WORD;

      -- Parse first word
      TEXT_HANDLER.SET (FIRST_WORD, GET_NEXT_WORD);

      exit when COMMENT = ASCII.NUL
      or else (not TEXT_HANDLER.EMPTY(CURRENT_WHOLE_LINE)
               and then TEXT_HANDLER.VALUE(FIRST_WORD)(1) /= COMMENT);
    end loop;

  end READ_NEXT_LINE;


  -- Get the whole line (not parsed)
  procedure GET_WHOLE_LINE (LINE : in out LINE_TXT) is
  begin
    TEXT_HANDLER.SET (LINE, CURRENT_WHOLE_LINE);
  end GET_WHOLE_LINE;


    -- Get the first significant word of the line (not parsed)
  function GET_FIRST_WORD return STRING is
  begin
    if not TEXT_IO.IS_OPEN (F) then
      raise NOT_OPEN;
    end if;
    return TEXT_HANDLER.VALUE(FIRST_WORD);
  end GET_FIRST_WORD;



  procedure PARSE_WORDS is
  begin
    if PARSED then
      return;
    end if;
    RESET_WORD;
    -- Parse words
    loop
      -- Check word length
      begin
        TEXT_HANDLER.SET (WORD, GET_NEXT_WORD);
      exception
        when CONSTRAINT_ERROR =>
          raise WORD_TOO_LONG;
      end;

      -- Check no more word in line
      if TEXT_HANDLER.LENGTH(WORD) = 0 then
        exit;
      end if;

      -- Check word count
      if NB_WORDS = WORD_RANGE'LAST then
        raise TOO_MANY_WORDS;
      end if;

      -- Store word
      NB_WORDS := NB_WORDS + 1;
      TEXT_HANDLER.SET (CURRENT_LINE(NB_WORDS), WORD);
    end loop;
    PARSED := TRUE;
  end PARSE_WORDS;


  -- Number of words in currently loaded line
  function GET_WORD_NUMBER return WORD_COUNT is
  begin
    if not TEXT_IO.IS_OPEN (F) then
      raise NOT_OPEN;
    end if;
    PARSE_WORDS;
    return NB_WORDS;
  end GET_WORD_NUMBER;


  -- Words of the currently loaded line
  procedure GET_WORDS (LINE : in out LINE_ARRAY) is
  begin
    if not TEXT_IO.IS_OPEN (F) then
      raise NOT_OPEN;
    end if;
    PARSE_WORDS;
    for I in 1 .. NB_WORDS loop
      TEXT_HANDLER.SET (LINE(I), CURRENT_LINE(I));
    end loop;
    for I in INTEGER(NB_WORDS) + 1 .. WORD_RANGE'LAST loop
      TEXT_HANDLER.SET (LINE(I), "");
    end loop;
  end GET_WORDS;


end GET_LINE;

