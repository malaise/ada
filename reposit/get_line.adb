package body GET_LINE is


  F : TEXT_IO.FILE_TYPE;
  CURRENT_LINE : LINE_ARRAY;
  NB_WORDS : WORD_COUNT;
  CURRENT_LINE_NO : TEXT_IO.COUNT;
  CUR : POSITIVE;
  CURRENT_WHOLE_LINE : LINE_TXT;

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

  -- Next word of STR (from CUR). "" if no more word.
  function GET_NEXT_WORD (STR : STRING) return STRING is
    F, L : POSITIVE;
    IN_WORD : BOOLEAN := TRUE;
  begin
    if CUR > STR'LAST then
      return "";
    end if;
    F := CUR;
    for I in CUR .. STR'LAST loop
      if STR(I) = ' ' or STR(I) = ASCII.HT then
        if IN_WORD then
          L := I;
          IN_WORD := FALSE;
        end if;
      else
        if not IN_WORD then
            CUR := I;
            return STR (F .. L-1);
        end if;
      end if;
    end loop;
    CUR := STR'LAST + 1;
    if IN_WORD then
      return STR (F .. STR'LAST);
    else
      return "";
    end if;
  end GET_NEXT_WORD;

  -- Reset CUR and parse leading spaces
  procedure RESET_WORD (STR : in STRING) is
  begin
    for I in STR'FIRST .. STR'LAST loop
      if STR(I) = ' ' or STR(I) = ASCII.HT then
        null;
      else
        CUR := I;
        return;
      end if;
    end loop;
    CUR := STR'LAST + 1;
  end RESET_WORD;

  -- First word of STR ("" if not)
  function GET_FIRST_WORD (STR : STRING) return STRING is
  begin
    RESET_WORD (STR);
    return GET_NEXT_WORD (STR);
  end GET_FIRST_WORD;

  -- Get next line
  procedure READ_NEXT_LINE is
    BUFF : STRING (1 .. MAX_LINE_LEN+1);
    WORD : WORD_TXT;
    LAST : NATURAL;
  begin
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

    RESET_WORD (BUFF(1..LAST));
    NB_WORDS := 0;

    -- Parse words
    loop
      -- Check word length
      begin
        TEXT_HANDLER.SET (WORD, GET_NEXT_WORD(BUFF(1..LAST)));
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

  end READ_NEXT_LINE;

  -- Current line number
  function GET_LINE_NO return TEXT_IO.POSITIVE_COUNT is
  begin
    if TEXT_IO."=" (CURRENT_LINE_NO, 0) then
      return 1;
    else
      return CURRENT_LINE_NO;
    end if;
  end GET_LINE_NO;

  -- Number of words in currently loaded line
  function GET_WORD_NUMBER return WORD_COUNT is
  begin
    return NB_WORDS;
  end GET_WORD_NUMBER;

  -- Words of the currently loaded line
  procedure GET_WORDS (LINE : in out LINE_ARRAY) is
  begin
    for I in 1 .. NB_WORDS loop
      TEXT_HANDLER.SET (LINE(I), CURRENT_LINE(I));
    end loop;
    for I in INTEGER(NB_WORDS) + 1 .. WORD_RANGE'LAST loop
      TEXT_HANDLER.SET (LINE(I), "");
    end loop;
  end GET_WORDS;

  -- Get the whole line (not parsed)
  procedure GET_WHOLE_LINE (LINE : in out LINE_TXT) is
  begin
    TEXT_HANDLER.SET (LINE, CURRENT_WHOLE_LINE);
  end GET_WHOLE_LINE;

end GET_LINE;

