with TEXT_IO;
with SYS_CALLS, TEXT_HANDLER, MY_IO, ARGUMENT, UPPER_STR, NORMAL, MATH;
with GRID_1, GRID_2;

procedure CODE is
  CODE : BOOLEAN;
  MAX_LINE_LEN : constant := 1024;
  BUFF : STRING (1 .. MAX_LINE_LEN);
  MAX_FILE_LEN : constant := 58;
  REC : GRID_1.COORDINATE_REC;
  -- Bug in Meridian: Cannot allocate that in stack, but in heap.
  type ACCESS_LONG_STRING is access GRID_2.LONG_STRING;
  STR  : ACCESS_LONG_STRING
       := new GRID_2.LONG_STRING(1 .. MATH.INTE(1_048_576));
  FILE_TOO_LONG : exception;


  subtype LINE_INDEX is NATURAL range 0 .. MAX_LINE_LEN;
  LEN : LINE_INDEX;
  MIN_KEY_LEN : constant LINE_INDEX := 8;
  LINE_TOO_LONG : exception;
  KEY : TEXT_HANDLER.TEXT(80);
  IN_FILE  : TEXT_IO.FILE_TYPE;
  OUT_FILE : TEXT_IO.FILE_TYPE;
  SL : GRID_2.LONG_POSITIVE;
  SI : GRID_2.LONG_POSITIVE;
  C : CHARACTER;

  function IS_A_TTY return BOOLEAN is
    RESULT : INTEGER;
  begin
    RESULT := SYS_CALLS.CALL_SYSTEM("tty > /dev/null 2>&1");
    return RESULT = 0;
  end IS_A_TTY;
  

  procedure ECHO (ON : in BOOLEAN) is
    DUMMY : INTEGER;
    STTY_ECHO : constant STRING := "stty echo";
    STTY_NO_ECHO : constant STRING := "stty -echo";
  begin
    if ON then
      DUMMY := SYS_CALLS.CALL_SYSTEM(STTY_ECHO);
    else
      DUMMY := SYS_CALLS.CALL_SYSTEM(STTY_NO_ECHO);
    end if;
  end ECHO;


  procedure CODE_1 is
  begin
    begin
      LEN := LEN + 1;
    exception
      when CONSTRAINT_ERROR =>
        raise LINE_TOO_LONG;
    end;
    BUFF(LEN) := ASCII.CR;
    for I in 1 .. LEN loop
      REC := GRID_1.ENCODE(BUFF(I));
      if SL > STR'LAST then
        raise FILE_TOO_LONG;
      end if;
      STR(SL) := REC.ROW;
      STR(SL + 1) := REC.COL;
      SL := SL + 2;
    end loop;
  end CODE_1;

  procedure DECODE_1 is
  begin
    LEN := 0;
    loop
      REC.ROW := STR(SI);
      REC.COL := STR(SI + 1);
      SI := SI + 2;
      C := GRID_1.DECODE(REC);
      LEN := LEN + 1;
      BUFF(LEN) := C;
      exit when C = ASCII.CR;
    end loop;
  end DECODE_1;

begin

  -- Get coding mode
  begin
    if ARGUMENT.GET_NBRE_ARG < 2 or else ARGUMENT.GET_NBRE_ARG > 3 then
      raise ARGUMENT.ARGUMENT_NOT_FOUND;
    end if;
    if UPPER_STR (ARGUMENT.GET_PARAMETER) = "-C" then
      CODE := TRUE;
    elsif UPPER_STR (ARGUMENT.GET_PARAMETER) = "-D" then
      CODE := FALSE;
    else
      raise ARGUMENT.ARGUMENT_NOT_FOUND;
    end if;
  exception
    when others =>
      MY_IO.PUT_LINE ("Wrong argument. Usage : "
                     & ARGUMENT.GET_PARAMETER(OCCURENCE => 0) 
                     & " -c  |  -d     <input_file> [ <output_file> ] ");
      return;
  end;

  -- Get input file name
  ARGUMENT.GET_PARAMETER (BUFF(1 .. MAX_FILE_LEN), LEN, OCCURENCE => 2);
  -- Open input file
  begin
    TEXT_IO.OPEN (IN_FILE, TEXT_IO.IN_FILE, BUFF(1 .. LEN));
  exception
    when others =>
      MY_IO.PUT_LINE ("Unable to open input file >" & BUFF (1 .. LEN)
                     & "<. Abort.");
      raise;
  end;

  -- Get output file name
  if ARGUMENT.GET_NBRE_ARG = 3 then
    ARGUMENT.GET_PARAMETER (BUFF(1 .. MAX_FILE_LEN), LEN, OCCURENCE => 3);
    -- Open output file
    begin
      begin
        TEXT_IO.OPEN (OUT_FILE, TEXT_IO.OUT_FILE, BUFF(1 .. LEN));
      exception
        when TEXT_IO.NAME_ERROR =>
          TEXT_IO.CREATE (OUT_FILE, TEXT_IO.OUT_FILE, BUFF(1 .. LEN));
      end;
    exception
      when others =>
        MY_IO.PUT_LINE ("Unable to open/create output file >" & BUFF (1 .. LEN)
                       & "<. Abort.");
        raise;
    end;
  end if;

  -- Get key
  loop
    if IS_A_TTY then
      MY_IO.PUT ("Key: ");
      ECHO (FALSE);
    end if;
    MY_IO.GET_LINE (BUFF, LEN);
    if IS_A_TTY then
      ECHO (TRUE);
      MY_IO.NEW_LINE;
    end if;
    if LEN = 0 then
      MY_IO.PUT_LINE ("Program aborted by user.");
      return;
    elsif CODE and then LEN < MIN_KEY_LEN then
      MY_IO.PUT_LINE ("Too short ("
                    & NORMAL(MIN_KEY_LEN, 1) & " min), try again.");
      if not IS_A_TTY then
        return;
      end if;
    else
      exit;
    end if;
  end loop;

  if ARGUMENT.GET_NBRE_ARG = 3 then
    TEXT_IO.SET_OUTPUT (OUT_FILE);
  end if;

  TEXT_HANDLER.SET (KEY, BUFF (1 .. LEN));

  -- Initialize coding
  GRID_1.INITIALIZE(TEXT_HANDLER.VALUE(KEY));

  if CODE then
    SL := 1;
    -- Code key
    CODE_1;
    -- Code input file
    loop
      -- Read input file
      begin
        TEXT_IO.GET_LINE (IN_FILE, BUFF, LEN);
      exception
        when TEXT_IO.END_ERROR => exit;
      end;
      -- Code line
      CODE_1;
    end loop;
    SL := SL - 1;

    -- Code through code 2
    STR (1 .. SL) := GRID_2.ENCODE(TEXT_HANDLER.VALUE(KEY), STR(1 .. SL));
    -- Output result (cut each 80 cars)
    for I in 1 .. SL loop
      TEXT_IO.PUT (STR(I));
      if I mod 78 = 0 then
        TEXT_IO.NEW_LINE;
      end if;
    end loop;
    if SL mod 78 /= 0 then
      TEXT_IO.NEW_LINE;
    end if;
  else
    -- Decode input file
    SL := 1;
    loop
      -- Read input file
      begin
        TEXT_IO.GET_LINE (IN_FILE, BUFF, LEN);
      exception
        when TEXT_IO.END_ERROR => exit;
      end;
      -- Store characters
      for I in 1 .. LEN loop
        STR(SL) := BUFF(I);
        SL := SL + 1;
      end loop;
    end loop;
    SL := SL - 1;

    -- Decode through code 2
    STR (1 .. SL) := GRID_2.DECODE(TEXT_HANDLER.VALUE(KEY), STR(1 .. SL));

    -- Decode through code 1
    begin
      SI := 1;
      DECODE_1;
      if BUFF(1 .. LEN-1) = TEXT_HANDLER.VALUE (KEY) then
        loop
          DECODE_1;
          exit when LEN = 0;
          TEXT_IO.PUT_LINE (BUFF(1 .. LEN-1));
        end loop;
      end if;
    exception
      when others =>
        null;
    end;

  end if;


  TEXT_IO.CLOSE (IN_FILE);
  TEXT_IO.SET_OUTPUT (TEXT_IO.STANDARD_OUTPUT);
  if TEXT_IO.IS_OPEN (OUT_FILE) then
    TEXT_IO.CLOSE (OUT_FILE);
  end if;
exception
  when LINE_TOO_LONG =>
    TEXT_IO.PUT_LINE ("ERROR, input line too long.");
  when FILE_TOO_LONG =>
    TEXT_IO.PUT_LINE ("ERROR, input file too long.");
end CODE;
