with TEXT_IO;
with TEXT_HANDLER, MY_IO, ARGUMENT, UPPER_STR, SYS_CALLS, MATH;
with GRID_1, GRID_2;

procedure CODE is
  CODE : BOOLEAN;
  BUFF : STRING (1 .. 132);
  MAX_FILE_LEN : constant := 58;
  REC : GRID_1.COORDINATE_REC;
  -- Bug in Meridian: Cannot allocate that in stack, but in heap.
  type ACCESS_LONG_STRING is access GRID_2.LONG_STRING;
  STR  : ACCESS_LONG_STRING
       := new GRID_2.LONG_STRING(1 .. MATH.INTE(262_144));


  LEN : NATURAL;
  KEY : TEXT_HANDLER.TEXT(80);
  IN_FILE  : TEXT_IO.FILE_TYPE;
  OUT_FILE : TEXT_IO.FILE_TYPE;
  SL : GRID_2.LONG_POSITIVE;
  SI : GRID_2.LONG_POSITIVE;
  C : CHARACTER;

  procedure CODE_1 is
  begin
    LEN := LEN + 1;
    BUFF(LEN) := ASCII.CR;
    for I in 1 .. LEN loop
      REC := GRID_1.ENCODE(BUFF(I));
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
    if UPPER_STR (ARGUMENT.GET_PARAMETER) = "/C" then
      CODE := TRUE;
    elsif UPPER_STR (ARGUMENT.GET_PARAMETER) = "/D" then
      CODE := FALSE;
    else
      raise ARGUMENT.ARGUMENT_NOT_FOUND;
    end if;
  exception
    when others =>
      MY_IO.PUT_LINE ("Wrong argument. Usage : "
                     & ARGUMENT.GET_PARAMETER(OCCURENCE => 0) 
                     & " /C  |  /D     <input_file> [ <output_file> ] ");
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
  MY_IO.PUT ("Key: ");
  declare
    DUMMY : INTEGER;
  begin
    DUMMY := SYS_CALLS.CALL_SYSTEM ("stty -echo");
    MY_IO.GET_LINE (BUFF, LEN);
    DUMMY := SYS_CALLS.CALL_SYSTEM ("stty echo");
  exception
    when others =>
      DUMMY := SYS_CALLS.CALL_SYSTEM ("stty echo");
  end;
  if LEN = 0 then
    MY_IO.PUT_LINE ("Program aborted by user.");
    return;
  end if;
  MY_IO.NEW_LINE;

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

end CODE;
