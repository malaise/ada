with TEXT_IO, DIRECT_IO;
with CON_IO, GET_LINE, TEXT_HANDLER, NORMAL, ARGUMENT, DIRECTORY;
with AFPX_TYP;
use  AFPX_TYP;
-- Read AFPX.LIS, check
-- Build AFPX.DSC list of DESCR_REC
--       AFPX.FLD list of FIELDS_ARRAY
--       AFPX.INI list of CHAR_STR
procedure AFPX_BLD is

  -- Inputs name
  DEFAULT_LIST_FILE_NAME : constant STRING := "AFPX.LIS";
  LIST_FILE_NAME : TEXT_HANDLER.TEXT (DIRECTORY.MAX_DIR_NAME_LEN * 2);

  -- GET_LINE of descriptor file
  package DSCR_GET is new GET_LINE (MAX_WORD_LEN => 80, 
                                    MAX_WORD_NB  => 45,
                                    MAX_LINE_LEN => 132,
                                    COMMENT      => '#');
  DSCR_LINE : DSCR_GET.LINE_ARRAY;
  DSCR_WORDS : DSCR_GET.WORD_COUNT;

  -- Direct_io of descriptors, fields, init strings
  package DSCR_IO is new DIRECT_IO (AFPX_TYP.DESCRIPTORS_ARRAY);
  DSCR_FILE : DSCR_IO.FILE_TYPE;
  package FLD_IO  is new DIRECT_IO (AFPX_TYP.FIELDS_ARRAY);
  FLD_FILE : FLD_IO.FILE_TYPE;
  package INIT_IO is new DIRECT_IO (AFPX_TYP.CHAR_STR);
  INIT_FILE : INIT_IO.FILE_TYPE;

  -- List of descriptors
  DESCRIPTORS : AFPX_TYP.DESCRIPTORS_ARRAY;

  -- List of fields
  FIELDS : AFPX_TYP.FIELDS_ARRAY;

  -- Index in INIT_STR
  INIT_INDEX : POSITIVE;

  -- Initial characters of the fields
  INIT_STR : AFPX_TYP.CHAR_STR;

  -- Errors
  ARGUMENT_ERROR : exception;
  FILE_SYNTAX_ERROR : exception;
  FILE_NOT_FOUND : exception;

  -- Expected number of arguments
  EXPECTED_ARGS : NATURAL;

  procedure NEXT_LINE is
  begin
    DSCR_GET.READ_NEXT_LINE;
    DSCR_WORDS := DSCR_GET.GET_WORD_NUMBER;
    DSCR_GET.GET_WORDS(DSCR_LINE);
  end NEXT_LINE;

  procedure DUMP_LINE is
  begin
    TEXT_IO.PUT(TEXT_IO.POSITIVE_COUNT'IMAGE(DSCR_GET.GET_LINE_NO) & " : ");
    for I in 1 .. DSCR_WORDS loop
      TEXT_IO.PUT(">" & TEXT_HANDLER.VALUE(DSCR_LINE(I)) & "< ");
    end loop;
    TEXT_IO.NEW_LINE;
  end DUMP_LINE;

  procedure CLOSE (ON_ERROR : in BOOLEAN) is
  begin
    begin
      DSCR_GET.CLOSE;
    exception
      when others =>
        null;
    end;
    if ON_ERROR and then DSCR_IO.IS_OPEN (DSCR_FILE) then
      begin
        DSCR_IO.DELETE (DSCR_FILE);
      exception
        when others =>
          null;
      end;
      begin
        FLD_IO.DELETE (FLD_FILE);
      exception
        when others =>
          null;
      end;
      begin
        INIT_IO.DELETE (INIT_FILE);
      exception
        when others =>
          null;
      end;
    else
      begin
        DSCR_IO.CLOSE (DSCR_FILE);
      exception
        when others =>
          null;
      end;
      begin
        FLD_IO.CLOSE (FLD_FILE);
      exception
        when others =>
          null;
      end;
      begin
        INIT_IO.CLOSE (INIT_FILE);
      exception
        when others =>
          null;
      end;
    end if;
  end CLOSE;

  function FIRST_WORD return STRING is
  begin
    return TEXT_HANDLER.VALUE(DSCR_LINE(1));
  end;

  function END_OF (KEYWORD : STRING) return BOOLEAN is
  begin
    return DSCR_WORDS = 2 and then
           FIRST_WORD = "END" and then
           TEXT_HANDLER.VALUE(DSCR_LINE(2)) = KEYWORD;
  end END_OF;

  procedure FILE_ERROR (MSG : in STRING := "") is
  begin
    if MSG = "" then
      TEXT_IO.PUT_LINE ("SYNTAX ERROR.");
    else
      TEXT_IO.PUT_LINE ("SYNTAX ERROR : " & MSG & ".");
    end if;
    TEXT_IO.PUT (" In file " & TEXT_HANDLER.VALUE(LIST_FILE_NAME)
               & " at line ");
    DUMP_LINE;
    raise FILE_SYNTAX_ERROR;
  end FILE_ERROR;

  -- Check and store upper_left (and lower right in size) and colors
  procedure LOAD_GEO_COLOR (FN : in AFPX_TYP.ABSOLUTE_FIELD_RANGE) is
  begin
    if FIRST_WORD /= "GEOMETRY" or else DSCR_WORDS /= 5 then
      FILE_ERROR ("GEOMETRY <upper_row> <left_col> <lower_row> <right_col> expected");
    end if;
    begin
      -- Load upper_right and lower left
      FIELDS(FN).UPPER_LEFT.ROW :=
       CON_IO.ROW_RANGE'VALUE(TEXT_HANDLER.VALUE(DSCR_LINE(2)));
      FIELDS(FN).UPPER_LEFT.COL :=
       CON_IO.COL_RANGE'VALUE(TEXT_HANDLER.VALUE(DSCR_LINE(3)));
      FIELDS(FN).LOWER_RIGHT.ROW :=
       CON_IO.ROW_RANGE'VALUE(TEXT_HANDLER.VALUE(DSCR_LINE(4)));
      FIELDS(FN).LOWER_RIGHT.COL :=
       CON_IO.COL_RANGE'VALUE(TEXT_HANDLER.VALUE(DSCR_LINE(5)));
    exception
      when others =>
        FILE_ERROR ("Invalid geometry");
    end;
    if      FIELDS(FN).UPPER_LEFT.ROW > FIELDS(FN).LOWER_RIGHT.ROW
    or else FIELDS(FN).UPPER_LEFT.COL > FIELDS(FN).LOWER_RIGHT.COL
    then
      FILE_ERROR ("Invalid geometry. Upper_left < lower_right");
    end if;

    -- Compute size
    FIELDS(FN).HEIGHT :=
     FIELDS(FN).LOWER_RIGHT.ROW - FIELDS(FN).UPPER_LEFT.ROW + 1;
    FIELDS(FN).WIDTH :=
     FIELDS(FN).LOWER_RIGHT.COL - FIELDS(FN).UPPER_LEFT.COL + 1;

    -- One ROW for GET fields
    if FIELDS(FN).KIND = AFPX_TYP.GET and then FIELDS(FN).HEIGHT /= 1 then
      FILE_ERROR ("Invalid geometry. GET fields must have ONE row");
    end if;

    NEXT_LINE;

    -- Parse colors
    if FIRST_WORD /= "COLORS" then
      FILE_ERROR ("Keyword COLORS expected");
    end if;
    begin
      if FN = 0 or else FIELDS(FN).KIND = AFPX_TYP.GET then
        if DSCR_WORDS /= 4 then
          FILE_ERROR ("COLORS <foreground> <background> <selected> expected");
        end if;
        FIELDS(FN).COLORS.FOREGROUND :=
         CON_IO.EFFECTIVE_COLORS'VALUE (TEXT_HANDLER.VALUE(DSCR_LINE(2)));
        FIELDS(FN).COLORS.BLINK_STAT := CON_IO.NOT_BLINK;
        FIELDS(FN).COLORS.BACKGROUND :=
         CON_IO.EFFECTIVE_BASIC_COLORS'VALUE (TEXT_HANDLER.VALUE(DSCR_LINE(3)));
        FIELDS(FN).COLORS.SELECTED :=
         CON_IO.EFFECTIVE_BASIC_COLORS'VALUE (TEXT_HANDLER.VALUE(DSCR_LINE(4)));

      elsif FIELDS(FN).KIND = PUT then
        if DSCR_WORDS /= 4 then
          FILE_ERROR ("COLORS <foreground> <blink> <background> expected");
        end if;
        FIELDS(FN).COLORS.FOREGROUND :=
         CON_IO.EFFECTIVE_COLORS'VALUE (TEXT_HANDLER.VALUE(DSCR_LINE(2)));
        FIELDS(FN).COLORS.BLINK_STAT :=
         CON_IO.EFFECTIVE_BLINK_STATS'VALUE(TEXT_HANDLER.VALUE(DSCR_LINE(3)));
        FIELDS(FN).COLORS.BACKGROUND :=
         CON_IO.EFFECTIVE_BASIC_COLORS'VALUE (TEXT_HANDLER.VALUE(DSCR_LINE(4)));
        FIELDS(FN).COLORS.SELECTED := FIELDS(FN).COLORS.BACKGROUND;

      elsif FIELDS(FN).KIND = BUTTON then
        if DSCR_WORDS /= 3 then
          FILE_ERROR ("COLORS <foreground> <background> expected");
        end if;
        FIELDS(FN).COLORS.FOREGROUND :=
         CON_IO.EFFECTIVE_COLORS'VALUE (TEXT_HANDLER.VALUE(DSCR_LINE(2)));
        FIELDS(FN).COLORS.BLINK_STAT := CON_IO.NOT_BLINK;
        FIELDS(FN).COLORS.BACKGROUND :=
         CON_IO.EFFECTIVE_BASIC_COLORS'VALUE (TEXT_HANDLER.VALUE(DSCR_LINE(3)));
        FIELDS(FN).COLORS.SELECTED := FIELDS(FN).COLORS.BACKGROUND;
      end if;
    exception
      when FILE_SYNTAX_ERROR =>
        raise;
      when others =>
        FILE_ERROR ("Invalid colors specification");
    end;

    -- Foreground has to be basic for all but PUT fields
    if (FN = 0 or else FIELDS(FN).KIND /= PUT)
    and then FIELDS(FN).COLORS.FOREGROUND
             not in CON_IO.EFFECTIVE_BASIC_COLORS then
      -- For list, GET and BUTTON, FOREGROUND has to be basic
      FILE_ERROR ("For all but PUT fields, FOREGROUND has to be basic color");
    end if;
    NEXT_LINE;
  end LOAD_GEO_COLOR;

  procedure LOAD_LIST is
  begin
    if DSCR_WORDS /= 1 then
      FILE_ERROR ("LIST expected");
    end if;
    -- In LIST
    FIELDS(0).KIND := AFPX_TYP.BUTTON;
    FIELDS(0).ACTIVATED := TRUE;
    FIELDS(0).ISPROTECTED := FALSE;
    NEXT_LINE;
    LOAD_GEO_COLOR (0);
    if not END_OF ("LIST") then
      FILE_ERROR ("END LIST expected");
    end if;
    FIELDS(0).CHAR_INDEX := 1;
    NEXT_LINE;
  end LOAD_LIST;

  procedure LOC_LOAD_FIELD (NO : in AFPX_TYP.FIELD_RANGE)  is
    FIRST_INIT : BOOLEAN;
    PREV_INIT_SQUARE : CON_IO.SQUARE;
    -- Location in field of init string
    FINIT_SQUARE : CON_IO.SQUARE;
    -- Whole init line got from get_line
    FINIT_LINE   : DSCR_GET.LINE_TXT;
    -- Init string
    FINIT_STR    : STRING (1 .. FINIT_LINE.MAX_LEN);
    FINIT_LEN    : NATURAL;
    FINIT_START  : POSITIVE;
    -- Index in CHAR_STR of beginning of INIT string
    FINIT_INDEX : AFPX_TYP.CHAR_STR_RANGE;
  begin
    if DSCR_WORDS /= 3 then
      FILE_ERROR ("FIELD <field_no> <field_type> expected");
    end if;
    begin
      if AFPX_TYP.FIELD_RANGE'VALUE(TEXT_HANDLER.VALUE(DSCR_LINE(2))) /=
      NO then
        raise CONSTRAINT_ERROR;
      end if;
    exception
      when others =>
        FILE_ERROR ("Invalid field number. They must crescent positives");
    end;
    begin
      FIELDS(NO).KIND :=
        AFPX_TYP.FIELD_KIND_LIST'VALUE(TEXT_HANDLER.VALUE(DSCR_LINE(3)));
    exception
      when others =>
        FILE_ERROR ("Invalid field type. PUT, GET or BUTTON expected");
    end;
    FIELDS(NO).ACTIVATED := TRUE;
    FIELDS(NO).ISPROTECTED := FALSE;
    NEXT_LINE;
    LOAD_GEO_COLOR (NO);

    FIELDS(NO).CHAR_INDEX := INIT_INDEX;
    FINIT_LEN := FIELDS(NO).HEIGHT * FIELDS(NO).WIDTH;
    begin
      INIT_INDEX := INIT_INDEX + FINIT_LEN + 1;
    exception
      when others =>
        FILE_ERROR ("Too many init characters");
    end;

    if FIRST_WORD = "INIT" then

      if DSCR_WORDS /= 1 then
        FILE_ERROR ("INIT expected");
      end if;
      NEXT_LINE;

      FIRST_INIT := TRUE;
      PREV_INIT_SQUARE := (0, 0);
      while not END_OF ("INIT") loop
        -- Check init syntax and length of string
        if DSCR_WORDS < 2 then
          FILE_ERROR ("Invalid init. <row> <col> [ <str> ] expected");
        end if;
        begin
          FINIT_SQUARE.ROW :=
           CON_IO.ROW_RANGE'VALUE(TEXT_HANDLER.VALUE(DSCR_LINE(1)));
          FINIT_SQUARE.COL :=
           CON_IO.ROW_RANGE'VALUE(TEXT_HANDLER.VALUE(DSCR_LINE(2)));
        exception
          when others =>
            FILE_ERROR ("Invalid init row or column");
        end;
        -- Check init squares crescent
        if not FIRST_INIT then
          if FINIT_SQUARE.ROW < PREV_INIT_SQUARE.ROW then
            FILE_ERROR ("Invalid init row. Must be crescent");
          elsif    FINIT_SQUARE.ROW  = PREV_INIT_SQUARE.ROW
          and then FINIT_SQUARE.COL <= PREV_INIT_SQUARE.COL then
            FILE_ERROR ("Invalid init col. Must be crescent and not overlap");
          end if;
        end if;
        FIRST_INIT := FALSE;
        PREV_INIT_SQUARE := FINIT_SQUARE;
        -- Check init square in field
        if not AFPX_TYP.IN_FIELD (FIELDS(NO), FINIT_SQUARE) then
          FILE_ERROR ("Init row or col not in field");
        end if;
        -- Get the whole line to extract init string
        DSCR_GET.GET_WHOLE_LINE (FINIT_LINE);
        FINIT_LEN := TEXT_HANDLER.LENGTH (FINIT_LINE);
        FINIT_STR (1 .. FINIT_LEN) := TEXT_HANDLER.VALUE (FINIT_LINE);
        FINIT_START := 1;
        -- Skeep spaces after string
        while   FINIT_STR(FINIT_LEN) = ' '
        or else FINIT_STR(FINIT_LEN) = ASCII.HT loop
          FINIT_LEN := FINIT_LEN - 1;
        end loop;
        -- Skeep spaces before row
        while   FINIT_STR(FINIT_START) = ' '
        or else FINIT_STR(FINIT_START) = ASCII.HT loop
          FINIT_START := FINIT_START + 1;
        end loop;
        -- Skip row
        while    FINIT_STR(FINIT_START) /= ' '
        and then FINIT_STR(FINIT_START) /= ASCII.HT loop
          FINIT_START := FINIT_START + 1;
        end loop;
        -- Skeep spaces between row and col
        while   FINIT_STR(FINIT_START) = ' '
        or else FINIT_STR(FINIT_START) = ASCII.HT loop
          FINIT_START := FINIT_START + 1;
        end loop;
        -- Skip col
        while    FINIT_START <= FINIT_LEN
        and then FINIT_STR(FINIT_START) /= ' '
        and then FINIT_STR(FINIT_START) /= ASCII.HT loop
          FINIT_START := FINIT_START + 1;
        end loop;
        if FINIT_START /= FINIT_LEN then
          -- There is a init string. Skeep spaces between col and str
          while   FINIT_START <= FINIT_LEN
          and then (        FINIT_STR(FINIT_START) = ' '
                    or else FINIT_STR(FINIT_START) = ASCII.HT) loop
            FINIT_START := FINIT_START + 1;
          end loop;
          -- Set effective init string
          FINIT_STR (1 .. FINIT_LEN - FINIT_START + 1) :=
                 FINIT_STR (FINIT_START .. FINIT_LEN);
          FINIT_LEN := FINIT_LEN - FINIT_START + 1;
          -- Check FINIT col + string length compatible with field width
          if not AFPX_TYP.IN_FIELD (FIELDS(NO),
                  (FINIT_SQUARE.ROW, FINIT_SQUARE.COL + FINIT_LEN - 1)) then
            FILE_ERROR ("Init string too long for this col in this field");
          end if;
          -- Update prev_init_square col to last char of init string
          PREV_INIT_SQUARE.COL := FINIT_SQUARE.COL + FINIT_LEN - 1;
          -- Copy in init string
          FINIT_INDEX := FIELDS(NO).CHAR_INDEX
           + FINIT_SQUARE.ROW * FIELDS(NO).WIDTH
           + FINIT_SQUARE.COL;
          INIT_STR (FINIT_INDEX .. FINIT_INDEX + FINIT_LEN - 1) :=
            FINIT_STR (1 .. FINIT_LEN);
        end if;
        NEXT_LINE;
      end loop;
      NEXT_LINE;
    end if;
    if not END_OF ("FIELD") then
      FILE_ERROR ("END FIELD expected");
    end if;
    NEXT_LINE;
  end LOC_LOAD_FIELD;

  procedure CHECK_OVERLAP (DSCR_NO  : in DESCRIPTOR_RANGE;
                           FI1, FI2 : in ABSOLUTE_FIELD_RANGE) is
    F1 : FIELD_REC renames FIELDS(FI1);
    F2 : FIELD_REC renames FIELDS(FI2);

  begin
            -- F1 above F2
    if      F1.UPPER_LEFT.ROW  > F2.LOWER_RIGHT.ROW
            -- F1 below F2
    or else F1.LOWER_RIGHT.ROW < F2.UPPER_LEFT.ROW
            -- F1 left to F2
    or else F1.UPPER_LEFT.COL  > F2.LOWER_RIGHT.COL
            --  F1 right to F2
    or else F1.LOWER_RIGHT.COL < F2.UPPER_LEFT.COL
    then
      -- No overlap
      return;
    end if;
    if FI1 = 0 then
      TEXT_IO.PUT ("ERROR : LIST");
    else
      TEXT_IO.PUT ("ERROR : Field " & AFPX_TYP.FIELD_RANGE'IMAGE(FI1));
    end if;
    TEXT_IO.PUT_LINE (" and Field " & AFPX_TYP.FIELD_RANGE'IMAGE(FI2)
                      & " of descriptor "
                      & AFPX_TYP.DESCRIPTOR_RANGE'IMAGE(DSCR_NO)
                      & " overlap.");
    raise FILE_SYNTAX_ERROR;
  end CHECK_OVERLAP;

  procedure LOAD_DSCRS (CHECK_ONLY : in BOOLEAN) is
    DSCR_INDEX : AFPX_TYP.DESCRIPTOR_RANGE;
    DSCR_NO    : AFPX_TYP.DESCRIPTOR_RANGE;
    ERROR_MSG : TEXT_HANDLER.TEXT(60);
    EOF_ALLOWED : BOOLEAN := FALSE;

  begin
    -- Open list file.
    begin
      DSCR_GET.OPEN (TEXT_HANDLER.VALUE(LIST_FILE_NAME));
    exception
      when TEXT_IO.NAME_ERROR =>
        TEXT_IO.PUT_LINE ("ERROR : File " & TEXT_HANDLER.VALUE(LIST_FILE_NAME)
                          & " not found.");
        raise FILE_NOT_FOUND;
    end;
    -- If not check_only, delete then create binary files
    if not CHECK_ONLY then
      begin
        DSCR_IO.OPEN (DSCR_FILE, DSCR_IO.IN_FILE,
                      TEXT_HANDLER.VALUE(AFPX_TYP.DEST_PATH) & AFPX_TYP.DSCR_FILE_NAME);
        DSCR_IO.DELETE (DSCR_FILE);
      exception
        when DSCR_IO.NAME_ERROR => null;
      end;
      DSCR_IO.CREATE (DSCR_FILE, DSCR_IO.OUT_FILE,
                      TEXT_HANDLER.VALUE(AFPX_TYP.DEST_PATH) & AFPX_TYP.DSCR_FILE_NAME);

      begin
        FLD_IO.OPEN (FLD_FILE, FLD_IO.IN_FILE,
                     TEXT_HANDLER.VALUE(AFPX_TYP.DEST_PATH) & AFPX_TYP.FLD_FILE_NAME);
        FLD_IO.DELETE (FLD_FILE);
      exception
        when FLD_IO.NAME_ERROR => null;
      end;
      FLD_IO.CREATE (FLD_FILE, FLD_IO.OUT_FILE,
                     TEXT_HANDLER.VALUE(AFPX_TYP.DEST_PATH) & AFPX_TYP.FLD_FILE_NAME);

      begin
        INIT_IO.OPEN (INIT_FILE, INIT_IO.IN_FILE,
                      TEXT_HANDLER.VALUE(AFPX_TYP.DEST_PATH) & AFPX_TYP.INIT_FILE_NAME);
        INIT_IO.DELETE (INIT_FILE);
      exception
        when INIT_IO.NAME_ERROR => null;
      end;
      INIT_IO.CREATE (INIT_FILE, INIT_IO.OUT_FILE,
                      TEXT_HANDLER.VALUE(AFPX_TYP.DEST_PATH) & AFPX_TYP.INIT_FILE_NAME);
    end if;

    -- Initialize the descriptors array as not used
    for I in AFPX_TYP.DESCRIPTOR_RANGE loop
      DESCRIPTORS(I).VERSION  := AFPX_TYP.AFPX_VERSION;
      DESCRIPTORS(I).MODIFIED := FALSE;
      DESCRIPTORS(I).DSCR_INDEX := AFPX_TYP.DESCRIPTOR_RANGE'FIRST;
      DESCRIPTORS(I).NB_FIELDS := 0;
    end loop;

    TEXT_HANDLER.SET (ERROR_MSG, "DESCRIPTOR <descriptor_number> expected");
    DSCR_WORDS := DSCR_GET.GET_WORD_NUMBER;
    DSCR_GET.GET_WORDS(DSCR_LINE);

    -- Loop on descriptors
    -- Descriprors are stored in the descriptor file at DSCR_NO
    -- Fields and init tables are stored in their files at DSCR_INDEX
    DSCR_INDEX := 1;
    DSCRS:
    loop
      EOF_ALLOWED := FALSE;
      -- DESCRIPTOR line
      TEXT_HANDLER.SET (ERROR_MSG, "DESCRIPTOR <descriptor_number> expected");
      if DSCR_WORDS /= 2 then
        FILE_ERROR (TEXT_HANDLER.VALUE (ERROR_MSG));
      end if;
      if FIRST_WORD /= "DESCRIPTOR" then
        FILE_ERROR (TEXT_HANDLER.VALUE (ERROR_MSG));
      end if;
      begin
        DSCR_NO := AFPX_TYP.DESCRIPTOR_RANGE'VALUE (
                               TEXT_HANDLER.VALUE(DSCR_LINE(2)));
      exception
        when others =>
        FILE_ERROR (TEXT_HANDLER.VALUE (ERROR_MSG));
      end;
      TEXT_IO.PUT_LINE ("   descriptor " &
                        NORMAL(INTEGER(DSCR_NO), 2, GAP => '0'));
      -- Dscr no has to be uniq
      if DESCRIPTORS(DSCR_NO).MODIFIED then
        FILE_ERROR ("DESCRIPTOR " & AFPX_TYP.DESCRIPTOR_RANGE'IMAGE(DSCR_NO)
                                  & " already defined");
      end if;
      -- Init dscr and fields array. No list at init
      DESCRIPTORS(DSCR_NO).MODIFIED := TRUE;
      DESCRIPTORS(DSCR_NO).DSCR_INDEX := DSCR_INDEX;
      DESCRIPTORS(DSCR_NO).NB_FIELDS := 0;
      INIT_INDEX := 1;
      INIT_STR := (others => ' ');
      NEXT_LINE;

      if FIRST_WORD = "LIST" then
        LOAD_LIST;
      else
        FIELDS(0).KIND := PUT;
      end if;
      while FIRST_WORD = "FIELD" loop
        if DESCRIPTORS(DSCR_NO).NB_FIELDS /= AFPX_TYP.FIELD_RANGE 'LAST then
          DESCRIPTORS(DSCR_NO).NB_FIELDS := DESCRIPTORS(DSCR_NO).NB_FIELDS + 1;
        else
          FILE_ERROR ("Too many fields. Maximum is"
           & FIELD_RANGE'IMAGE(AFPX_TYP.FIELD_RANGE'LAST) & " per descriptor");
        end if;
        LOC_LOAD_FIELD (DESCRIPTORS(DSCR_NO).NB_FIELDS);
      end loop;

      if not END_OF ("DESCRIPTOR") then
        if DESCRIPTORS(DSCR_NO).NB_FIELDS = 0
        and then FIELDS(0).KIND = AFPX_TYP.PUT then
          -- No list nor field
          FILE_ERROR ("LIST, FIELD, or END DESCRIPTOR expected");
        else
          -- Some list or fields
          FILE_ERROR ("FIELD, or END DESCRIPTOR expected");
        end if;
      end if;

      -- Check no overlapping of fields
      if FIELDS(0).KIND /= PUT then
        -- Check list and each field
        for J in 1 .. DESCRIPTORS(DSCR_NO).NB_FIELDS loop
          CHECK_OVERLAP (DSCR_NO, 0, J);
        end loop;
      end if;
      -- Check each field with others
      for I in 1 .. DESCRIPTORS(DSCR_NO).NB_FIELDS - 1 loop
        for J in I + 1 .. DESCRIPTORS(DSCR_NO).NB_FIELDS loop
          CHECK_OVERLAP (DSCR_NO, I, J);
        end loop;
      end loop;

      -- if not check_only, write fields and init
      if not CHECK_ONLY then
        FLD_IO.WRITE  (FLD_FILE , FIELDS,   FLD_IO.POSITIVE_COUNT(DSCR_INDEX));
        INIT_IO.WRITE (INIT_FILE, INIT_STR, INIT_IO.POSITIVE_COUNT(DSCR_INDEX));
      end if;

      DSCR_INDEX := DSCR_INDEX + 1;
      EOF_ALLOWED := TRUE;
      NEXT_LINE;
    end loop DSCRS;

    -- Should never go there.
    -- Exit loop DSCRS on exception DSCR_GET.NO_MORE_LINE
    raise CONSTRAINT_ERROR;

  exception
    when DSCR_GET.NO_MORE_LINE =>
      if EOF_ALLOWED then
        -- if not check_only, write descriptors and close files
        if not CHECK_ONLY then
          DSCR_IO.WRITE (DSCR_FILE, DESCRIPTORS);
        end if;
        CLOSE(FALSE);
      else
        FILE_ERROR ("Unexpected end of file");
        CLOSE (TRUE);
        raise FILE_SYNTAX_ERROR;
      end if;
    when DSCR_GET.TOO_MANY_WORDS =>
      FILE_ERROR ("Too many words");
      CLOSE (TRUE);
      raise FILE_SYNTAX_ERROR;
    when DSCR_GET.LINE_TOO_LONG =>
      FILE_ERROR ("Line too long");
      CLOSE (TRUE);
      raise FILE_SYNTAX_ERROR;
    when DSCR_GET.WORD_TOO_LONG =>
      FILE_ERROR ("Word too long");
      CLOSE (TRUE);
      raise FILE_SYNTAX_ERROR;
    when others =>
      CLOSE (TRUE);
      raise;
  end LOAD_DSCRS;


begin
  -- Help
  begin
    ARGUMENT.GET_PARAMETER (LIST_FILE_NAME, PARAM_KEY => "h");
    TEXT_IO.PUT_LINE ("Usage: " & ARGUMENT.GET_PROGRAM_NAME
                    & " [ -l<afpx_list_file> ] [ -d<destination_dir> ]");
    return;
  exception
    when others =>
      null;
  end;

  -- Source file and dest path arguments
  EXPECTED_ARGS := 0;
  begin
    ARGUMENT.GET_PARAMETER (LIST_FILE_NAME, PARAM_KEY => "l");
    if TEXT_HANDLER.EMPTY (LIST_FILE_NAME) then
      raise ARGUMENT_ERROR;
    end if;
    -- Argument found
    EXPECTED_ARGS := EXPECTED_ARGS + 1;
  exception
    when ARGUMENT.ARGUMENT_NOT_FOUND =>
      TEXT_HANDLER.SET (LIST_FILE_NAME, DEFAULT_LIST_FILE_NAME);
    when others =>
      raise ARGUMENT_ERROR;
  end;

  begin
    ARGUMENT.GET_PARAMETER (AFPX_TYP.DEST_PATH, PARAM_KEY => "d");
    if TEXT_HANDLER.EMPTY (AFPX_TYP.DEST_PATH) then
      raise ARGUMENT_ERROR;
    end if;
    -- Argument found
    EXPECTED_ARGS := EXPECTED_ARGS + 1;
  exception
    when ARGUMENT.ARGUMENT_NOT_FOUND =>
      TEXT_HANDLER.SET (AFPX_TYP.DEST_PATH, ".");
    when others =>
      raise ARGUMENT_ERROR;
  end;

  if ARGUMENT.GET_NBRE_ARG /= EXPECTED_ARGS then
    raise ARGUMENT_ERROR;
  end if;

  TEXT_IO.PUT_LINE ("Reading " & TEXT_HANDLER.VALUE(LIST_FILE_NAME));
  TEXT_IO.PUT_LINE ("Writing in " & TEXT_HANDLER.VALUE(AFPX_TYP.DEST_PATH));
  TEXT_HANDLER.APPEND (AFPX_TYP.DEST_PATH, "/");

  -- First check
  TEXT_IO.PUT_LINE ("Checking:");
  LOAD_DSCRS(TRUE);
  -- Then write
  TEXT_IO.PUT_LINE ("Building:");
  LOAD_DSCRS(FALSE);
  TEXT_IO.PUT_LINE ("Done.");
exception
  when ARGUMENT_ERROR =>
    CLOSE (TRUE);
    TEXT_IO.PUT_LINE ("Argument error. Try -h option.");
  when FILE_NOT_FOUND =>
    CLOSE (TRUE);
    TEXT_IO.PUT_LINE ("Directory or file not found. Try -h option.");
  when FILE_SYNTAX_ERROR =>
    CLOSE (TRUE);
    TEXT_IO.PUT_LINE ("Syntax error.");
  when others =>
    CLOSE (TRUE);
    TEXT_IO.PUT_LINE ("Unexpected exception.");
    raise;
end AFPX_BLD;

