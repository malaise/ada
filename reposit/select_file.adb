with TEXT_HANDLER, CON_IO, AFPX, DIRECTORY, DIR_MNG, STRING_MNG;

function SELECT_FILE (DESCRIPTOR : AFPX.DESCRIPTOR_RANGE;
                      CURRENT_FILE : STRING;
                      FOR_READ : BOOLEAN) return STRING is

  CURSOR_FIELD : AFPX.FIELD_RANGE;
  CURSOR_COL   : CON_IO.COL_RANGE;
  REDISPLAY    : BOOLEAN;
  PTG_RESULT   : AFPX.RESULT_REC;

  -- Text/get fields
  -- 1 is the fixed title
  TITLE_FLD    : constant AFPX.FIELD_RANGE := 2;
  FILE_FLD     : constant AFPX.FIELD_RANGE := 4;
  GET_FLD      : constant AFPX.FIELD_RANGE := 5;
  INFO_FLD     : constant AFPX.FIELD_RANGE := 6;
  -- The scroll buttons
  subtype LIST_SCROLL_FLD_RANGE is AFPX.FIELD_RANGE range 7 .. 12;
  -- Action buttons
  REREAD_FLD   : constant AFPX.FIELD_RANGE := 13;
  OK_FLD       : constant AFPX.FIELD_RANGE := 14;
  CANCEL_FLD   : constant AFPX.FIELD_RANGE := 15;

  type ERROR_LIST is (E_FILE_NOT_FOUND, E_IO_ERROR, E_FILE_NAME);

  GET_WIDTH    : NATURAL := 0;
  GET_CONTENT  : AFPX.STR_TXT;
  GET_OK       : BOOLEAN;
  DIR_LIST     : DIR_MNG.FILE_LIST_MNG.LIST_TYPE;
  FILE_REC     : DIR_MNG.FILE_ENTRY_REC;
  VALID        : BOOLEAN;
  POS_IN_LIST  : NATURAL;
  IS_A_DIR     : BOOLEAN;


  -- Return width of GET field
  function GET_GET_WIDTH return AFPX.WIDTH_RANGE is
    HEIGHT : AFPX.HEIGHT_RANGE;
  begin
    if GET_WIDTH = 0 then
      AFPX.GET_FIELD_SIZE (GET_FLD, HEIGHT, GET_WIDTH);
    end if;
    return GET_WIDTH;
  end GET_GET_WIDTH;

  -- Remove trailing spaces. No heading nor intermediate spaces allowed
  procedure PARSE_SPACES (TXT : in out TEXT_HANDLER.TEXT;
                          OK : out BOOLEAN) is
    STR : constant STRING := TEXT_HANDLER.VALUE(TXT);
    L : NATURAL;
  begin
    L := 0;
    for I in reverse STR'RANGE loop
      if STR(I) /= ' ' and then STR(I) /= ASCII.HT then
        -- Significant char
        if L = 0 then
          L := I;
        end if;
      else
        -- space
        if L /= 0 then
          -- Space before significant char
          OK := FALSE;
          return;
        end if;
      end if;
    end loop;
    -- If all spaces, L = 0 => empty
    TEXT_HANDLER.SET (TXT, STR(1 .. L));
    OK := TRUE;
  end PARSE_SPACES;

  -- Put file name
  procedure PUT_FILE (FILE_NAME : in STRING) is
    HEIGHT : AFPX.HEIGHT_RANGE;
    WIDTH  : AFPX.WIDTH_RANGE;
  begin
    AFPX.GET_FIELD_SIZE(FILE_FLD, HEIGHT, WIDTH);
    AFPX.ENCODE_FIELD(FILE_FLD, (0, 0), 
      STRING_MNG.PROCUSTE(FILE_NAME, WIDTH));
  end PUT_FILE;

  -- Encode in info field
  procedure ENCODE_INFO (STR : in STRING) is
  begin
    AFPX.CLEAR_FIELD (INFO_FLD);
    AFPX.ENCODE_FIELD (INFO_FLD, (0, 0), STR);
  end ENCODE_INFO;

  -- Scroll the list
  procedure SCROLL (FLD_NO : in LIST_SCROLL_FLD_RANGE) is
  begin
    case FLD_NO is
      when 07 => AFPX.UPDATE_LIST(AFPX.TOP);
      when 08 => AFPX.UPDATE_LIST(AFPX.PAGE_UP);
      when 09 => AFPX.UPDATE_LIST(AFPX.UP);
      when 10 => AFPX.UPDATE_LIST(AFPX.DOWN);
      when 11 => AFPX.UPDATE_LIST(AFPX.PAGE_DOWN);
      when 12 => AFPX.UPDATE_LIST(AFPX.BOTTOM);
    end case;
  end SCROLL;

  -- Ptg on OK (and cancel) buttons
  function CONFIRM return BOOLEAN is
    CURSOR_FIELD : AFPX.FIELD_RANGE := 1;
    CURSOR_COL : CON_IO.COL_RANGE := 0;
    REDISPLAY : BOOLEAN := FALSE;
    PTG_RESULT : AFPX.RESULT_REC;
    GET_PROT : BOOLEAN;
    GET_ACT : BOOLEAN;
    RES : BOOLEAN;
  begin
    -- Protect get field
    AFPX.GET_FIELD_PROTECTION (GET_FLD, GET_PROT);
    if not GET_PROT then
      AFPX.SET_FIELD_PROTECTION (GET_FLD, TRUE);
    end if;
    AFPX.SET_FIELD_COLORS(GET_FLD, BACKGROUND => CON_IO.BLACK);
    loop
      AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
      REDISPLAY := FALSE;
      case PTG_RESULT.EVENT is
        when AFPX.KEYBOARD =>
          case PTG_RESULT.KEYBOARD_KEY is
            when AFPX.RETURN_KEY =>
              RES := TRUE;
              exit;
            when AFPX.ESCAPE_KEY =>
              RES := FALSE;
              exit;
            when AFPX.BREAK_KEY =>
              null;
          end case;
        when AFPX.MOUSE_BUTTON =>
          case PTG_RESULT.FIELD_NO is
            when LIST_SCROLL_FLD_RANGE'FIRST .. LIST_SCROLL_FLD_RANGE'LAST =>
              SCROLL(PTG_RESULT.FIELD_NO);
            when OK_FLD =>
              RES := TRUE;
              exit;
            when CANCEL_FLD =>
              RES := FALSE;
              exit;
            when others =>
              null;
          end case;
        when AFPX.REFRESH =>
          REDISPLAY := TRUE;
      end case;
    end loop;
    -- Restore GET field
    AFPX.GET_FIELD_ACTIVATION (GET_FLD, GET_ACT);
    AFPX.RESET_FIELD (GET_FLD, RESET_STRING => FALSE);
    AFPX.SET_FIELD_ACTIVATION (GET_FLD, GET_ACT);
    if not GET_PROT then
      AFPX.SET_FIELD_PROTECTION (GET_FLD, FALSE);
    end if;
    return RES;
  end CONFIRM;


  procedure ERROR (MSG : in ERROR_LIST) is
    RES : BOOLEAN;
  begin
    CON_IO.BELL(1);
    AFPX.SET_FIELD_PROTECTION (AFPX.LIST_FIELD_NO, TRUE);
    AFPX.SET_FIELD_ACTIVATION (REREAD_FLD, FALSE);
    AFPX.SET_FIELD_ACTIVATION (CANCEL_FLD, FALSE);
    AFPX.SET_FIELD_COLORS(INFO_FLD, FOREGROUND => CON_IO.ORANGE,
                                    BLINK_STAT => CON_IO.BLINK);
    case MSG is
      when E_FILE_NOT_FOUND     => ENCODE_INFO ("File not found");
      when E_IO_ERROR           => ENCODE_INFO ("Error accessing file");
      when E_FILE_NAME          => ENCODE_INFO ("Error invalid file name");
    end case;

    RES := CONFIRM;

    AFPX.RESET_FIELD(INFO_FLD);
    AFPX.SET_FIELD_ACTIVATION (CANCEL_FLD, TRUE);
    AFPX.SET_FIELD_ACTIVATION (REREAD_FLD, TRUE);
    AFPX.SET_FIELD_PROTECTION (AFPX.LIST_FIELD_NO, FALSE);
    CURSOR_FIELD := GET_FLD;
  end ERROR;

  function IS_DIR (FILE : STRING) return BOOLEAN is
    KIND : DIRECTORY.FILE_KIND_LIST;
    RIGHTS : NATURAL;
    MTIME : DIRECTORY.TIME_T;
    FILE_TXT : TEXT_HANDLER.TEXT(DIRECTORY.MAX_DIR_NAME_LEN);
    use DIRECTORY;
  begin
    TEXT_HANDLER.SET (FILE_TXT, FILE);
    DIRECTORY.FILE_STAT(TEXT_HANDLER.VALUE(FILE_TXT), KIND, RIGHTS, MTIME);
    if KIND = DIRECTORY.SYMBOLIC_LINK then
      DIRECTORY.READ_LINK(TEXT_HANDLER.VALUE(FILE_TXT), FILE_TXT);
      DIRECTORY.FILE_STAT(TEXT_HANDLER.VALUE(FILE_TXT), KIND, RIGHTS, MTIME);
    end if;
    return KIND = DIRECTORY.DIR;
  end IS_DIR;

  procedure CHANGE_DIR (NEW_DIR : in STRING) is
    HEIGHT : AFPX.HEIGHT_RANGE;
    WIDTH  : AFPX.WIDTH_RANGE;
    DIR_ITEM : DIR_MNG.FILE_ENTRY_REC;
    CHAR : CHARACTER;
    AFPX_ITEM : AFPX.LINE_REC;
  begin
    -- Clear get field
    CURSOR_COL := 0;

    -- change dir
    DIRECTORY.CHANGE_CURRENT(NEW_DIR);
    -- Title
    if DIRECTORY.GET_CURRENT = "/" then
      PUT_FILE ("/*");
    else
      PUT_FILE (DIRECTORY.GET_CURRENT & "/*");
    end if;

    -- Set AFPX list
    -- Get list width
    AFPX.GET_FIELD_SIZE(AFPX.LIST_FIELD_NO, HEIGHT, WIDTH);
    -- Read dir and move to first
    DIR_MNG.FILE_LIST_MNG.DELETE_LIST (DIR_LIST);
    DIR_MNG.LIST_DIR (DIR_LIST, ".");
    DIR_MNG.FILE_SORT (DIR_LIST);
    DIR_MNG.FILE_LIST_MNG.MOVE_TO (DIR_LIST, DIR_MNG.FILE_LIST_MNG.NEXT,
                                 0 , FALSE);
    -- Clear AFPX list
    AFPX.LINE_LIST_MNG.DELETE_LIST(AFPX.LINE_LIST);
    loop
      DIR_MNG.FILE_LIST_MNG.READ (DIR_LIST, DIR_ITEM,
                                  DIR_MNG.FILE_LIST_MNG.CURRENT);
      case DIR_ITEM.KIND is
        when DIRECTORY.FILE =>
          CHAR := ' ';
        when DIRECTORY.DIR =>
          CHAR := '/';
        when DIRECTORY.SYMBOLIC_LINK =>
          CHAR := '@';
        when DIRECTORY.BLOCK_DEVICE | DIRECTORY.CHARACTER_DEVICE =>
          CHAR := '>';
        when DIRECTORY.FIFO =>
          CHAR := '|';
        when DIRECTORY.SOCKET =>
          CHAR := '=';
        when DIRECTORY.UNKNOWN =>
          -- device, fifo ...
          CHAR := '?';
      end case;
      AFPX_ITEM.LEN := WIDTH;
      AFPX_ITEM.STR (1 .. WIDTH) :=
        STRING_MNG.PROCUSTE(DIR_ITEM.NAME (1 .. DIR_ITEM.LEN) & ' ' & CHAR,
                            WIDTH);
      AFPX.LINE_LIST_MNG.INSERT (AFPX.LINE_LIST, AFPX_ITEM);
      exit when DIR_MNG.FILE_LIST_MNG.GET_POSITION (DIR_LIST)
           = DIR_MNG.FILE_LIST_MNG.LIST_LENGTH (DIR_LIST);
      DIR_MNG.FILE_LIST_MNG.MOVE_TO (DIR_LIST);
    end loop;
    -- Move to beginning of AFPX list
    AFPX.LINE_LIST_MNG.MOVE_TO (AFPX.LINE_LIST, AFPX.LINE_LIST_MNG.NEXT,
       0, FALSE);
    AFPX.UPDATE_LIST(AFPX.TOP);

  end CHANGE_DIR;

begin
  AFPX.USE_DESCRIPTOR(DESCRIPTOR);

  -- Call client specific init
  INIT_PROCEDURE;

  AFPX.ENCODE_FIELD (INFO_FLD, (0, 0), "Select or enter file name");
  -- Title
  if FOR_READ then
    AFPX.ENCODE_FIELD (TITLE_FLD, (0, 0), "Load a file");
  else
    AFPX.ENCODE_FIELD (TITLE_FLD, (0, 0), "Save in a file");
  end if;

  -- Encode current file name in get field
  if CURRENT_FILE'LENGTH <= GET_GET_WIDTH then
    TEXT_HANDLER.SET (GET_CONTENT, 
      STRING_MNG.PROCUSTE(CURRENT_FILE, GET_GET_WIDTH));
  else
    TEXT_HANDLER.EMPTY(GET_CONTENT);
  end if;
  AFPX.ENCODE_FIELD (GET_FLD, (0, 0), GET_CONTENT);

  

  -- File name
  CHANGE_DIR (".");

  CURSOR_FIELD := GET_FLD;
  REDISPLAY := FALSE;
  loop

    AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
    REDISPLAY := FALSE;
    case PTG_RESULT.EVENT is
      when AFPX.KEYBOARD =>
        case PTG_RESULT.KEYBOARD_KEY is
          when AFPX.RETURN_KEY =>
            AFPX.DECODE_FIELD (GET_FLD, 0, GET_CONTENT);
            PARSE_SPACES (GET_CONTENT, GET_OK);
            GET_OK := GET_OK and then not TEXT_HANDLER.EMPTY(GET_CONTENT);
            if not GET_OK then
              ERROR (E_FILE_NAME);
            else
              -- Value to return if not dir
              FILE_REC.LEN := TEXT_HANDLER.LENGTH(GET_CONTENT);
              FILE_REC.NAME(1 .. FILE_REC.LEN) :=
                        TEXT_HANDLER.VALUE(GET_CONTENT);
              begin
                IS_A_DIR := IS_DIR (TEXT_HANDLER.VALUE(GET_CONTENT));
                if IS_A_DIR then 
                  -- Change dir
                  AFPX.CLEAR_FIELD (GET_FLD);
                  CHANGE_DIR(TEXT_HANDLER.VALUE(GET_CONTENT));
                else 
                  -- Valid file entered
                  VALID := TRUE;
                  exit;
                end if;
              exception
                when DIRECTORY.NAME_ERROR =>
                  -- File not found
                  if FOR_READ then
                    -- Read non existing file
                    ERROR (E_FILE_NOT_FOUND);
                  else
                    -- Save on new file
                    VALID := TRUE;
                    exit;
                  end if;
                when others =>
                  ERROR (E_IO_ERROR);
              end;
            end if;
          when AFPX.ESCAPE_KEY =>
            VALID := FALSE;
            exit;
          when AFPX.BREAK_KEY =>
            null;
        end case;

      when AFPX.MOUSE_BUTTON =>
        case PTG_RESULT.FIELD_NO is
          when LIST_SCROLL_FLD_RANGE'FIRST .. LIST_SCROLL_FLD_RANGE'LAST =>
            SCROLL(PTG_RESULT.FIELD_NO);

          -- Ok button or double click in list
          when OK_FLD | AFPX.LIST_FIELD_NO =>
            POS_IN_LIST := AFPX.LINE_LIST_MNG.GET_POSITION(AFPX.LINE_LIST);
            DIR_MNG.FILE_LIST_MNG.MOVE_TO(DIR_LIST,
                   NUMBER => POS_IN_LIST - 1,
                   FROM_CURRENT => FALSE);
            DIR_MNG.FILE_LIST_MNG.READ(DIR_LIST, FILE_REC, 
                   DIR_MNG.FILE_LIST_MNG.CURRENT);
            begin
              if IS_DIR (FILE_REC.NAME(1 .. FILE_REC.LEN)) then
                AFPX.CLEAR_FIELD (GET_FLD);
                CHANGE_DIR(FILE_REC.NAME(1 .. FILE_REC.LEN));
              else 
                -- File selected
                VALID := TRUE;
                exit;
              end if;
            exception
              when others =>
                ERROR (E_IO_ERROR);
            end;

          when CANCEL_FLD =>
            VALID := FALSE;
            exit;
          when REREAD_FLD =>
            -- Reread current directory
            CHANGE_DIR(".");
          when others => null;
        end case;
      when AFPX.REFRESH =>
        REDISPLAY := TRUE;
    end case;
  end loop;
 
  DIR_MNG.FILE_LIST_MNG.DELETE_LIST(DIR_LIST);
  if VALID then
    return FILE_REC.NAME(1 .. FILE_REC.LEN);
  else
    return "";
  end if;

end SELECT_FILE;
