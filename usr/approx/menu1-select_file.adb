with DIR_MNG;
separate (MENU1)
function SELECT_FILE (FOR_READ : BOOLEAN) return STRING is
  CURSOR_COL : CON_IO.COL_RANGE;
  REDISPLAY : BOOLEAN;
  PTG_RESULT : AFPX.RESULT_REC;
  GET_CONTENT : AFPX.STR_TXT;
  GET_OK      : BOOLEAN;
  DIR_LIST    : DIR_MNG.FILE_LIST_MNG.LIST_TYPE;
  FILE_REC    : DIR_MNG.FILE_ENTRY_REC;
  POS_IN_LIST : NATURAL;
  IS_A_DIR    : BOOLEAN;


  procedure ERROR (MSG : in SCREEN.S_ERROR_LIST) is
  begin
    AFPX.SET_FIELD_PROTECTION (0, TRUE);
    SCREEN.ERROR(MSG);
    AFPX.SET_FIELD_PROTECTION (0, FALSE);
    SCREEN.INIT_FOR_GET(CURSOR_FIELD);
  end ERROR;

  function IS_DIR (FILE : STRING) return BOOLEAN is
    KIND : DIRECTORY.FILE_KIND_LIST;
    RIGHTS : NATURAL;
    FILE_TXT : TEXT_HANDLER.TEXT(DIRECTORY.MAX_DIR_NAME_LEN);
    use DIRECTORY;
  begin
    TEXT_HANDLER.SET (FILE_TXT, FILE);
    DIRECTORY.FILE_STAT(TEXT_HANDLER.VALUE(FILE_TXT), KIND, RIGHTS);
    if KIND = DIRECTORY.SYMBOLIC_LINK then
      DIRECTORY.READ_LINK(TEXT_HANDLER.VALUE(FILE_TXT), FILE_TXT);
      DIRECTORY.FILE_STAT(TEXT_HANDLER.VALUE(FILE_TXT), KIND, RIGHTS);
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
      SCREEN.PUT_FILE ("/*");
    else
      SCREEN.PUT_FILE (DIRECTORY.GET_CURRENT & "/*");
    end if;

    -- Set AFPX list
    -- Get list width
    AFPX.GET_FIELD_SIZE(0, HEIGHT, WIDTH);
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
          SCREEN.PROCUSTE(DIR_ITEM.NAME (1 .. DIR_ITEM.LEN) & ' ' & CHAR,
                          WIDTH);
      AFPX.LINE_LIST_MNG.INSERT (AFPX.LINE_LIST, AFPX_ITEM);
      exit when DIR_MNG.FILE_LIST_MNG.GET_POSITION (DIR_LIST)
           = DIR_MNG.FILE_LIST_MNG.LIST_LENGTH (DIR_LIST);
      DIR_MNG.FILE_LIST_MNG.MOVE_TO (DIR_LIST);
    end loop;
    -- Move to beginning of AFPX list
    AFPX.LINE_LIST_MNG.MOVE_TO (AFPX.LINE_LIST, AFPX.LINE_LIST_MNG.NEXT,
       0, FALSE);

  end CHANGE_DIR;

begin
  AFPX.USE_DESCRIPTOR(2);
  SCREEN.INIT_FOR_GET(CURSOR_FIELD);
  SCREEN.INFORM (SCREEN.I_FILE_NAME);
  -- Title
  if FOR_READ then
    SCREEN.PUT_TITLE(SCREEN.READ_POINTS);
  else
    SCREEN.PUT_TITLE(SCREEN.WRITE_POINTS);
  end if;
  -- Encode current file name in get field
  if TEXT_HANDLER.LENGTH(FILE_NAME_TXT) <= SCREEN.GET_GET_WIDTH then
    TEXT_HANDLER.SET (GET_CONTENT,
       SCREEN.PROCUSTE(TEXT_HANDLER.VALUE(FILE_NAME_TXT), SCREEN.GET_GET_WIDTH));
  end if;
  AFPX.ENCODE_FIELD (SCREEN.GET_FLD, (0, 0), GET_CONTENT);
  
  -- Set Nb of points and save_status
  SCREEN.PUT_POINT_STATUS;

  -- File name
  CHANGE_DIR (".");

  REDISPLAY := FALSE;
  loop

    AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
    REDISPLAY := FALSE;
    case PTG_RESULT.EVENT is
      when AFPX.KEYBOARD =>
        case PTG_RESULT.KEYBOARD_KEY is
          when AFPX.RETURN_KEY =>
            AFPX.DECODE_FIELD (SCREEN.GET_FLD, 0, GET_CONTENT);
            DIALOG.PARSE_SPACES (GET_CONTENT, GET_OK);
            if not GET_OK then
              ERROR (SCREEN.E_FILE_NAME);
            else
              begin
                IS_A_DIR := IS_DIR (TEXT_HANDLER.VALUE(GET_CONTENT));
                if IS_A_DIR then 
                  AFPX.CLEAR_FIELD (SCREEN.GET_FLD);
                  CHANGE_DIR(TEXT_HANDLER.VALUE(GET_CONTENT));
                else 
                  exit;
                end if;
              exception
                when DIRECTORY.NAME_ERROR =>
                  -- File not found
                  if FOR_READ then
                    -- Read non existing file
                    ERROR (SCREEN.E_FILE_NOT_FOUND);
                  else
                    -- Save on new file
                    exit;
                  end if;
                when others =>
                  ERROR (SCREEN.E_IO_ERROR);
              end;
            end if;
          when AFPX.ESCAPE_KEY =>
            TEXT_HANDLER.EMPTY(GET_CONTENT);
            exit;
          when AFPX.BREAK_KEY =>
            null;
        end case;

      when AFPX.MOUSE_BUTTON =>
        case PTG_RESULT.FIELD_NO is
          when SCREEN.LIST_SCROLL_FLD_RANGE'FIRST ..
               SCREEN.LIST_SCROLL_FLD_RANGE'LAST =>
            SCREEN.SCROLL(PTG_RESULT.FIELD_NO);
          when SCREEN.OK_BUTTON_FLD =>
            POS_IN_LIST := AFPX.LINE_LIST_MNG.GET_POSITION(AFPX.LINE_LIST);
            DIR_MNG.FILE_LIST_MNG.MOVE_TO(DIR_LIST,
                   NUMBER => POS_IN_LIST - 1,
                   FROM_CURRENT => FALSE);
            DIR_MNG.FILE_LIST_MNG.READ(DIR_LIST, FILE_REC, 
                   DIR_MNG.FILE_LIST_MNG.CURRENT);
            TEXT_HANDLER.SET(GET_CONTENT, FILE_REC.NAME(1 .. FILE_REC.LEN));
            begin
              if IS_DIR (TEXT_HANDLER.VALUE(GET_CONTENT)) then
                AFPX.CLEAR_FIELD (SCREEN.GET_FLD);
                CHANGE_DIR(TEXT_HANDLER.VALUE(GET_CONTENT));
              else 
                exit;
              end if;
            exception
              when others =>
                ERROR (SCREEN.E_IO_ERROR);
            end;
          when SCREEN.CANCEL_BUTTON_FLD =>
            TEXT_HANDLER.EMPTY(GET_CONTENT);
            exit;
          when others => null;
        end case;
      when AFPX.REFRESH =>
        REDISPLAY := TRUE;
    end case;
  end loop;

  DIR_MNG.FILE_LIST_MNG.DELETE_LIST(DIR_LIST);
  return TEXT_HANDLER.VALUE(GET_CONTENT);

end SELECT_FILE;
