with CON_IO, AFPX, NORMAL, X_MNG;
with POINTS, RESOL;
package body SCREEN is

  TITLE_FLD : constant AFPX.FIELD_RANGE := 2;
  FILE_FLD : constant AFPX.FIELD_RANGE := 4;
  NB_POINT_FLD : constant AFPX.FIELD_RANGE := 5;
  STATE_POINT_FLD : constant AFPX.FIELD_RANGE := 7;
  INFO_FLD : constant AFPX.FIELD_RANGE := 10;
  
  DEGREE_FLD : constant AFPX.FIELD_RANGE := 21;

  STORED_FILE_NAME : TEXT_HANDLER.TEXT (AFPX.WIDTH_RANGE'LAST);

  GET_WIDTH : NATURAL := 0;

  -- Return width of GET field
  function GET_GET_WIDTH return AFPX.WIDTH_RANGE is
    HEIGHT : AFPX.HEIGHT_RANGE;
  begin
    if GET_WIDTH = 0 then
      AFPX.GET_FIELD_SIZE (GET_FLD, HEIGHT, GET_WIDTH);
    end if;
    return GET_WIDTH;
  end GET_GET_WIDTH;


  procedure PUT_TITLE (S_ACTION : in S_ACTION_LIST) is

    procedure ENCODE_TITLE (MSG : in STRING) is
    begin
      AFPX.CLEAR_FIELD (TITLE_FLD);
      AFPX.ENCODE_FIELD (TITLE_FLD, (0, 0), MSG);
    end ENCODE_TITLE;

  begin
    case S_ACTION is
      when DATA         => ENCODE_TITLE("Data management");
      when READ_POINTS  => ENCODE_TITLE("Load a file of points");
      when WRITE_POINTS => ENCODE_TITLE("Save points in a file");
      when NEW_POINTS   => ENCODE_TITLE("Clear current points");
      when MODIFY_1     => ENCODE_TITLE("Modify a point");
      when ADD_1        => ENCODE_TITLE("Add a new point");
      when SUPPRESS_1   => ENCODE_TITLE("Delete a point");
      when APPROXIMATE  => ENCODE_TITLE("Data approximation");
      when SORT_POINTS  => ENCODE_TITLE("Sort points");
      when GET_DEGREE   => ENCODE_TITLE("Set the degree");
      when POLYNOM      => ENCODE_TITLE("Compute polynom");
      when Y_F_X        => ENCODE_TITLE("Compute Y from X");
      when SCALES       => ENCODE_TITLE("Set scales type");
      when BOUNDARIES   => ENCODE_TITLE("Set scales boundaries");
      when CURVE        => ENCODE_TITLE("Draw curve");
      when EXIT_APPROX  => ENCODE_TITLE("Exit approx");
    end case;
  end PUT_TITLE;

  -- Truncate head of string:  "> " & truncated head
  -- Or or padds with spaces
  function PROCUSTE (STR : STRING; LEN : POSITIVE) return STRING is
    RES : STRING (1 .. LEN);
  begin
    if STR'LENGTH <= LEN then
      RES (1 .. STR'LENGTH) := STR;
      RES (STR'LENGTH + 1 .. LEN) := (others => ' ');
    else
      RES (1 .. 2) := "> ";
      RES (3 .. LEN) := STR (STR'LAST - LEN + 3 .. STR'LAST);
    end if;
    return RES;
  end PROCUSTE;


  -- Put file name
  procedure PUT_FILE (FILE_NAME : in FILE.F_T_FILE_NAME) is
    HEIGHT : AFPX.HEIGHT_RANGE;
    WIDTH  : AFPX.WIDTH_RANGE;
  begin
    AFPX.GET_FIELD_SIZE(FILE_FLD, HEIGHT, WIDTH);
    AFPX.ENCODE_FIELD(FILE_FLD, (0, 0), PROCUSTE(FILE_NAME, WIDTH));
  end PUT_FILE;

  -- Scroll the list
  procedure SCROLL (FLD_NO : in LIST_SCROLL_FLD_RANGE) is
  begin
    case FLD_NO is
      when 11 => AFPX.UPDATE_LIST(AFPX.TOP);
      when 12 => AFPX.UPDATE_LIST(AFPX.PAGE_UP);
      when 13 => AFPX.UPDATE_LIST(AFPX.UP);
      when 14 => AFPX.UPDATE_LIST(AFPX.DOWN);
      when 15 => AFPX.UPDATE_LIST(AFPX.PAGE_DOWN);
      when 16 => AFPX.UPDATE_LIST(AFPX.BOTTOM);
    end case;
  end SCROLL;

  -- Clear all menu dependant fields
  procedure CLEAR_MENU is
  begin
    -- Inhibit all menu dependant fields
    for I in MENU_FLD_RANGE loop
      AFPX.SET_FIELD_ACTIVATION (I, FALSE);
    end loop;
  end CLEAR_MENU;

  -- Encode in info field
  procedure ENCODE_INFO (MSG : in STRING) is
  begin
    AFPX.CLEAR_FIELD (INFO_FLD);
    AFPX.ENCODE_FIELD (INFO_FLD, (0, 0), MSG);
  end ENCODE_INFO;

  -- Ptg on OK (and cancel) buttons
  function S_CONFIRM return BOOLEAN is
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
            when OK_BUTTON_FLD =>
              RES := TRUE;
              exit;
            when CANCEL_BUTTON_FLD =>
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
  end S_CONFIRM;


  procedure INFORM (MSG : in S_INFO_LIST) is
    LIST_ACTIVE : BOOLEAN;
  begin
    case MSG is
      when I_CLEAR     => ENCODE_INFO ("");
      when I_FILE_NAME => ENCODE_INFO ("Select or enter file name");
      when I_X         => ENCODE_INFO ("Enter X");
      when I_Y         => ENCODE_INFO ("Enter Y");
      when I_XMIN      => ENCODE_INFO ("Enter X min");
      when I_YMIN      => ENCODE_INFO ("Enter Y min");
      when I_XMAX      => ENCODE_INFO ("Enter X max");
      when I_YMAX      => ENCODE_INFO ("Enter Y max");
      when I_DEGREE    => ENCODE_INFO ("Enter degree from 0 to Npoints - 1");
      when I_SCALES    => ENCODE_INFO ("Select a scales kind");
      when I_WAIT      =>
        ENCODE_INFO ("Computing, please wait");
        AFPX.GET_FIELD_ACTIVATION (AFPX.LIST_FIELD_NO, LIST_ACTIVE);
        if LIST_ACTIVE then
          AFPX.SET_FIELD_ACTIVATION (AFPX.LIST_FIELD_NO, FALSE);
        end if;
        AFPX.PUT;
        if LIST_ACTIVE then
          AFPX.SET_FIELD_ACTIVATION (AFPX.LIST_FIELD_NO, TRUE);
        end if;
    end case;
  end INFORM;


  function CONFIRM (MSG : S_CONFIRM_LIST) return BOOLEAN is
    RES : BOOLEAN;
  begin
    -- No menu. Ok or cancel
    CLEAR_MENU;
    -- Inhibit exit field
    AFPX.SET_FIELD_ACTIVATION (EXIT_BUTTON_FLD, FALSE);
    AFPX.SET_FIELD_ACTIVATION(OK_BUTTON_FLD, TRUE);
    AFPX.SET_FIELD_ACTIVATION(CANCEL_BUTTON_FLD, TRUE);
    -- Set colors
    AFPX.SET_FIELD_COLORS(INFO_FLD, FOREGROUND => CON_IO.ORANGE,
                                    BLINK_STAT => CON_IO.BLINK);
    CON_IO.BELL(1);
    case MSG is
      when C_FILE_EXISTS  =>  ENCODE_INFO ("File exists and will be overwritten");
      when C_DELETE_POINT =>  ENCODE_INFO ("Delete this point");
      when C_DATA_LOST    =>  ENCODE_INFO ("Data is not saved and will be lost");
    end case;
    RES := S_CONFIRM;
    -- Reset default colors
    AFPX.RESET_FIELD(INFO_FLD);
    return RES;
  end CONFIRM;


  procedure ERROR (MSG : in S_ERROR_LIST) is
    RES : BOOLEAN;
  begin
    -- No menu. Ok
    CLEAR_MENU;
    -- Inhibit exit field
    AFPX.SET_FIELD_ACTIVATION(EXIT_BUTTON_FLD, FALSE);
    AFPX.SET_FIELD_ACTIVATION(OK_BUTTON_FLD, TRUE);
    AFPX.SET_FIELD_ACTIVATION(CANCEL_BUTTON_FLD, FALSE);
    -- Set colors
    AFPX.SET_FIELD_COLORS(INFO_FLD, FOREGROUND => CON_IO.ORANGE,
                                    BLINK_STAT => CON_IO.BLINK);
    if MSG /= E_DONE then
      CON_IO.BELL(1);
    end if;
    case MSG is
      when E_DONE               => ENCODE_INFO ("");
      when E_FILE_NOT_FOUND     => ENCODE_INFO ("File not found");
      when E_IO_ERROR           => ENCODE_INFO ("Error accessing file");
      when E_FILE_NAME          => ENCODE_INFO ("Error invalid file name");
      when E_NO_DATA            => ENCODE_INFO ("Error, no data");
      when E_WRONG_DEGREE       => ENCODE_INFO ("Error, invalid degree");
      when E_WRONG_COORDINATE   => ENCODE_INFO ("Error, invalid coordinate");
      when E_RESOLUTION_PROBLEM => ENCODE_INFO ("Internal error while solving");
      when E_CURVE_PROBLEM      => ENCODE_INFO ("Internal error while drawing");
      when E_CURVE_ACTIVE       => ENCODE_INFO ("A curve is already active");
      when E_TOO_MANY_POINTS    => ENCODE_INFO ("Too many points");
    end case;
    RES := S_CONFIRM;
    -- Reset default colors
    AFPX.RESET_FIELD(INFO_FLD);
  end ERROR;


  procedure PUT_POINT_STATUS is
    -- Width of nb_point
    HEIGHT : AFPX.HEIGHT_RANGE;
    WIDTH  : AFPX.WIDTH_RANGE;
  begin
    AFPX.GET_FIELD_SIZE(NB_POINT_FLD, HEIGHT, WIDTH);
    AFPX.ENCODE_FIELD(NB_POINT_FLD, (0, 0), NORMAL(POINTS.P_NB, WIDTH));
    if POINTS.P_SAVED then
      AFPX.CLEAR_FIELD(STATE_POINT_FLD);
    else
      AFPX.RESET_FIELD(STATE_POINT_FLD);
    end if;
  end PUT_POINT_STATUS;

 

  procedure INIT_FOR_MAIN1 (CURSOR_FIELD : out AFPX.FIELD_RANGE) is
  begin
    -- Disable OK & cancel
    AFPX.SET_FIELD_ACTIVATION(OK_BUTTON_FLD, FALSE);
    AFPX.SET_FIELD_ACTIVATION(CANCEL_BUTTON_FLD, FALSE);
    -- Disable GET
    AFPX.SET_FIELD_ACTIVATION(GET_FLD, FALSE); 
    -- So whatever cursor field
    CURSOR_FIELD := 1;
    PUT_TITLE(DATA);
    PUT_POINT_STATUS;
  end INIT_FOR_MAIN1;

  -- Init for file search
  procedure INIT_FOR_GET (CURSOR_FIELD : out AFPX.FIELD_RANGE) is
  begin
    -- No menu.
    CLEAR_MENU;
    -- Inhibit exit field
    AFPX.SET_FIELD_ACTIVATION (EXIT_BUTTON_FLD, FALSE);
    -- GET, Ok or cancel
    AFPX.SET_FIELD_ACTIVATION(GET_FLD, TRUE);
    AFPX.SET_FIELD_ACTIVATION(OK_BUTTON_FLD, TRUE);
    AFPX.SET_FIELD_ACTIVATION(CANCEL_BUTTON_FLD, TRUE);
    CURSOR_FIELD := GET_FLD;
  end INIT_FOR_GET;

  -- Store current file_name for further menus
  -- Put stored file
  procedure STORE_FILE is
  begin
    AFPX.DECODE_FIELD (FILE_FLD, 0, STORED_FILE_NAME);
  end STORE_FILE;

  procedure PUT_FILE is
  begin
    PUT_FILE (TEXT_HANDLER.VALUE(STORED_FILE_NAME));
  end PUT_FILE;

  procedure PUT_DEGREE is
  begin
    AFPX.ENCODE_FIELD (DEGREE_FLD, (0, 0),  NORMAL (RESOL.R_DEGREE, MAX_DEGREE_WIDTH));
  end PUT_DEGREE;

  procedure INIT_FOR_MAIN2 (CURSOR_FIELD : out AFPX.FIELD_RANGE) is
  begin
    -- Disable OK & cancel
    AFPX.SET_FIELD_ACTIVATION(OK_BUTTON_FLD, FALSE);
    AFPX.SET_FIELD_ACTIVATION(CANCEL_BUTTON_FLD, FALSE);
    -- Disable GET
    AFPX.SET_FIELD_ACTIVATION(GET_FLD, FALSE); 
    -- So whatever cursor field
    CURSOR_FIELD := 1;
    -- Lock points
    AFPX.SET_FIELD_PROTECTION (AFPX.LIST_FIELD_NO, TRUE);
    PUT_TITLE(APPROXIMATE);
    PUT_POINT_STATUS;
    PUT_DEGREE;
  end INIT_FOR_MAIN2;

  procedure INIT_FOR_MAIN21 (CURSOR_FIELD : out AFPX.FIELD_RANGE) is
  begin
    -- Disallow CANCEL
    AFPX.SET_FIELD_ACTIVATION(CANCEL_BUTTON_FLD, FALSE);
    -- Disable GET
    AFPX.SET_FIELD_ACTIVATION(GET_FLD, FALSE); 
    -- So whatever cursor field
    CURSOR_FIELD := 1;
    -- Lock points
    AFPX.SET_FIELD_PROTECTION (AFPX.LIST_FIELD_NO, TRUE);
    PUT_TITLE(BOUNDARIES);
    PUT_POINT_STATUS;
  end INIT_FOR_MAIN21;

end SCREEN;

