with AFPX, TEXT_HANDLER;
with POINTS, FILE;
package SCREEN is

  -- The get field
  GET_FLD           : constant AFPX.FIELD_RANGE := 9;
  -- The scrool buttons
  subtype LIST_SCROLL_FLD_RANGE is AFPX.FIELD_RANGE range 11 .. 16;
  -- The OK/Cancel buttons
  OK_BUTTON_FLD     : constant AFPX.FIELD_RANGE := 18;
  CANCEL_BUTTON_FLD : constant AFPX.FIELD_RANGE := 19;
  -- The exit/back button
  EXIT_BUTTON_FLD : constant AFPX.FIELD_RANGE := 17;

  -- The F(xxxxx)= field
  YFX_PUT_FLD : constant AFPX.FIELD_RANGE := 30;

  -- All the menues dependant fields
  subtype MENU_FLD_RANGE is AFPX.FIELD_RANGE range 20 .. 31;

  -- Return width of GET field
  function GET_GET_WIDTH return AFPX.WIDTH_RANGE;

  -- Max width of degree
  MAX_DEGREE_WIDTH : constant := 4;

  -- Put a title
  type S_ACTION_LIST is (DATA, READ_POINTS, WRITE_POINTS, NEW_POINTS,
      MODIFY_1, ADD_1, SUPPRESS_1, APPROXIMATE, SORT_POINTS,
      GET_DEGREE, POLYNOM, Y_F_X, SCALES, BOUNDARIES, CURVE, EXIT_APPROX);
  procedure PUT_TITLE (S_ACTION : in S_ACTION_LIST);

  -- Truncate head of string:  "> " & truncated head
  -- Or or padds with spaces
  function PROCUSTE (STR : STRING; LEN : POSITIVE) return STRING;

  -- Put file name
  procedure PUT_FILE (FILE_NAME : in FILE.F_T_FILE_NAME);

  -- Scroll the list
  procedure SCROLL (FLD_NO : in LIST_SCROLL_FLD_RANGE);

  -- Put/hide info. Display error. Confirm
  type S_MESSAGE_LIST is (
      I_CLEAR, I_FILE_NAME, I_X, I_Y, I_XMIN, I_YMIN, I_XMAX, I_YMAX,
      I_DEGREE, I_SCALES, I_WAIT, 
      C_FILE_EXISTS, C_DELETE_POINT, C_GO_ON, C_DATA_LOST,
      E_DONE, E_FILE_NOT_FOUND, E_IO_ERROR, E_FILE_NAME,
      E_NO_DATA, E_WRONG_DEGREE, E_WRONG_COORDINATE,
      E_RESOLUTION_PROBLEM, E_CURVE_PROBLEM, E_CURVE_ACTIVE,
      E_TOO_MANY_POINTS);

  subtype S_INFO_LIST is S_MESSAGE_LIST range I_CLEAR .. I_WAIT;
  subtype S_CONFIRM_LIST is S_MESSAGE_LIST range C_FILE_EXISTS .. C_DATA_LOST;
  subtype S_ERROR_LIST is S_MESSAGE_LIST
                          range E_DONE .. E_TOO_MANY_POINTS;

  procedure INFORM  (MSG : in S_INFO_LIST);
  function  CONFIRM (MSG : S_CONFIRM_LIST; ALERT : BOOLEAN) return BOOLEAN; 
  procedure ERROR   (MSG : in S_ERROR_LIST);


  -- Update number and status of points
  procedure PUT_POINT_STATUS;

  -- Init for file search, for get coordinate...
  procedure INIT_FOR_GET (CURSOR_FIELD : out AFPX.FIELD_RANGE);

  -- Init screen for main menu1
  procedure INIT_FOR_MAIN1 (CURSOR_FIELD : out AFPX.FIELD_RANGE);

  -- Store current file_name for further menus
  -- Put stored file
  procedure STORE_FILE;
  procedure PUT_FILE;

  -- Init screen for main menu2
  procedure INIT_FOR_MAIN2 (CURSOR_FIELD : out AFPX.FIELD_RANGE);

  -- Init screen for main submenu21
  procedure INIT_FOR_MAIN21 (CURSOR_FIELD : out AFPX.FIELD_RANGE);

end SCREEN;

