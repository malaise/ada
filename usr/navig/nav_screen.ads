with CON_IO;
with NAV_DATA;
-- all the primitives to access the screen
package NAV_SCREEN is

  -- Where to go after a field is got
  subtype MOVEMENT is CON_IO.CURS_MVT;

  -- Result of get of action to do
  type ACTION is (COMPUTE, QUIT, HELP, CLEAR, PREV, NEXT, REFRESH);

  -- Clear all the screen
  procedure RESET;
  -- To write the title
  procedure TITLE;

  -- To put the mask (all fixed text around) for the get the problem and
  --  for the put of the result
  procedure PUT_MASK;


  -- get a problem data field
  procedure GET (FIELD : in NAV_DATA.T_LIST_DATA; BLINK : in BOOLEAN := FALSE;
   STR : in out STRING; POS : in out POSITIVE; INSERT : in out BOOLEAN;
   NEXT : out MOVEMENT);
  -- put the formated field when successfully got
  procedure PUT (FIELD : in NAV_DATA.T_LIST_DATA; STR : in STRING;
   BLINK : in BOOLEAN := FALSE);

  -- put a field of the result
  procedure PUT_RESULT (FIELD : in NAV_DATA.T_LIST_DATA; STR : in STRING);

  -- draw a line of dots between field in got area and it in result area
  procedure DOT (FIELD : in NAV_DATA.T_LIST_DATA);
  -- draw an arrow between a clear field in got area and the result
  procedure ARROW (FIELD : in NAV_DATA.T_LIST_DATA);
  -- clears a line of dots or an arrow
  procedure CLEAR_LINE (FIELD : in NAV_DATA.T_LIST_DATA);


  -- get an action
  function GET_ACTION return ACTION;

  -- displays the "wrong format" error message
  procedure ERR_FORMAT;
  -- display an error adapted to the detected inconsistency of data
  --  (result of check)
  procedure ERR_CHECK (ERROR : in NAV_DATA.T_CONSISTENCY);
  -- Clears the error message
  procedure CLEAR_ERR;

  -- Ask the operator wether he realy wants to quit
  function CONFIRM_QUIT return BOOLEAN;
  -- displays the help screen
  procedure PUT_HELP;

  -- to stop clock and abort
  procedure ABORT_CLOCK;

end NAV_SCREEN;
