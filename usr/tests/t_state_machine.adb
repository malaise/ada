with MY_IO;
with STATE_MACHINE;
procedure T_STATE_MACHINE is

  type STATE_LIST is (UNKNOWN, STARTING, FAILED, DETACHED, OK, ERROR);

  type EVENT_LIST is (TRUE, DEFAULT, START, FAILURE, SUCCESS, ATTACH, DETACH);

  procedure DISPLAY_TRANSITION (PREV_STATE : in STATE_LIST;
                                EVENT : in EVENT_LIST;
                                NEW_STATE : in STATE_LIST);


  package MSM is new STATE_MACHINE (STATE_LIST, EVENT_LIST, DISPLAY_TRANSITION);
  use MSM;

  CUR_STATE : STATE_LIST;
  EVENT : EVENT_LIST;
  VALID_EVENT : BOOLEAN;

  function GET_EVENT return EVENT_LIST is
    EVENT : EVENT_LIST;
    STR : STRING (1 .. 500);
    LEN : NATURAL;
  begin
    for E in EVENT_LIST loop
      MY_IO.PUT (EVENT_LIST'IMAGE(E) & " ");
    end loop;
    MY_IO.PUT (" ? ");
    MY_IO.GET_LINE (STR, LEN);
    EVENT := EVENT_LIST'VALUE (STR(1 .. LEN));
    return EVENT;
  end GET_EVENT;


  procedure PUT_TRANSITION (TRANSITION : in TRANSITION_REC) is
    procedure PUTS (STR : in STRING) is
      STRMAX : STRING(1 .. 8) := (others => ' ');
    begin
      if STR'LENGTH <= STRMAX'LENGTH then
        STRMAX(1 .. STR'LENGTH) := STR;
        MY_IO.PUT(STRMAX);
      else
        MY_IO.PUT(STR);
      end if;
    end PUTS;
  begin
    PUTS(STATE_LIST'IMAGE(TRANSITION.ORIGINAL_STATE));
    MY_IO.PUT(" -- ");
    PUTS(EVENT_LIST'IMAGE(TRANSITION.EVENT));
    MY_IO.PUT(" -> ");
    PUTS(STATE_LIST'IMAGE(TRANSITION.DESTINATION_STATE));
    MY_IO.NEW_LINE;
  end PUT_TRANSITION;

  procedure DISPLAY_TRANSITION (PREV_STATE : in STATE_LIST;
                                EVENT : in EVENT_LIST;
                                NEW_STATE : in STATE_LIST) is
  begin
    if NEW_STATE /= PREV_STATE then
      PUT_TRANSITION( (PREV_STATE, EVENT, NEW_STATE) );
    end if;
  end DISPLAY_TRANSITION; 
  

  procedure MY_ADD_TRANSITION (TRANSITION : in TRANSITION_REC) is
  begin
    MSM.ADD_TRANSITION(TRANSITION);
    PUT_TRANSITION(TRANSITION);
  end MY_ADD_TRANSITION;

begin

  MY_IO.PUT_LINE("State machine definition:");
  -- Declare state machine
  MY_ADD_TRANSITION ( (UNKNOWN,  DETACH,    DETACHED) );
  MY_ADD_TRANSITION ( (UNKNOWN,  START,     STARTING) );
  MY_ADD_TRANSITION ( (UNKNOWN,  TRUE,      STARTING) );
  MY_ADD_TRANSITION ( (UNKNOWN,  ATTACH,    UNKNOWN) );
  MY_ADD_TRANSITION ( (UNKNOWN,  DEFAULT,   ERROR)    );
  MY_ADD_TRANSITION ( (STARTING, DETACH,    DETACHED) );
  MY_ADD_TRANSITION ( (STARTING, START,     STARTING) );
  MY_ADD_TRANSITION ( (STARTING, FAILURE,   FAILED)   );
  MY_ADD_TRANSITION ( (STARTING, SUCCESS,   OK)       );
  MY_ADD_TRANSITION ( (STARTING, ATTACH,    STARTING) );
  MY_ADD_TRANSITION ( (STARTING, DEFAULT,   ERROR)    );
  MY_ADD_TRANSITION ( (FAILED,   DETACH,    DETACHED) );
  MY_ADD_TRANSITION ( (FAILED,   START,     STARTING) );
  MY_ADD_TRANSITION ( (FAILED,   ATTACH,    FAILED)   );
  MY_ADD_TRANSITION ( (FAILED,   DEFAULT,   ERROR)    );
  MY_ADD_TRANSITION ( (OK,       DETACH,    DETACHED) );
  MY_ADD_TRANSITION ( (OK,       FAILURE,   FAILED)   );
  MY_ADD_TRANSITION ( (OK,       ATTACH,    OK)       );
  MY_ADD_TRANSITION ( (OK,       DEFAULT,   ERROR)    );
  MY_ADD_TRANSITION ( (DETACHED, ATTACH,    UNKNOWN)  );
  MY_ADD_TRANSITION ( (ERROR,    DETACH,    DETACHED) );
-- For true_loop detection
--MY_ADD_TRANSITION ( (OK,       TRUE,      FAILED)   );
--MY_ADD_TRANSITION ( (FAILED,   TRUE,      OK)       );
  MY_IO.PUT_LINE("End of state machine definition.");
  MY_IO.NEW_LINE;
  END_DECLARATION;

  MY_IO.PUT_LINE ("Initial state : " & STATE_LIST'IMAGE(CURRENT_STATE));

  -- Drive
  loop
    CUR_STATE := CURRENT_STATE;

    VALID_EVENT := TRUE;
    case CUR_STATE is
      when FAILED =>
        MY_IO.PUT_LINE (" test program : setting state to unknown");
        SET_STATE (UNKNOWN);
      when others =>
        begin 
          EVENT := GET_EVENT;
        exception
          when others =>
            VALID_EVENT := FALSE;
        end;
        if VALID_EVENT then
          NEW_EVENT (EVENT);
        else
          MY_IO.PUT_LINE (" ???");
        end if;
    end case;
  end loop;

end T_STATE_MACHINE;
