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

  procedure DISPLAY_TRANSITION (PREV_STATE : in STATE_LIST;
                                EVENT : in EVENT_LIST;
                                NEW_STATE : in STATE_LIST) is
  begin
    MY_IO.PUT_LINE (STATE_LIST'IMAGE(PREV_STATE) & " -- " & EVENT_LIST'IMAGE(EVENT)
                  & " -> " &STATE_LIST'IMAGE(NEW_STATE));
  end DISPLAY_TRANSITION;
begin

  -- Declare state machine
  ADD_TRANSITION ( (UNKNOWN,  DETACH,    DETACHED) );
  ADD_TRANSITION ( (UNKNOWN,  START,     STARTING) );
  ADD_TRANSITION ( (UNKNOWN,  TRUE,      STARTING) );
  ADD_TRANSITION ( (STARTING, DETACH,    DETACHED) );
  ADD_TRANSITION ( (STARTING, START,     STARTING) );
  ADD_TRANSITION ( (STARTING, FAILURE,   FAILED)   );
  ADD_TRANSITION ( (STARTING, SUCCESS,   OK)       );
  ADD_TRANSITION ( (FAILED,   DETACH,    DETACHED) );
  ADD_TRANSITION ( (FAILED,   START,     STARTING) );
  ADD_TRANSITION ( (OK,       DETACH,    DETACHED) );
  ADD_TRANSITION ( (OK,       START,     STARTING) );
  ADD_TRANSITION ( (OK,       FAILURE,   FAILED)   );
  ADD_TRANSITION ( (DETACHED, ATTACH,    UNKNOWN)  );
--ADD_TRANSITION ( (OK,       TRUE,      FAILED)   );
--ADD_TRANSITION ( (FAILED,   TRUE,      OK)       );
  ADD_TRANSITION ( (UNKNOWN,  DEFAULT,   ERROR)    );
  ADD_TRANSITION ( (STARTING, DEFAULT,   ERROR)    );
  ADD_TRANSITION ( (FAILED,   DEFAULT,   ERROR)    );
  ADD_TRANSITION ( (OK,       DEFAULT,   ERROR)    );
  ADD_TRANSITION ( (ERROR,    DETACH,    DETACHED) );
  END_DECLARATION;

  MY_IO.PUT_LINE ("Initial state : " & STATE_LIST'IMAGE(CURRENT_STATE));

  -- Drive
  loop
    CUR_STATE := CURRENT_STATE;

    VALID_EVENT := TRUE;
    case CUR_STATE is
      when FAILED =>
        MY_IO.PUT_LINE (" test program --> setting state to unknown");
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
        end if;
    end case;
  end loop;

end T_STATE_MACHINE;
