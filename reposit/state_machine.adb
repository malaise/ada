package body STATE_MACHINE is

  -- A transition
  type TRANSITION_CELL;
  type TRANSITION_ACCESS is access TRANSITION_CELL;
  type TRANSITION_CELL is record
    EVENT : EVENT_LIST;
    NEW_STATE : STATE_LIST;
    -- Other transition from this state
    NEXT_TRANSITION : TRANSITION_ACCESS;
  end record;

  -- The states, each one is an access to first transition
  STATE_ARRAY : array (STATE_LIST) of TRANSITION_ACCESS := (others => null);
  STATE_NB : INTEGER := STATE_ARRAY'LENGTH;

  -- The current state
  THE_CURRENT_STATE : STATE_LIST;

  -- Still declaring?
  IN_DECLARATION : BOOLEAN := TRUE;

  procedure DO_TRANSITION (FROM_STATE : in STATE_LIST; TRANSITION : TRANSITION_ACCESS;
                           REPORT : in BOOLEAN) is
  begin
    -- Call user procedure
    if REPORT then
       REPORT_TRANSITION (FROM_STATE, TRANSITION.EVENT, TRANSITION.NEW_STATE);
    end if;
    THE_CURRENT_STATE := TRANSITION.NEW_STATE;
  end DO_TRANSITION;


  -- Do all True transitions from the current state
  -- detects loops
  procedure DO_TRUES (REPORT : in BOOLEAN);

  -- To add a transition in the state machine
  -- May raise DECLARATION_ENDED if called after END_DECLARATION;
  procedure ADD_TRANSITION (TRANSITION : in TRANSITION_REC) is
    TA, TAP : TRANSITION_ACCESS;
  begin
    if not IN_DECLARATION then
      raise DECLARATION_ENDED;
    end if;

    TA := STATE_ARRAY(TRANSITION.ORIGINAL_STATE);

    if TA = null then
      -- First transition from this state
      STATE_ARRAY (TRANSITION.ORIGINAL_STATE) := new TRANSITION_CELL'(
                 EVENT           => TRANSITION.EVENT,
                 NEW_STATE       => TRANSITION.DESTINATION_STATE,
                 NEXT_TRANSITION => null );
    else
      -- Check not already defined
      loop
        if TA.EVENT = TRANSITION.EVENT then
          -- This event already defined from this state
          raise EVENT_ALREADY;
        end if;
        if TA.NEW_STATE = TRANSITION.DESTINATION_STATE then
          -- This new_state already reached by another transition from this state
          raise DESTINATION_ALREADY;
        end if;
        TAP := TA;
        TA := TAP.NEXT_TRANSITION;
        exit when TA = null;
      end loop;

      TAP.NEXT_TRANSITION := new TRANSITION_CELL'(
                 EVENT           => TRANSITION.EVENT,
                 NEW_STATE       => TRANSITION.DESTINATION_STATE,
                 NEXT_TRANSITION => null );
    end if;
  end ADD_TRANSITION;

  -- To end declarations
  -- May raise DECLARATION_ENDED if re-called after END_DECLARATION;
  procedure END_DECLARATION is
  begin
    if not IN_DECLARATION then
      raise DECLARATION_ENDED;
    end if;
    IN_DECLARATION := FALSE;
    -- Do all true transitions from any state,
    --  do first state last and report it only
    for START_STATE in reverse STATE_LIST loop
      THE_CURRENT_STATE := START_STATE;
      DO_TRUES (START_STATE = STATE_LIST'FIRST);
    end loop;
  end END_DECLARATION;

  procedure DO_TRUES (REPORT : in BOOLEAN) is
    TA : TRANSITION_ACCESS;
    NB_TRUE : NATURAL := 0;
  begin
    if IN_DECLARATION then
      raise DECLARATION_NOT_ENDED;
    end if;
    TA := STATE_ARRAY(THE_CURRENT_STATE);
    -- Look for a TRUE transition
    loop
      if TA = null then
        -- No other transition (which means no TRUE transition) from this state
        return;
      end if;
      if EVENT_LIST'IMAGE(TA.EVENT) = "TRUE" then
        -- Transition is TRUE, follow it
        DO_TRANSITION (THE_CURRENT_STATE, TA, REPORT);
        TA := STATE_ARRAY(THE_CURRENT_STATE);
        -- Count true transitions to detect loops
        NB_TRUE := NB_TRUE + 1;
        if NB_TRUE = STATE_NB then
          raise TRUE_LOOP;
        end if;
      else
        TA := TA.NEXT_TRANSITION;
      end if;
    end loop;
  end DO_TRUES;

  -- All following calls may raise DECLARATION_NOT_ENDED if
  --  called before END_DECLARATION

  -- An event: do a transition. If the event is not defined for
  --  current state, the state reamains unchanged.
  function NEW_EVENT (EVENT : EVENT_LIST) return STATE_LIST is
  begin
    NEW_EVENT (EVENT);
    return THE_CURRENT_STATE;
  end NEW_EVENT;

  procedure NEW_EVENT (EVENT : in EVENT_LIST) is
    TA : TRANSITION_ACCESS;
  begin
    if IN_DECLARATION then
      raise DECLARATION_NOT_ENDED;
    end if;
    TA := STATE_ARRAY(THE_CURRENT_STATE);
    loop
      if TA = null then
        -- No other transition for this state (not found)
        return;
      end if;
      if TA.EVENT = EVENT then
        -- Transition event matches
        DO_TRANSITION (THE_CURRENT_STATE, TA, TRUE);
        DO_TRUES (TRUE);
        return;
      else
        TA := TA.NEXT_TRANSITION;
      end if;
    end loop;
  end NEW_EVENT;

  -- Get current state
  function CURRENT_STATE return STATE_LIST is
  begin
    if IN_DECLARATION then
      raise DECLARATION_NOT_ENDED;
    end if;
    return THE_CURRENT_STATE;
  end CURRENT_STATE;

  -- To force a new state without event
  procedure SET_STATE (STATE : in STATE_LIST) is
  begin
    if IN_DECLARATION then
      raise DECLARATION_NOT_ENDED;
    end if;
    THE_CURRENT_STATE := STATE;
    DO_TRUES (TRUE);
  end SET_STATE;

end STATE_MACHINE;

