generic

  -- List of states
  -- The initial state of the machine will be First of the list
  type STATE_LIST is (<>);

  -- List of events
  -- The TRUE event can be declared in this list to generate
  --  automatic transitions
  -- The DEFAULT event can be declared in this list to generate
  --  a transition on any unspecified event
  type EVENT_LIST is (<>);

  -- Procedure to report a transition
  with procedure REPORT_TRANSITION (PREV_STATE : in STATE_LIST;
                                    EVENT : in EVENT_LIST;
                                    NEW_STATE : in STATE_LIST);
package STATE_MACHINE is

  -- A transition
  type TRANSITION_REC is record
    ORIGINAL_STATE : STATE_LIST;
    EVENT : EVENT_LIST;
    DESTINATION_STATE : STATE_LIST;
  end record;

  -- To add a transition in the state machine
  -- May raise EVENT_ALREADY if this event is already defined
  --  from the original state
  -- May raise DECLARATION_ENDED if called after END_DECLARATION;
  procedure ADD_TRANSITION (TRANSITION : in TRANSITION_REC);

  -- To end declarations
  -- May raise DECLARATION_ENDED if re-called after END_DECLARATION;
  -- May raise TRUE_LOOP if TRUE transitions from any state loop
  procedure END_DECLARATION;


  -- All following calls may raise DECLARATION_NOT_ENDED if
  --  called before END_DECLARATION

  -- An event: do a transition. If the event is not defined for
  --  current state, the state reamains unchanged
  -- Any TRUE event has no effect (any potential TRUE transistion
  --  would already have been done)
  function NEW_EVENT (EVENT : EVENT_LIST) return STATE_LIST;

  procedure NEW_EVENT (EVENT : in EVENT_LIST);

  -- Get current state
  function CURRENT_STATE return STATE_LIST;

  -- To force a new state without event
  procedure SET_STATE (STATE : in STATE_LIST);


  DECLARATION_ENDED, DECLARATION_NOT_ENDED : exception;
  TRUE_LOOP : exception;
  EVENT_ALREADY : exception;

end STATE_MACHINE;

