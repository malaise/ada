generic

  -- List of states
  -- The initial state of the machine will be First of the list
  type State_List is (<>);

  -- List of events
  -- The TRUE event can be declared in this list to generate
  --  automatic transitions
  -- The DEFAULT event can be declared in this list to generate
  --  a transition on any unspecified event
  type Event_List is (<>);

  -- Procedure to report a transition
  with procedure Report_Transition (Prev_State : in State_List;
                                    Event : in Event_List;
                                    New_State : in State_List);
package State_Machine is

  -- A transition
  type Transition_Rec is record
    Original_State : State_List;
    Event : Event_List;
    Destination_State : State_List;
  end record;

  -- To add a transition in the state machine
  -- May raise EVENT_ALREADY if this event is already defined
  --  from the original state
  -- May raise DECLARATION_ENDED if called after END_DECLARATION;
  procedure Add_Transition (Transition : in Transition_Rec);

  -- To end declarations
  -- May raise DECLARATION_ENDED if re-called after END_DECLARATION;
  -- May raise TRUE_LOOP if TRUE transitions from any state loop
  procedure End_Declaration;


  -- All following calls may raise DECLARATION_NOT_ENDED if
  --  called before END_DECLARATION

  -- An event: do a transition. If the event is not defined for
  --  current state, the DEFAULT (if any) transition is performed
  --  otherwise (no DEFAULT) the state remains unchanged
  -- Any TRUE event has no effect (any potential TRUE transition
  --  would already have been done)
  function New_Event (Event : Event_List) return State_List;

  procedure New_Event (Event : in Event_List);

  -- Get current state
  function Current_State return State_List;

  -- To force a new state without event
  procedure Set_State (State : in State_List);


  Declaration_Ended, Declaration_Not_Ended : exception;
  True_Loop : exception;
  Event_Already : exception;

end State_Machine;

