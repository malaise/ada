generic

  -- List of states
  -- The initial state of the machine will be First of the list
  type State_List is (<>);

  -- List of events
  -- The True event can be declared in this list to generate
  --  automatic transitions
  -- The Default event can be declared in this list to generate
  --  a transition on any unspecified event
  type Event_List is (<>);

package State_Machine is

  -- A transition
  type Transition_Rec is record
    Original_State : State_List;
    Event : Event_List;
    Destination_State : State_List;
  end record;

  -- Procedure to report a transition
  type Transition_Report_Access is
       access procedure (Transition : in Transition_Rec);

  -- To add a transition in the state machine
  -- May raise Event_Already if this event is already defined
  --  from the original state
  -- May raise Declaration_Ended if called after End_Declaration;
  procedure Add_Transition (Transition : in Transition_Rec;
                            Report : in Transition_Report_Access := null);

  -- To end declarations
  -- May raise Declaration_Ended if re-called after END_DECLARATION;
  -- May raise True_Loop if True transitions from any state loop
  procedure End_Declaration;


  -- All following calls may raise Declaration_Not_Ended if
  --  called before End_Declaration

  -- An event: do a transition. If the event is not defined for
  --  current state, the Default (if any) transition is performed
  --  otherwise (no Default) the state remains unchanged
  -- A True event has no effect if no Default is defined
  --  (any potential True transition would already have been done)
  --  It generates the Default transition if it is defined
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

