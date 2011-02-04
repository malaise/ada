-- State machine management
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

  -- Identifies (preferably uniquely) a machine so that the user callbacks
  --  can know which machine is calling
  type Machine_Id is (<>);

package State_Machine is

  -- A machine
  type Machine_Type (Id : Machine_Id) is tagged limited private;

  -- General transition / event / state report
  type State_Change_Rec (On_Event : Boolean := True) is record
    Original_State : State_List;
    Destination_State : State_List;
    case On_Event is
      when True =>
        Event : Event_List;
      when False =>
         null;
    end case;
  end record;

  -- Procedure to report a new state
  --  (Callback on state)
  type State_Report_Access is
       access procedure (Id : in Machine_Id;
                        State_Change : in State_Change_Rec);

  -- To add a report callback on new state
  -- May raise Event_Already if this state already has a report callback
  -- May raise Declaration_Ended if called after End_Declaration;
  procedure Add_State_Report (Machine : in out Machine_Type;
                              To_State : in State_List;
                              Report : in State_Report_Access);


  -- A transition or event/transition report
  subtype Transition_Rec is State_Change_Rec (On_Event => True);

  -- Procedure to report a transition
  --  (Callback on event or transition)
  type Transition_Report_Access is
       access procedure (Id : in Machine_Id;
                         Transition : in Transition_Rec);

  -- To add a report callback on event occurence
  -- May raise Event_Already if this event already has a report callback
  procedure Add_Event_Report (Machine : in out Machine_Type;
                              To_Event : in Event_List;
                              Report : in Transition_Report_Access);

  -- To add a transition in the state machine
  -- May raise Event_Already if this event is already defined
  --  from the original state
  -- May raise Declaration_Ended if called after End_Declaration;
  procedure Add_Transition (Machine : in out Machine_Type;
                            Transition : in Transition_Rec;
                            Report : in Transition_Report_Access := null);


  -- To end declarations
  -- May raise Declaration_Ended if re-called after End_Declaration
  -- May raise True_Loop if True transitions from any state loop
  procedure End_Declaration (Machine : in out Machine_Type);


  -- All following calls may raise Declaration_Not_Ended if
  --  called before End_Declaration

  -- An event: do a transition. If the event is not defined for
  --  current state, the Default (if any) transition is performed
  --  otherwise (no Default) the state remains unchanged
  -- A True event has no effect if no Default is defined
  --  (any potential True transition would already have been done)
  --  It generates the Default transition if it is defined
  procedure New_Event (Machine : in out Machine_Type;
                       Event : in Event_List);
  procedure New_Event (Machine : in out Machine_Type;
                       Event : in Event_List;
                       New_State : out State_List);

  -- Get current state
  function Current_State (Machine : Machine_Type) return State_List;

  -- To force a new state without event
  procedure Set_State (Machine : in out Machine_Type; State : in State_List);


  Declaration_Ended, Declaration_Not_Ended : exception;
  True_Loop : exception;
  Event_Already : exception;
private

  -- A transition
  type Transition_Cell;
  type Transition_Access is access Transition_Cell;
  type Transition_Cell is record
    Event : Event_List;
    New_State : State_List;
    Report : Transition_Report_Access;
    -- Other transitions from this state
    Next_Transition : Transition_Access;
  end record;

  -- Arrays of states and Cbs
  type State_Array is array (State_List) of Transition_Access;
  type State_Cb_Array is array (State_List) of State_Report_Access;
  type Event_Cb_Array is array (Event_List) of Transition_Report_Access;

  -- A machine
  type Machine_Type (Id : Machine_Id) is tagged limited record
    -- Still declaring?
    In_Declaration : Boolean := True;
    -- The current state
    Curr_State : State_List := State_List'First;
    -- The states, each one is an access to first transition
    States : State_Array := (others => null);

    -- The callabcks on events and states
    State_Cbs : State_Cb_Array := (others => null);
    Event_Cbs : Event_Cb_Array := (others => null);
  end record;

end State_Machine;

