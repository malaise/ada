package body State_Machine is

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

  -- The states, each one is an access to first transition
  State_Array : array (State_List) of Transition_Access := (others => null);
  State_Nb : Integer := State_Array'Length;

  -- The current state
  The_Current_State : State_List;

  -- Still declaring?
  In_Declaration : Boolean := True;

  procedure Do_Transition (From_State : in State_List;
                           Transition : in Transition_Access;
                           Report     : in Boolean) is
  begin
    -- Call user procedure
    if Report and then Transition.Report /= null then
       Transition.Report ( (From_State,
                            Transition.Event,
                            Transition.New_State) );
    end if;
    The_Current_State := Transition.New_State;
  end Do_Transition;


  -- Do all True transitions from the current state
  -- detects loops
  procedure Do_Trues (Report : in Boolean);

  -- To add a transition in the state machine
  -- May raise Declaration_Ended if called after End_Declaration;
  procedure Add_Transition (Transition : in Transition_Rec;
                            Report : in Transition_Report_Access := null) is
    Ta, Tap : Transition_Access;
  begin
    if not In_Declaration then
      raise Declaration_Ended;
    end if;

    Ta := State_Array(Transition.Original_State);

    if Ta = null then
      -- First transition from this state
      State_Array (Transition.Original_State) := new Transition_Cell'(
                 Event           => Transition.Event,
                 New_State       => Transition.Destination_State,
                 Report          => Report,
                 Next_Transition => null );
    else
      -- Check not already defined
      loop
        if Ta.Event = Transition.Event then
          -- This event already defined from this state
          raise Event_Already;
        end if;
        Tap := Ta;
        Ta := Tap.Next_Transition;
        exit when Ta = null;
      end loop;

      Tap.Next_Transition := new Transition_Cell'(
                 Event           => Transition.Event,
                 New_State       => Transition.Destination_State,
                 Report          => Report,
                 Next_Transition => null );
    end if;
  end Add_Transition;

  -- To end declarations
  -- May raise Declaration_Ended if re-called after End_Declaration;
  procedure End_Declaration is
  begin
    if not In_Declaration then
      raise Declaration_Ended;
    end if;
    In_Declaration := False;
    -- Check true loops
    -- Do all true transitions from any state,
    -- Last one (first state) cannot have true loop
    --  (it would have been detected by a check on the
    --   destination state of this true event)
    --   except if it has true to itself, so check it as well.
    for Start_State in reverse State_List loop
      The_Current_State := Start_State;
      Do_Trues (False);
    end loop;
    -- Do and report true transitions from initial state.
    -- The_Current_State is first of State_List now.
    Do_Trues (True);
  end End_Declaration;

  procedure Do_Trues (Report : in Boolean) is
    Ta : Transition_Access;
    Nb_True : Natural := 0;
  begin
    if In_Declaration then
      raise Declaration_Not_Ended;
    end if;
    Ta := State_Array(The_Current_State);
    -- Look for a True transition
    loop
      if Ta = null then
        -- No other transition (which means no True transition) from this state
        return;
      end if;
      if Event_List'Image(Ta.Event) = "TRUE" then
        -- Transition is TRUE, follow it
        Do_Transition (The_Current_State, Ta, Report);
        Ta := State_Array(The_Current_State);
        -- Count true transitions to detect loops
        Nb_True := Nb_True + 1;
        if Nb_True = State_Nb then
          raise True_Loop;
        end if;
      else
        Ta := Ta.Next_Transition;
      end if;
    end loop;
  end Do_Trues;

  -- All following calls may raise Declaration_Not_Ended if
  --  called before End_Declaration

  -- An event: do a transition.
  function New_Event (Event : Event_List) return State_List is
  begin
    New_Event (Event);
    return The_Current_State;
  end New_Event;

  procedure New_Event (Event : in Event_List) is
    Ta : Transition_Access;
    Defaulta : Transition_Access := null;
  begin
    if In_Declaration then
      raise Declaration_Not_Ended;
    end if;
    Ta := State_Array(The_Current_State);
    loop
      if Ta = null then
        -- No other transition for this state (not found)
        -- Do default if defined (found)
        if Defaulta /= null then
          Do_Transition (The_Current_State, Defaulta, True);
          Do_Trues (True);
        end if;
        return;
      end if;
      if Ta.Event = Event then
        -- Transition event matches
        Do_Transition (The_Current_State, Ta, True);
        Do_Trues (True);
        return;
      else
        if Event_List'Image(Ta.Event) = "DEFAULT" then
          -- Default transition found. Store it.
          Defaulta := Ta;
        end if;
        Ta := Ta.Next_Transition;
      end if;
    end loop;
  end New_Event;

  -- Get current state
  function Current_State return State_List is
  begin
    if In_Declaration then
      raise Declaration_Not_Ended;
    end if;
    return The_Current_State;
  end Current_State;

  -- To force a new state without event
  procedure Set_State (State : in State_List) is
  begin
    if In_Declaration then
      raise Declaration_Not_Ended;
    end if;
    The_Current_State := State;
    Do_Trues (True);
  end Set_State;

end State_Machine;

