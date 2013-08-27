package body State_Machine is

  -- To add a report callback on new state
  procedure Add_State_Report (Machine : in out Machine_Type;
                              To_State : in State_List;
                              Report : in State_Report_Access) is
  begin
    if Machine.State_Cbs(To_State) /= null then
      raise Report_Already;
    end if;
    Machine.State_Cbs(To_State) := Report;
  end Add_State_Report;

  -- To add a report callback on event occurence
  procedure Add_Event_Report (Machine : in out Machine_Type;
                              To_Event : in Event_List;
                              Report : in Transition_Report_Access) is
  begin
    if Machine.Event_Cbs(To_Event) /= null then
      raise Report_Already;
    end if;
    Machine.Event_Cbs(To_Event) := Report;
  end Add_Event_Report;

  -- INTERNAL
  procedure Do_Transition (Machine : in out Machine_Type;
                           From_State : in State_List;
                           Transition : in Transition_Access;
                           Report     : in Boolean) is
  begin
    -- Call user report callbacks
    if Report then
      if Machine.Event_Cbs(Transition.Event) /= null then
        Machine.Event_Cbs (Transition.Event) (
            Machine.Id,
            (True,
             From_State,
             Transition.New_State,
             Transition.Event) );
      end if;
      if Transition.Report /= null then
        Transition.Report (
            Machine.Id,
            (True,
             From_State,
             Transition.New_State,
             Transition.Event) );
      end if;
      if Machine.State_Cbs(Transition.New_State) /= null then
        Machine.State_Cbs(Transition.New_State) (
            Machine.Id,
            (True,
             From_State,
             Transition.New_State,
             Transition.Event) );
      end if;
    end if;
    Machine.Curr_State := Transition.New_State;
  end Do_Transition;


  -- Do all True transitions from the current state
  -- detects loops
  procedure Do_Trues (Machine : in out Machine_Type;
                      Report : in Boolean);

  -- To add a transition in the state machine
  -- May raise Declaration_Ended if called after End_Declaration;
  procedure Add_Transition (Machine : in out Machine_Type;
                            Transition : in Transition_Rec;
                            Report : in Transition_Report_Access := null) is
    Ta, Tap : Transition_Access;
  begin
    if not Machine.In_Declaration then
      raise Declaration_Ended;
    end if;

    Ta := Machine.States(Transition.Original_State);

    if Ta = null then
      -- First transition from this state
      Machine.States(Transition.Original_State) := new Transition_Cell'(
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
  procedure End_Declaration (Machine : in out Machine_Type) is
  begin
    if not Machine.In_Declaration then
      raise Declaration_Ended;
    end if;
    Machine.In_Declaration := False;
    -- Check true loops
    -- Do all true transitions from any state,
    -- Last one (first state) cannot have true loop
    --  (it would have been detected by a check on the
    --   destination state of this true event)
    --   except if it has true to itself, so check it as well.
    for Start_State in reverse State_List loop
      Machine.Curr_State := Start_State;
      Do_Trues (Machine, False);
    end loop;
    -- Reset state to first of State_List (Do_Trues may have changed it).
    -- Do and report true transitions from initial state.
    Machine.Curr_State := State_List'First;
    Do_Trues (Machine, True);
  end End_Declaration;

  procedure Do_Trues (Machine : in out Machine_Type; Report : in Boolean) is
    Ta : Transition_Access;
    Nb_True : Natural := 0;
  begin
    if Machine.In_Declaration then
      raise Declaration_Not_Ended;
    end if;
    Ta := Machine.States(Machine.Curr_State);
    -- Look for a True transition
    loop
      if Ta = null then
        -- No other transition (which means no True transition) from this state
        return;
      end if;
      if Event_List'Image(Ta.Event) = "TRUE" then
        -- Transition is True, follow it
        Do_Transition (Machine, Machine.Curr_State, Ta, Report);
        Ta := Machine.States(Machine.Curr_State);
        -- Count true transitions to detect loops
        Nb_True := Nb_True + 1;
        if Nb_True = Machine.States'Length then
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
  procedure New_Event (Machine : in out Machine_Type;
                       Event : in Event_List) is
    Ta : Transition_Access;
    Defaulta : Transition_Access := null;
  begin
    if Machine.In_Declaration then
      raise Declaration_Not_Ended;
    end if;
    Ta := Machine.States(Machine.Curr_State);
    loop
      if Ta = null then
        -- No other transition for this state (not found)
        -- Do default if defined (found)
        if Defaulta /= null then
          Do_Transition (Machine, Machine.Curr_State, Defaulta, True);
          Do_Trues (Machine, True);
        end if;
        return;
      end if;
      if Ta.Event = Event then
        -- Transition event matches
        Do_Transition (Machine, Machine.Curr_State, Ta, True);
        Do_Trues (Machine, True);
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

  procedure New_Event (Machine : in out Machine_Type;
                       Event : in Event_List;
                       New_State : out State_List) is
  begin
    New_Event (Machine, Event);
    New_State := Machine.Curr_State;
  end New_Event;

  -- Get current state
  function Current_State (Machine : Machine_Type) return State_List is
  begin
    if Machine.In_Declaration then
      raise Declaration_Not_Ended;
    end if;
    return Machine.Curr_State;
  end Current_State;

  -- To force a new state without event
  procedure Set_State (Machine : in out Machine_Type; State : in State_List) is
  begin
    if Machine.In_Declaration then
      raise Declaration_Not_Ended;
    end if;
    if Machine.State_Cbs(State) /= null then
      Machine.State_Cbs(State) (
          Machine.Id,
          (False,
           Machine.Curr_State,
           State) );
    end if;
    Machine.Curr_State := State;
    Do_Trues (Machine, True);
  end Set_State;

end State_Machine;

