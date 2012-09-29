with Basic_Proc, State_Machine;
procedure T_State_Machine is

  type State_List is (Unknown, Starting, Failed, Detached, Ok, Error);

  type Event_List is (True, Default, Start, Failure, Success, Attach, Detach);

  package Msm is new State_Machine (State_List, Event_List, Boolean);
  Mach : Msm.Machine_Type (True);
  Mach1 : Msm.Machine_Type (False);
  Mach2 : Msm.Machine_Type (False);
  pragma Unreferenced (Mach1, Mach2);

  Cur_State : State_List;
  Event : Event_List;
  Valid_Event : Boolean;

  function Get_Event return Event_List is
    Event : Event_List;
  begin
    for E in Event_List loop
      Basic_Proc.Put_Output (Event_List'Image(E) & " ");
    end loop;
    Basic_Proc.Put_Output (" ? ");
    Event := Event_List'Value (Basic_Proc.Get_Line);
    return Event;
  end Get_Event;


  procedure Put_Change (Change : in Msm.State_Change_Rec) is
    procedure Puts (Str : in String) is
      Strmax : String(1 .. 8) := (others => ' ');
    begin
      if Str'Length <= Strmax'Length then
        Strmax(1 .. Str'Length) := Str;
        Basic_Proc.Put_Output (Strmax);
      else
        Basic_Proc.Put_Output (Str);
      end if;
    end Puts;
  begin
    Puts(State_List'Image(Change.Original_State));
    Basic_Proc.Put_Output (" -- ");
    if Change.On_Event then
      Puts (Event_List'Image(Change.Event));
    else
      Puts ("Forced");
    end if;
    Basic_Proc.Put_Output (" -> ");
    Puts (State_List'Image(Change.Destination_State));
    Basic_Proc.New_Line_Output;
  end Put_Change;

  procedure Display_Change (Msg : in String;
                            Change : in Msm.State_Change_Rec) is
  begin
    if Change.Destination_State /= Change.Original_State then
      Basic_Proc.Put_Output (Msg & ": ");
      Put_Change (Change);
    end if;
  end Display_Change;

  procedure Report_Transition (Id : in Boolean;
                               Transition : in Msm.Transition_Rec) is
    pragma Unreferenced (Id);
  begin
    Display_Change ("Transition", Transition);
  end Report_Transition;
  procedure Report_Event (Id : in Boolean;
                          Transition : in Msm.Transition_Rec) is
    pragma Unreferenced (Id);
  begin
    Display_Change ("Event", Transition);
  end Report_Event;
  procedure Report_State (Id : in Boolean;
                          Change : in Msm.State_Change_Rec) is
    pragma Unreferenced (Id);
  begin
    Display_Change ("State", Change);
  end Report_State;

  procedure My_Add_Transition (From_State : in State_List;
                               Event : Event_List;
                               To_State : in State_List) is
    Transition : Msm.Transition_Rec;
  begin
    Transition := (True, From_State, To_State, Event);
    Mach.Add_Transition (Transition, Report_Transition'Unrestricted_Access);
    Put_Change (Transition);
  end My_Add_Transition;

begin

  Basic_Proc.Put_Line_Output ("State machine definition:");
  -- Declare state machine
  My_Add_Transition (Unknown,  Detach,    Detached);
  My_Add_Transition (Unknown,  Start,     Starting);
  My_Add_Transition (Unknown,  True,      Starting);
  My_Add_Transition (Unknown,  Attach,    Unknown) ;
  My_Add_Transition (Unknown,  Default,   Error)   ;
  My_Add_Transition (Starting, Detach,    Detached);
  My_Add_Transition (Starting, Start,     Starting);
  My_Add_Transition (Starting, Failure,   Failed)  ;
  My_Add_Transition (Starting, Success,   Ok)      ;
  My_Add_Transition (Starting, Attach,    Starting);
  My_Add_Transition (Starting, Default,   Error)   ;
  My_Add_Transition (Failed,   Detach,    Detached);
  My_Add_Transition (Failed,   Start,     Starting);
  My_Add_Transition (Failed,   Attach,    Failed)  ;
  My_Add_Transition (Failed,   Default,   Error)   ;
  My_Add_Transition (Ok,       Detach,    Detached);
  My_Add_Transition (Ok,       Failure,   Failed)  ;
  My_Add_Transition (Ok,       Attach,    Ok)      ;
  My_Add_Transition (Ok,       Default,   Error)   ;
  My_Add_Transition (Detached, Attach,    Unknown) ;
  My_Add_Transition (Error,    Detach,    Detached);
-- For true_loop detection
--My_Add_Transition (Ok,       True,      Failed)  ;
--My_Add_Transition (Failed,   True,      Ok)      ;
  Basic_Proc.Put_Line_Output ("Reports set on State=Unknown and Event=Default");
  Mach.Add_State_Report(Unknown, Report_State'Unrestricted_Access);
  Mach.Add_Event_Report(Default, Report_Event'Unrestricted_Access);
  Basic_Proc.Put_Line_Output ("End of state machine definition.");
  Basic_Proc.New_Line_Output;
  Mach.End_Declaration;

  Basic_Proc.Put_Line_Output  ("Initial state : "
                             & State_List'Image(Mach.Current_State));

  -- Drive
  loop
    Cur_State := Mach.Current_State;

    Valid_Event := True;
    case Cur_State is
      when Failed =>
        Basic_Proc.Put_Line_Output (" test program : setting state to unknown");
        Mach.Set_State (Unknown);
      when others =>
        begin
          Event := Get_Event;
        exception
          when Basic_Proc.End_Error =>
            raise;
          when others =>
            Valid_Event := False;
        end;
        if Valid_Event then
          Mach.New_Event (Event);
        else
          Basic_Proc.Put_Line_Output (" ???");
        end if;
    end case;
  end loop;

exception
  when Basic_Proc.End_Error =>
    null;
end T_State_Machine;
