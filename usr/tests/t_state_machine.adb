with My_Io;
with State_Machine;
procedure T_State_Machine is

  type State_List is (Unknown, Starting, Failed, Detached, Ok, Error);

  type Event_List is (True, Default, Start, Failure, Success, Attach, Detach);

  package Msm is new State_Machine (State_List, Event_List);
  use Msm;

  Cur_State : State_List;
  Event : Event_List;
  Valid_Event : Boolean;

  function Get_Event return Event_List is
    Event : Event_List;
    Str : String (1 .. 500);
    Len : Natural;
  begin
    for E in Event_List loop
      My_Io.Put (Event_List'Image(E) & " ");
    end loop;
    My_Io.Put (" ? ");
    My_Io.Get_Line (Str, Len);
    Event := Event_List'Value (Str(1 .. Len));
    return Event;
  end Get_Event;


  procedure Put_Change (Change : in State_Change_Rec) is
    procedure Puts (Str : in String) is
      Strmax : String(1 .. 8) := (others => ' ');
    begin
      if Str'Length <= Strmax'Length then
        Strmax(1 .. Str'Length) := Str;
        My_Io.Put(Strmax);
      else
        My_Io.Put(Str);
      end if;
    end Puts;
  begin
    Puts(State_List'Image(Change.Original_State));
    My_Io.Put(" -- ");
    if Change.On_Event then
      Puts(Event_List'Image(Change.Event));
    else
      Puts("Forced");
    end if;
    My_Io.Put(" -> ");
    Puts(State_List'Image(Change.Destination_State));
    My_Io.New_Line;
  end Put_Change;

  procedure Display_Change (Msg : in String; Change : in State_Change_Rec) is
  begin
    if Change.Destination_State /= Change.Original_State then
      My_Io.Put (Msg & ": ");
      Put_Change (Change);
    end if;
  end Display_Change; 

  procedure Report_Transition (Transition : in Transition_Rec) is
  begin
    Display_Change ("Transition", Transition);
  end Report_Transition; 
  procedure Report_Event (Transition : in Transition_Rec) is
  begin
    Display_Change ("Event", Transition);
  end Report_Event; 
  procedure Report_State (Change : in State_Change_Rec) is
  begin
    Display_Change ("State", Change);
  end Report_State; 

  procedure My_Add_Transition (From_State : in State_List;
                               Event : Event_List;
                               To_State : in State_List) is
    Transition : Transition_Rec;
  begin
    Transition := (True, From_State, To_State, Event);
    Msm.Add_Transition (Transition, Report_Transition'Unrestricted_Access);
    Put_Change (Transition);
  end My_Add_Transition;

begin

  My_Io.Put_Line("State machine definition:");
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
--MY_ADD_TRANSITION (OK,       TRUE,      FAILED)  ;
--MY_ADD_TRANSITION (FAILED,   TRUE,      OK)      ;
  My_Io.Put_Line("End of state machine definition.");
  My_Io.New_Line;
  My_Io.Put_Line("Reports on State=Unknown and Event=Default");
  Msm.Add_State_Report(Unknown, Report_State'Unrestricted_Access);
  Msm.Add_Event_Report(Default, Report_Event'Unrestricted_Access);
  End_Declaration;

  My_Io.Put_Line ("Initial state : " & State_List'Image(Current_State));

  -- Drive
  loop
    Cur_State := Current_State;

    Valid_Event := True;
    case Cur_State is
      when Failed =>
        My_Io.Put_Line (" test program : setting state to unknown");
        Set_State (Unknown);
      when others =>
        begin 
          Event := Get_Event;
        exception
          when others =>
            Valid_Event := False;
        end;
        if Valid_Event then
          New_Event (Event);
        else
          My_Io.Put_Line (" ???");
        end if;
    end case;
  end loop;

end T_State_Machine;
