-- Test State_Machine. Arg1 is the file name defining the machines and Arg2 is
-- the machine name. Events are then read from stdin.
with Argument, Basic_Proc, As.U, Mixed_Str, State_Machine, Xml_Parser;
procedure T_State_Machine is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
      & " <Xml_File> <Machine_Name>");
  end Usage;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Basic_Proc.Set_Error_Exit_Code;
   end Error;

  -- Xml context
  Ctx : Xml_Parser.Ctx_Type;
  Ok : Boolean;
  Machines, Machine, States, Events, Transitions, Elt : Xml_Parser.Element_Type;

  -- Machine name and machine list
  Machine_Name : As.U.Asu_Us;
  type Machine_List is (The_One);

begin

  -- Check arguments
  if Argument.Get_Nbre_Arg /= 2 then
    Error ("Invalid argument");
    Usage;
    return;
  end if;

  -- Parse Xml file
  Ctx.Parse (Argument.Get_Parameter (Occurence => 1), Ok);
  if not Ok then
    Error (Ctx.Get_Parse_Error_Message);
    return;
  end if;

  -- Locate machine
  Machine_Name := As.U.Tus (Argument.Get_Parameter (Occurence => 2));
  Machines := Ctx.Get_Root_Element;
  Ok := False;
  for I in 1 .. Ctx.Get_Nb_Children (Machines) loop
    Machine := Ctx.Get_Child (Machines, I);
    if Ctx.Get_Attribute (Machine, "Name") = Machine_Name.Image then
      Ok := True;
      exit;
    end if;
  end loop;
  if not Ok then
    Error ("Machine name " & Machine_Name.Image & " not found");
    return;
  end if;

  -- Get Nb of states and events
  States := Ctx.Get_Child (Machine, 1);
  Events := Ctx.Get_Child (Machine, 2);

  -- Instanciate the manager
  declare
    subtype State_List is Positive range 1 .. Ctx.Get_Nb_Children (States);
    subtype Event_List is Positive range 1 .. Ctx.Get_Nb_Children (Events);
    package Msm is new State_Machine
        (State_List, Event_List, Machine_List); --## Rule line off Generic_Aliasing
    Mach, Unused : Msm.Machine_Type (The_One);

    State_Names : As.U.Asu_Array (State_List);
    Event_Names : As.U.Asu_Array (Event_List);
    Curr_State, Orig_State, Dest_State : State_List;
    Event : Event_List;
    Valid_Event : Boolean;

    -- Get index of state or event from its name
    function State_Id (State : String) return State_List is
    begin
      for I in State_List loop
        if State_Names(I).Image = State then
          return I;
        end if;
      end loop;
      raise Constraint_Error;
    end State_Id;
    function Event_Id (Event : String) return Event_List is
    begin
      for I in Event_List loop
        if Event_Names(I).Image = Event then
          return I;
        end if;
      end loop;
      raise Constraint_Error;
    end Event_Id;

    -- Get event from stdin
    function Get_Event return Event_List is
      Event : Event_List;
    begin
      for E in Event_List loop
        Basic_Proc.Put_Output (Event_Names(E).Image & " ");
      end loop;
      Basic_Proc.Put_Output (" ? ");
      Event := Event_Id (Mixed_Str (Basic_Proc.Get_Line));
     return Event;
    end Get_Event;

    -- Put transition: State -- Event -> State
    procedure Put_Change (Change : in Msm.State_Change_Rec) is
      -- Fixed indent if possible
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
      Puts(State_Names(Change.Original_State).Image);
      Basic_Proc.Put_Output (" -- ");
      if Change.On_Event then
        Puts (Event_Names(Change.Event).Image);
      else
        Puts ("Forced");
      end if;
      Basic_Proc.Put_Output (" -> ");
      Puts (State_Names(Change.Destination_State).Image);
      Basic_Proc.New_Line_Output;
    end Put_Change;

    -- Put transition is state changes
    procedure Display_Change (Msg : in String;
                              Change : in Msm.State_Change_Rec) is
    begin
      if Change.Destination_State /= Change.Original_State then
        Basic_Proc.Put_Output (Msg & ": ");
        Put_Change (Change);
      end if;
    end Display_Change;

    -- Callbacks of transition, event, state change
    procedure Report_Transition (Unused_Id : in Machine_List;
                                 Transition : in Msm.Transition_Rec) is
    begin
      Display_Change ("Transition", Transition);
    end Report_Transition;
    procedure Report_Event (Unused_Id : in Machine_List;
                            Transition : in Msm.Transition_Rec) is
    begin
      Display_Change ("Event", Transition);
    end Report_Event;
    procedure Report_State (Unused_Id : in Machine_List;
                            Change : in Msm.State_Change_Rec) is
    begin
      Display_Change ("State", Change);
    end Report_State;

    -- Define a transition
    procedure My_Add_Transition (From_State : in State_List;
                                 Event : Event_List;
                                 To_State : in State_List) is
      Transition : Msm.Transition_Rec;
    begin
      Transition := (True, From_State, To_State, Event);
      Mach.Add_Transition (Transition, Report_Transition'Unrestricted_Access);
      Put_Change (Transition);
    end My_Add_Transition;

  -- Start of main bloc
  begin

    Basic_Proc.Put_Line_Output ("State machine definition:");
    -- Store state names and event names, detect duplicates
    for I in 1 .. Ctx.Get_Nb_Children (States) loop
      Elt := Ctx.Get_Child (States, I);
      State_Names(I) := Ctx.Get_Attribute (Elt, "Name");
      if State_Id (State_Names(I).Image) /= I then
        Error ("State " & State_Names(I).Image & " already defined");
        return;
      end if;
    end loop;
    for I in 1 .. Ctx.Get_Nb_Children (Events) loop
      Elt := Ctx.Get_Child (Events, I);
      Event_Names(I) := Ctx.Get_Attribute (Elt, "Name");
      if Event_Id (Event_Names(I).Image) /= I then
        Error ("Event " & Event_Names(I).Image & " already defined");
        return;
      end if;
    end loop;

    -- Check states "Unknown" and "Failed"
    declare
      Dummy : State_List;
    begin
      Dummy := State_Id ("Unknown");
    exception
      when Constraint_Error =>
        Error ("Missing definition of state ""Unknown""");
        return;
    end;
    declare
      Dummy : State_List;
    begin
      Dummy := State_Id ("Failed");
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output ("Warning: No state ""Failed""");
    end;

    -- Set True event
    declare
      Dummy : Event_List;
    begin
      Dummy := Event_Id ("True");
      Mach.Set_True (Dummy);
      Basic_Proc.Put_Line_Output ("Event ""True"" defined");
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output ("Warning: No ""True"" event defined");
    end;

    -- Define transitions
    Transitions := Ctx.Get_Child (Machine, 3);
    for I in 1 .. Ctx.Get_Nb_Children (Transitions) loop
      Elt := Ctx.Get_Child (Transitions, I);
      begin
        Orig_State  := State_Id (Ctx.Get_Attribute (Elt, "OrigState"));
      exception
        when Constraint_Error =>
          Error ("Unknown state " & Ctx.Get_Attribute (Elt, "OrigState"));
          return;
      end;
      begin
        Event  := Event_Id (Ctx.Get_Attribute (Elt, "Event"));
      exception
        when Constraint_Error =>
          Error ("Unknown event " & Ctx.Get_Attribute (Elt, "Event"));
          return;
      end;
      begin
        Dest_State  := State_Id (Ctx.Get_Attribute (Elt, "DestState"));
      exception
        when Constraint_Error =>
          Error ("Unknown state " & Ctx.Get_Attribute (Elt, "DestState"));
          return;
      end;
      My_Add_Transition (Orig_State, Event, Dest_State);
    end loop;

    -- Set observers, check mandatory
    Mach.Add_State_Report(State_Id ("Unknown"),
                          Report_State'Unrestricted_Access);
    Basic_Proc.Put_Line_Output ("Set observer on State=Unknown");
    begin
      Mach.Add_Event_Report(Event_Id("Default"),
                            Report_Event'Unrestricted_Access);
      Basic_Proc.Put_Line_Output ("Set observer on Event=Default");
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output (
            "Warning: skipping observer on Event=Default");
    end;

    Basic_Proc.Put_Line_Output ("End of state machine definition.");
    Basic_Proc.New_Line_Output;
    Mach.End_Declaration;

    Basic_Proc.Put_Line_Output  ("Initial state : "
                               & State_Names(Mach.Current_State).Image);

    -- Drive
    loop
      Curr_State := Mach.Current_State;

      Valid_Event := True;
      if State_Names(Curr_State).Image = "Failed" then
          Basic_Proc.Put_Line_Output
              (" test program : setting state to unknown");
          Mach.Set_State (State_Id ("Unknown"));
      else
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
      end if;
    end loop;

  end;

exception
  when Basic_Proc.End_Error =>
    null;
end T_State_Machine;

