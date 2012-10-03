with Images, Timeval;

separate (X_Mng)
package body Dispatch is

  procedure Log (Operation : in String;
                 Client : in Line_Range;
                 Message : in String) is
  begin
    if Debug then
      Basic_Proc.Put_Line_Output ("X_Mng.Dispatcher." & Operation & ":"
                    & Client'Img & " " & Message);
    end if;
  end Log;

  ------------------------------------------------------------------
  -- Wake-up the task that is in select
  -- void evt_wake_up (void)
  ------------------------------------------------------------------
  procedure C_Wake_Up;
  pragma Import(C, C_Wake_Up, "evt_wake_up");
  procedure Wake_Up is
  begin
    C_Wake_Up;
  end Wake_Up;

  ------------------------------------------------------------------
  -- Wait for some events
  -- int x_select (int *p_fd, int *p_read, timeout_t *timeout_ms);
  ------------------------------------------------------------------
  function X_Select (P_Fd : System.Address;
                     P_Read : System.Address;
                     P_Timeout : System.Address) return Result;
  pragma Import(C, X_Select, "x_select");

  ------------------------------------------------------------------
  -- Process a X event (Tid or Keyboard or other)
  -- int x_process_event (void **p_line_id, int *p_kind, boolean *p_next);
  ------------------------------------------------------------------
  function X_Process_Event(P_Line_Id : System.Address;
                           P_Kind    : System.Address;
                           P_Next    : System.Address) return Result;
  pragma Import(C, X_Process_Event, "x_process_event");


  -- Prepare the registration of a new task
  -- Ensure that the dispatcher will not remain blocked in select
  procedure Pre_Register is
  begin
    Wake_Up;
  end Pre_Register;

  -- From x_process_event
  C_Xevent_Discard        : constant Integer := 0;
  pragma Unreferenced (C_Xevent_Discard);
  C_Xevent_Mouse_Release  : constant Integer := 1;
  C_Xevent_Mouse_Press    : constant Integer := 2;
  C_Xevent_Keyboard       : constant Integer := 3;
  C_Xevent_Refresh        : constant Integer := 4;
  C_Xevent_Mouse_Motion   : constant Integer := 5;
  C_Xevent_Exit_Request   : constant Integer := 6;
  C_Xevent_Selection      : constant Integer := 7;

  -- Fetch a pending event and map it to our events
  -- C_Id is the owner of the event
  procedure Xx_Get_Event (C_Id : out Line_For_C;
                          Event : out Event_Kind;
                          Next : out Boolean) is
    C_Event : Integer;
    C_Next : Bool_For_C;
    C_Res : Result;
  begin
    C_Res :=  X_Process_Event (C_Id'Address,
                               C_Event'Address, C_Next'Address);
    if C_Res /= Ok then
      Log ("Xx_Get_Event", No_Client_No, "-> X_Process_Event ERROR");
      raise X_Failure;
    end if;

    -- Translate X event
    Next := Boolean (C_Next);
    if C_Event = C_Xevent_Mouse_Release then
      Event := Tid_Release;
    elsif C_Event = C_Xevent_Mouse_Press then
      Event := Tid_Press;
    elsif C_Event = C_Xevent_Keyboard then
      Event := Keyboard;
    elsif C_Event = C_Xevent_Refresh then
      Event := Refresh;
    elsif C_Event = C_Xevent_Mouse_Motion then
      Event := Tid_Motion;
    elsif C_Event = C_Xevent_Exit_Request then
      Event := Exit_Request;
    elsif C_Event = C_Xevent_Selection then
      Event := Selection;
    else
      -- Discard, or Invalid X event
      Event := Timeout;
    end if;
    Log ("Xx_Get_Event", No_Client_No, "-> " & Event'Img);
  end Xx_Get_Event;

  -- From x_Select
  C_Select_No_Event   : constant Integer := -01;
  C_Select_Sig_Event  : constant Integer := -02;
  C_Select_Wake_Event : constant Integer := -03;
  C_Select_X_Event    : constant Integer := -10;

  -- Fetch a new event (if C_Id is set)
  -- Otherwise The real call to select
  procedure Xx_Select (Exp : in Timers.Expiration_Rec;
                       C_Id : in out Line_For_C;
                       Event : out Event_Rec;
                       Next : out Boolean) is

    -- Next expiration
    Select_Exp : Timers.Expiration_Rec;
    Timeout_Val : C_Types.Timeval_T;
    Now : Ada.Calendar.Time;
    -- For C x_select
    C_Fd    : C_Types.Int;
    C_Read  : Bool_For_C;
    C_Res : Result;
    -- For Event_Mng.Handle
    Handle_Event : Boolean;
    Evt_In : Event_Mng.Event_Rec;
    Evt_Out : Event_Mng.Out_Event_List;

    use type Ada.Calendar.Time, System.Address,
             Timers.Expiration_Rec,
             Event_Mng.Out_Event_List,
             Perpet.Delta_Rec;
  begin

    loop
      -- Compute min of Exp and timers, set timeout in Ms
      Select_Exp := Timers.Next_Expiration (Exp);
      if Select_Exp = Timers.Infinite_Expiration then
        Timeout_Val := Timeval.Infinite_C_Timeout;
      else
        Now := Ada.Calendar.Clock;
        if Now < Select_Exp.Time then
          Timeout_Val := Timeval.To_C_Timeout (Select_Exp.Time - Now);
        else
          Timeout_Val := (0, 0);
        end if;
      end if;

      -- Call the real select
      Log ("Xx_Select", No_Client_No, "timeout " & Timeval.Image (Timeout_Val));
      C_Res := X_Select (C_Fd'Address, C_Read'Address, Timeout_Val'Address);
      if C_Res /= Ok then
        Log ("Xx_Select.X_Select", No_Client_No, "-> ERROR");
        raise X_Failure;
      end if;
      Log ("Xx_Select.X_Select", No_Client_No,
            "-> " & Integer'Image(C_Fd) & " " & Bool_For_C'Image(C_Read));

      -- For all but X:
      C_Id := No_Line_For_C;
      Handle_Event := True;
      Next := False;

      -- Treat result: set Evt_In to handle  for general events
      --               set event to X event if valid
      --               go on if Discard
      if C_Fd = C_Select_No_Event then
        Evt_In := (Kind => Event_Mng.Timeout);
      elsif C_Fd = C_Select_Sig_Event then
        Evt_In := (Kind => Event_Mng.Signal_Event);
      elsif C_Fd = C_Select_Wake_Event then
        -- A wakeup event to transmit to Wait
        Handle_Event := False;
        Event := (Internal => True, Internal_Kind => Wakeup_Event);
      elsif C_Fd = C_Select_X_Event then
        -- Get X event and its owner
        Handle_Event := False;
        Event := (Internal => False, Kind => Timeout);
        Xx_Get_Event (C_Id, Event.Kind, Next);
      else
        -- A fd
        Evt_In := (Kind => Event_Mng.Fd_Event,
                   Fd   => Event_Mng.File_Desc(C_Fd),
                   Read => Boolean(C_Read));
      end if;

      if Handle_Event then
        -- Handle non X nor wakeup event and convert
        Evt_Out := Event_Mng.Handle (Evt_In);
        case Evt_Out is
          when Event_Mng.Timer_Event =>
            Event := (Internal => False, Kind => Timer_Event);
          when Event_Mng.Fd_Event =>
            Event := (Internal => False, Kind => Fd_Event);
          when Event_Mng.Signal_Event =>
            Event := (Internal => False, Kind => Signal_Event);
          when Event_Mng.Timeout =>
            Event := (Internal => False, Kind => Timeout);
        end case;

        -- Done on select timeout (No_Event) or an event to report
        exit when Evt_In.Kind = Event_Mng.Timeout
        or else Event.Kind /= Timeout;
      else
        -- X event or private event, done if valid
        exit when Event.Internal or else Event.Kind /= Timeout;
      end if;

    end loop;

    if Event.Internal then
      Log ("Xx_Select", No_Client_No,
           "-> " & Event.Internal_Kind'Img & " " & Next'Img);
    else
      Log ("Xx_Select", No_Client_No,
           "-> " & Event.Kind'Img & " " & Next'Img);
    end if;
  end Xx_Select;

  -------------------------------------

  protected body Dispatcher is

    -- Initialize only once, suspended by default
    procedure Initialize is
      Res : Boolean;
    begin
      if Initialized then
        Log ("Initialize", No_Client_No, "already initialized");
        raise Dispatch_Error;
      end if;
      Res := X_Suspend = Ok;
      if not Res then
        raise X_Failure;
      end if;
      Initialized := True;
    end Initialize;

    -- First Client free
    function First_Free return Line_Range is
    begin
      for I in Client_Range loop
        if not Clients(I).Used then
          return I;
        end if;
      end loop;
      return No_Client_No;
    end First_Free;

    -- First, Last, Next, Prev Client used
    function First return Line_Range is
    begin
      for I in Client_Range loop
        if Clients(I).Used then
          return I;
        end if;
      end loop;
      return No_Client_No;
    end First;

    function Next (C : Client_Range) return Line_Range is
    begin
      if C = Client_Range'Last then
        return No_Client_No;
      end if;
      for I in Client_Range'Succ(C) .. Client_Range'Last loop
        if Clients(I).Used then
          return I;
        end if;
      end loop;
      return No_Client_No;
    end Next;

    -- First to expire
    function Closest return Client_Range is
      I : Line_Range;
      Exp : Timers.Expiration_Rec;
      Res : Client_Range;
      use type Timers.Expiration_Rec;
    begin
      I := First;
      if I = No_Client_No then
        Log ("Closest", No_Client_No, "no client");
        raise Dispatch_Error;
      end if;
      Res := I;
      Exp := Clients(Res).Wait_Exp;
      loop
        I := Next (I);
        exit when I = No_Client_No;
        if Clients(I).Wait_Exp < Exp then
          Res := I;
          Exp := Clients(Res).Wait_Exp;
        end if;
      end loop;
      return Res;
    end Closest;

    -- Find by C line
    function Find_From_C (C_Id : Line_For_C) return Line_Range is
      use type System.Address;
    begin
      for I in Client_Range loop
        if Clients(I).Used
        and then Clients(I).Line_For_C_Id = C_Id then
          return I;
        end if;
      end loop;
      return No_Client_No;
    end Find_From_C;

    -- First registered
    function Oldest return Client_Range is
      I : Line_Range;
      Birth : Ada.Calendar.Time;
      Res : Client_Range;
      use type Ada.Calendar.Time;
    begin
      I := First;
      if I = No_Client_No then
        Log ("Oldest", No_Client_No, "no client");
        raise Dispatch_Error;
      end if;
      Res := I;
      Birth := Clients(Res).Birth;
      loop
        I := Next (I);
        exit when I = No_Client_No;
        if Clients(Res).Birth < Birth then
          Res := I;
          Birth := Clients(Res).Birth;
        end if;
      end loop;
      return Res;
    end Oldest;

    -- Check client is known and is allowed to be active
    procedure Check (Client : in Line_Range;
                     Check_X : in Boolean;
                     Running : in Boolean := True) is
    begin
      if Client not in Client_Range then
        Log ("Check", Client, "not registered");
        raise X_Failure;
      end if;
      if not Clients(Client).Used then
        Log ("Check", Client, "unknown");
        raise Dispatch_Error;
      end if;
      if Clients(Client).Running /= Running then
        Log ("Check", Client, "unexpected");
        raise Dispatch_Error;
      end if;
      if Check_X and then In_X then
        Log ("Check", Client, "in X");
        raise Dispatch_Error;
      end if;
    end Check;

    -- Register one client at a time
    -- Find free slot, init it, incease client count
    entry Register (Client : out Line_Range)
                   when Registered = No_Client_No
                   and then not In_X is
      Res : Boolean;
    begin
      Client := First_Free;
      if Client = No_Client_No then
        -- Too many clients
        Log ("Register", No_Client_No, "too many lines");
        return;
      end if;
      -- Update the client admin info
      Clients(Client).Used := True;
      Clients(Client).Running := True;
      Clients(Client).Refreshing := False;
      Clients(Client).Birth := Ada.Calendar.Clock;
      Clients(Client).Line_For_C_Id := No_Line_For_C;
      -- Update global admin info
      Registered := Client;
      Nb_Clients := Nb_Clients + 1;

      if Nb_Clients = 1 then
        -- First client => resume
        Log ("Register", No_Client_No, "resuming");
        Res := X_Resume = Ok;
        if not Res then
          Log ("Register", No_Client_No, "X_Failure");
          raise X_Failure;
        end if;
      end if;
      Log ("Register", Client, Nb_Clients'Img & " " & Nb_Waiting'Img);
    end Register;

    -- Unregister
    -- Count clients and refresh all on Unregister
    entry Unregister (Client : in out Line_Range)
                     when not In_X is
      Res : Boolean;
    begin
      if Client = No_Client_No then
        Log ("Unregister", Client, "invalid");
        raise Dispatch_Error;
      end if;
      Check (Client, True);
      Log ("Unregister", Client, Nb_Clients'Img & " " & Nb_Waiting'Img);
      -- I am executing, anyway Nb_Waiting is unchanged
      -- This client is not used (reset slot before calling First)
      Nb_Clients := Nb_Clients - 1;
      Clients(Client).Used := False;
      -- If I was registering... then I am not any more
      if Client = Registered then
        Registered := No_Client_No;
      end if;
      -- All remaining clients need to receive a Refresh event
      -- Start from the first client
      Selected := First;
      if Selected /= No_Client_No then
        -- At least one client remaining, refresh all
        Refresh_All := True;
        -- Deliver a Refresh event to this client,
        Event := (Internal => False, Kind => Refresh);
        Next_Event := False;
        Nb_X_Events := 0;
        -- This client is the first of the cohort so it must handle the event
        Clients(Selected).Refreshing := True;
      else
        -- Last client => suspend
        Log ("Unregister", No_Client_No, "suspending");
        Res := X_Suspend = Ok;
        if not Res then
          Log ("Register", No_Client_No, "X_Failure");
          raise X_Failure;
        end if;
      end if;

      Client := No_Client_No;
      Log ("Unregister", Selected, "is selected");
    end Unregister;

    -- Two calls to protect a call to X
    entry Call_On  (Client : in Line_Range;
                    Line_For_C_Id : out Line_For_C) when not In_X is
    begin
      Check (Client, True);
      In_X := True;
      Line_For_C_Id := Clients(Client).Line_For_C_Id;
    end Call_On;

    procedure Call_Off (Client : in Client_Range;
                    New_Line_For_C_Id : in Line_For_C) is
    begin
      Check (Client, False);
      if not In_X then
        Log ("Call_Off", Client, "not in X");
        raise Dispatch_Error;
      end if;
      In_X := False;
      Clients(Client).Line_For_C_Id := New_Line_For_C_Id;
    end Call_Off;

    -- Ready to wait, store expiration time. Select if last
    --  and no client to wake up
    entry Prepare (Client : in Client_Range;
                   Exp : in Timers.Expiration_Rec) when not In_X is
      New_Client : Line_Range;
      Got_Id : Line_For_C;
      use type System.Address; -- for checking Line_For_C
    begin
      Check (Client, True);
      Log ("Prepare", Client, Nb_Clients'Img & " " & Nb_Waiting'Img);
      if Exp.Infinite then
        Log  ("Prepare", Client, "Exp infinite at "
            & Images.Date_Image (Ada.Calendar.Clock));
      else
        Log  ("Prepare", Client, "Exp " & Images.Date_Image (Exp.Time) & " at "
            & Images.Date_Image (Ada.Calendar.Clock));
      end if;

      -- Update client data with desired expiration
      Clients(Client).Wait_Exp := Exp;

      -- First, handle registration
      if Client = Registered then
        -- This is the end of my registration
        -- Allow other (Un) Register
        Registered := No_Client_No;
      end if;

      -- Second, handle global refreshing
      if Refresh_All then
        Log ("Prepare", Client, "refresh_all");
        -- This was started in Unregister or on Refresh X event by first client
        -- Refresh first or next client (deliver a Refresh event)
        if Clients(Client).Refreshing then
          -- I was the first when the global refresh was triggered
          -- (in Unregister or on Refresh X event).
          -- Send a refresh event to myself
          New_Client := Client;
          Clients(Client).Refreshing := False;
        else
          -- Try to pass the Refresh event to next client (if any)
          New_Client := Next (Client);
        end if;
        if New_Client /= No_Client_No then
          -- One client to pass the event to (possibly me)
          Selected := New_Client;
          Event := (Internal => False, Kind => Refresh);
          Next_Event := False;
          Log ("Prepare", Selected, "will be refreshing");
          Log ("Prepare", Client, "goes waiting");
          -- I go to wait
          Clients(Client).Running := False;
          Nb_Waiting := Nb_Waiting + 1;
          return;
        end if;
        -- No more client to refresh, end of global refresh
        Refresh_All := False;
        Log ("Prepare", No_Client_No, "end of refresh");
      end if;

      -- Third, go waiting if we are not the last running
      if Nb_Waiting /= Nb_Clients - 1 then
        -- All but the last must go/remain waiting
        Selected := No_Client_No;
        Log ("Prepare", Client, "goes waiting cause not last");
        -- I go waiting
        Clients(Client).Running := False;
        Nb_Waiting := Nb_Waiting + 1;
        return;
      end if;
      -- Now I am the last client Running

      -- Fourth, priority is given to X events,
      -- but amount of successive X events is limited to prevent starvation
      if not Next_Event then
        Nb_X_Events := 0;
      end if;
      if Selected /= No_Client_No
      and then Next_Event
      and then Nb_X_Events = Max_Successive_X then
        -- Give up with X events and dispatch other events to oldest client
        Selected := Oldest;
        Event := (Internal => True, Internal_Kind => Dispatch_Event);
        Next_Event := False;
        Log ("Prepare", Selected, "will be dispatching non X events");
        Log ("Prepare", Client, "goes waiting");
        -- I go waiting
        Clients(Client).Running := False;
        Nb_Waiting := Nb_Waiting + 1;
        return;
      end if;

      -- Fifth, try to fetch a pending X event
      Event := (Internal => False, Kind => Timeout);
      if Selected /= No_Client_No
      and then Next_Event then
        New_Client := Selected;
        Got_Id := Clients(New_Client).Line_For_C_Id;
        Xx_Get_Event (Got_Id, Event.Kind, Next_Event);
      end if;

      -- Sixth, C select if no pending
      if Event.Kind = Timeout then
        -- Select on smaller delay
        New_Client := Closest;
        Log ("Prepare", Client, "is selecting for " & New_Client'Img);
        -- Call the C select
        Xx_Select (Clients(New_Client).Wait_Exp, Got_Id, Event, Next_Event);
      else
        Log ("Prepare", Client, "gets direct event for " & New_Client'Img);
      end if;

      -- Seventh, dispatch resulting event, set Selected
      if Event.Internal then
        case Event.Internal_Kind is
          when Wakeup_Event =>
            -- A wake up, dispatch by Closest
            Selected := New_Client;
            Next_Event := False;
          when others =>
            Log ("Prepare", New_Client, "unexpected private event");
            raise Dispatch_Error;
        end case;
      else
        case Event.Kind is
          when Keyboard | Tid_Release | Tid_Press | Tid_Motion
             | Refresh | Exit_Request | Selection =>
            -- A X event to deliver to proper client (optim: try Selected)
            if Selected = No_Client_No
            or else Clients(Selected).Line_For_C_Id /= Got_Id then
              Selected := Find_From_C (Got_Id);
            end if;
            Nb_X_Events := Nb_X_Events + 1;
            if Event.Kind = Refresh and then Nb_Clients /= 1 then
              -- Receiveing a X refresh event and at least one other client
              --  => refresh all
              Log ("Prepare", New_Client, "X refresh => refresh_all");
              Selected := First;
              Refresh_All := True;
              for I in Client_Range loop
                if Clients(I).Used then
                  Clients(Client).Refreshing := False;
                end if;
              end loop;
              Clients(Selected).Refreshing := True;
              -- Generate a dummy masked event
              Event := (Internal => True, Internal_Kind => Dispatch_Event);
            end if;
          when Timer_Event | Fd_Event | Signal_Event =>
            -- A general event to deliver to oldest
            Selected := Oldest;
            Next_Event := False;
          when Timeout =>
            -- Timeout on select, to deliver to closest
            Selected := New_Client;
            Next_Event := False;
        end case;
        -- When this event belongs to no registered client
        -- Send a "dummy" refresh event to oldest
        if Selected = No_Client_No then
          Selected := Oldest;
          Event := (Internal => False, Kind => Refresh);
          Next_Event := False;
          Log ("Prepare", Selected, " gets a dummy refresh");
        end if;
      end if;

      -- Nice work! time to go to wait
      Nb_Waiting := Nb_Waiting + 1;
      Clients(Client).Running := False;
      if Event.Internal then
        Log ("Prepare", Selected, "<= " & Event.Internal_Kind'Img);
      else
        Log ("Prepare", Selected, "<= " & Event.Kind'Img);
      end if;
      Log ("Prepare", Client, "goes waiting");

    end Prepare;

    -- All but one client wait here, eventually getting and event
    --  from select [ process_event ]
    entry Wait_Event (for Client in Client_Range) (New_Event : out Event_Rec)
          when not In_X and then Client = Selected is
    begin
      Check (Client, True, False);
      -- I become running with this event
      Clients(Selected).Running := True;
      Nb_Waiting := Nb_Waiting - 1;
      New_Event := Event;
      if New_Event.Internal then
        Log ("Wait_Event", Client, "<- " & New_Event.Internal_Kind'Img);
      else
        Log ("Wait_Event", Client, "<- " & New_Event.Kind'Img);
      end if;
    end Wait_Event;

  end Dispatcher;

end Dispatch;

