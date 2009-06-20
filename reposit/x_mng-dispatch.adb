with Date_Image;

separate (X_Mng)
package body Dispatch is

  -- Wake-up the task that is in select
  procedure C_Wake_Up;
  pragma Import(C, C_Wake_Up, "evt_wake_up");
  procedure Wake_Up is
  begin
    C_Wake_Up;
  end Wake_Up;

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
      if Debug then
        My_Io.Put_Line ("  Xx_Get_Event -> x_process_event ERROR");
      end if;
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
      Event := No_Event;
    end if;
    if Debug then
      My_Io.Put_Line ("  Xx_Get_Event -> " & Event'Img);
    end if;
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
    Timeout : Duration;
    -- For C x_select
    C_Fd    : Integer;
    C_Read  : Bool_For_C;
    C_Timeout_Ms : Integer;
    C_Res : Result;
    -- For Event_Mng.Handle
    Handle_Event : Boolean;
    Evt_In : Event_Mng.Event_Rec;
    Evt_Out : Event_Mng.Out_Event_List;

    use type Ada.Calendar.Time, System.Address,
             Timers.Expiration_Rec,
             Event_Mng.Out_Event_List;
  begin

    loop
      -- Compute min of Exp and timers, set timeout in Ms
      Select_Exp := Timers.Next_Expiration (Exp);
      if Select_Exp = Timers.Infinite_Expiration then
        Timeout := Infinite_Timeout;
      else
        Timeout := Select_Exp.Time - Ada.Calendar.Clock;
        if Timeout < 0.0 then
          Timeout := 0.0;
        end if;
      end if;
      C_Timeout_Ms := Integer(Timeout * 1000.0);

      -- Call the real select
      if Debug then
        My_Io.Put_Line ("  Xx_Select timeout " & C_Timeout_Ms'Img);
      end if;
      C_Res := X_Select (C_Fd'Address, C_Read'Address, C_Timeout_Ms'Address);
      if C_Res /= Ok then
        if Debug then
          My_Io.Put_Line ("  Xx_Select.x_select -> ERROR");
        end if;
        raise X_Failure;
      end if;
      if Debug then
        My_Io.Put_Line ("  Xx_Select.x_select -> " & Integer'Image(C_Fd)
                                    & " " & Bool_For_C'Image(C_Read));
      end if;

      -- For all but X:
      C_Id := No_Line_For_C;
      Handle_Event := True;
      Next := False;

      -- Treat result: set Evt_In to handle  for general events
      --               set event to X event if valid
      --               go on if Discard
      if C_Fd = C_Select_No_Event then
        Evt_In := (Kind => Event_Mng.No_Event);
      elsif C_Fd = C_Select_Sig_Event then
        Evt_In := (Kind => Event_Mng.Signal_Event);
      elsif C_Fd = C_Select_Wake_Event then
        -- A wakeup event to transmit to Wait
        Handle_Event := False;
        Event := (Prv => True, Prv_Kind => Wakeup_Event);
      elsif C_Fd = C_Select_X_Event then
        -- Get X event and its owner
        Handle_Event := False;
        Event := (False, No_Event);
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
            Event := (False, Timer_Event);
          when Event_Mng.Fd_Event =>
            Event := (False, Fd_Event);
          when Event_Mng.Signal_Event =>
            Event := (False, Signal_Event);
          when Event_Mng.No_Event =>
            Event := (False, No_Event);
        end case;

        -- Done on select timeout (No_Event) or an event to report
        exit when Evt_In.Kind = Event_Mng.No_Event
        or else Event.Kind /= No_Event;
      else
        -- X event or private event, done if valid
        exit when Event.Prv or else Event.Kind /= No_Event;
      end if;

    end loop;

    if Debug then
      if Event.Prv then
        My_Io.Put_Line ("  Xx_Select -> " & Event.Prv_Kind'Img
                      & " " & Next'Img);
      else
        My_Io.Put_Line ("  Xx_Select -> " & Event.Kind'Img
                      & " " & Next'Img);
      end if;
    end if;
  end Xx_Select;

  -------------------------------------

  protected body Dispatcher is

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
        if Debug then
          My_Io.Put_Line ("Dispatch.Closest: no client");
        end if;
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
        if Debug then
          My_Io.Put_Line ("Dispatch.Oldest: no client");
        end if;
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
    procedure Check (Client : in Client_Range;
                     Check_X : in Boolean;
                     Running : in Boolean := True) is
    begin
      if not Clients(Client).Used then
        if Debug then
          My_Io.Put_Line ("Dispatch.Check: unknown client " & Client'Img);
        end if;
        raise Dispatch_Error;
      end if;
      if Clients(Client).Running /= Running then
        if Debug then
          My_Io.Put_Line ("Dispatch.Check: unexpected client "
                        & Client'Img);
        end if;
        raise Dispatch_Error;
      end if;
      if Check_X and then In_X then
        if Debug then
          My_Io.Put_Line ("Dispatch.Check: in X client " & Client'Img);
        end if;
        raise Dispatch_Error;
      end if;
    end Check;

    -- Register one client at a time
    -- Find free slot, init it, incease client count
    entry Register (Client : out Line_Range)
                   when Registered = No_Client_No is
    begin
      if In_X then
        if Debug then
          My_Io.Put_Line ("Dispatch.Register: in X client " & Client'Img);
        end if;
        raise Dispatch_Error;
      end if;
      Client := First_Free;
      if Client = No_Client_No then
        -- Too many clients
        if Debug then
          My_Io.Put_Line ("Dispatch.Register: too many clients");
        end if;
        return;
      end if;
      -- Update the client admin info
      Clients(Client).Used := True;
      Clients(Client).Running := True;
      Clients(Client).Refreshing := False;
      Clients(Client).Registering := True;
      Clients(Client).Birth := Ada.Calendar.Clock;
      Clients(Client).Line_For_C_Id := No_Line_For_C;
      -- Update global admin info
      Registered := Client;
      Nb_Clients := Nb_Clients + 1;
      if Debug then
        My_Io.Put_Line ("Dispatch.Register " & Client'Img
                 & " " & Nb_Clients'Img & " " & Nb_Waiting'Img);
      end if;
    end Register;

    -- Unregister, not while registration in progress
    -- Count clients and refreshh all on Unregister
    entry Unregister (Client : in out Line_Range)
                     when Registered = No_Client_No is
    begin
      if Client = No_Client_No then
        if Debug then
          My_Io.Put_Line ("Dispatch.Unregister: invalid client");
        end if;
        raise Dispatch_Error;
      end if;
      Check (Client, True);
      if Debug then
        My_Io.Put_Line ("Dispatch.Unregister " & Client'Img
                 & " " & Nb_Clients'Img & " " & Nb_Waiting'Img);
      end if;
      -- I am executing, anyway Nb_Waiting is unchanged
      -- This client is not used (reset slot before calling First)
      Nb_Clients := Nb_Clients - 1;
      Clients(Client).Used := False;
      -- All remaining clients need to receive a Refresh event
      -- Start from the first client
      Selected := First;
      if Selected /= No_Client_No then
        -- At least one client remaining, refresh all
        Refresh_All := True;
        -- Deliver a Refresh event to this client,
        Event := (False, Refresh);
        Next_Event := False;
        Nb_X_Events := 0;
        -- This client is the first of the cohort so it must handle the event
        Clients(Selected).Refreshing := True;
      end if;

      Client := No_Client_No;
      if Debug then
        My_Io.Put_Line ("Dispatch.Unregister selected "
                       & Line_Range'Image(Selected));
      end if;
    end Unregister;

    -- Two calls to protect a call to X
    entry Call_On  (Client : in Client_Range;
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
        if Debug then
          My_Io.Put_Line ("Dispatch.Call_Off: not in X client "
                        & Client_Range'Image(Client));
        end if;
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
      if Debug then
        My_Io.Put ("Dispatch.Prepare: " & Client'Img
                 & " " & Nb_Clients'Img & " " & Nb_Waiting'Img);
        if Exp.Infinite then
          My_Io.Put_Line (" infinite");
        else
           My_Io.Put_Line (" " & Date_Image (Exp.Time));
        end if;
      end if;

      -- Update client data with desired expiration
      Clients(Client).Wait_Exp := Exp;

      -- First, handle registration
      if Registered /= No_Client_No then
        if Nb_Waiting = Nb_Clients - 1 then
          -- I am the last running task: I need to deliver a
          -- Refresh event to the registering task (possibly me)
          Selected := Registered;
          Event := (False, Refresh);
          Next_Event := False;
        else
          -- Some other task is running that will deliver the Refresh
          Selected := No_Client_No;
        end if;
          if Debug then
            My_Io.Put_Line ("Dispatch.Prepare: registering -> " & Selected'Img);
          end if;
        -- In any case, I go to Wait
        Clients(Client).Running := False;
        Nb_Waiting := Nb_Waiting + 1;
        return;
      end if;

      -- Second, handle global refreshing
      if Refresh_All then
        -- This was started in Unregister by first client
        -- Refresh first or next client (deliver a Refresh event)
        if Clients(Client).Refreshing then
          -- I was the first when the global refresh was triggered
          -- (in Unregister). Send a refresh event to myself
          New_Client := Client;
          Clients(Client).Refreshing := False;
        else
          -- Try to pass the Refresh event to next client (if any)
          New_Client := Next (Client);
        end if;
        if New_Client /= No_Client_No then
          -- One client to pass the event to (possibly me)
          Selected := New_Client;
          Event := (False, Refresh);
          Next_Event := False;
          if Debug then
            My_Io.Put_Line ("Dispatch.Prepare: refreshing -> " & Selected'Img);
          end if;
          -- I go to wait
          Clients(Client).Running := False;
          Nb_Waiting := Nb_Waiting + 1;
          return;
        end if;
        -- No more client to refresh, end of global refresh
        Refresh_All := False;
        if Debug then
          My_Io.Put_Line ("Dispatch.Wait: end of refresh");
        end if;
      end if;

      -- Third, go waiting if we are not the last running
      if Nb_Waiting /= Nb_Clients - 1 then
        -- All but the last must go/remain waiting
        Selected := No_Client_No;
        if Debug then
          My_Io.Put_Line ("Dispatch.Prepare: " & Client'Img & " goes waiting");
        end if;
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
        Event := (True, Dispatch_Event);
        Next_Event := False;
        if Debug then
          My_Io.Put_Line ("Dispatch.Prepare: " & Selected'Img
                        & " will dispatch non X events");
        end if;
        -- I go waiting
        Clients(Client).Running := False;
        Nb_Waiting := Nb_Waiting + 1;
        return;
      end if;

      -- Fifth, try to fetch a pending X event
      Event := (False, No_Event);
      if Selected /= No_Client_No
      and then Next_Event then
        New_Client := Selected;
        Got_Id := Clients(New_Client).Line_For_C_Id;
        Xx_Get_Event (Got_Id, Event.Kind, Next_Event);
      end if;

      -- Sixth, C select if no pending
      if Event.Kind = No_Event then
        -- Select on smaller delay
        New_Client := Closest;
        if Debug then
          My_Io.Put_Line ("Dispatch.Prepare: selecting " & New_Client'Img);
        end if;
        -- Call the C select
        Xx_Select (Clients(New_Client).Wait_Exp, Got_Id, Event, Next_Event);
      else
        if Debug then
          My_Io.Put_Line ("Dispatch.Prepare: got direct event "
                        & New_Client'Img);
        end if;
      end if;

      -- Seventh, dispatch resulting event
      if Event.Prv then
        case Event.Prv_Kind is
          when Wakeup_Event =>
            -- A wake up, dispatch by Closest
            Selected := New_Client;
            Next_Event := False;
          when others =>
            if Debug then
              My_Io.Put_Line ("Dispatch.Prepare: unexpected private event "
                            & New_Client'Img);
            end if;
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
          when Timer_Event | Fd_Event | Signal_Event =>
            -- A general event to deliver to oldest
            Selected := Oldest;
            Next_Event := False;
          when No_Event =>
            -- Timeout on select, to deliver to closest
            Selected := New_Client;
            Next_Event := False;
        end case;
        -- When this event belongs to no registered client
        -- Send a "dummy" refresh event to oldest
        if Selected = No_Client_No then
          Selected := Oldest;
          Event := (False, Refresh);
          Next_Event := False;
          if Debug then
            My_Io.Put_Line ("Dispatch.Prepare: no selected => " & Selected'Img);
          end if;
        end if;
      end if;

      -- Nice work! time to go to wait
      Nb_Waiting := Nb_Waiting + 1;
      Clients(Client).Running := False;
      return;

    end Prepare;

    -- All but one client wait here, eventually getting and event
    --  from select [ process_event ]
    entry Wait_Event (for Client in Client_Range) (Kind : out Event_Rec)
          when not In_X and then Client = Selected is
    begin
      Check (Client, True, False);
      if Clients(Client).Registering then
        -- This is the end of my registration
        -- Allow other (Un) Register
        Clients(Client).Registering := False;
        Registered := No_Client_No;
      end if;
      -- I become running with this event
      Clients(Selected).Running := True;
      Nb_Waiting := Nb_Waiting - 1;
      Kind := Event;
      if Debug then
        if Kind.Prv then
          My_Io.Put_Line ("Dispatch.Wait_Event: " & Client'Img
                       & " <- " & Kind.Prv_Kind'Img);
        else
          My_Io.Put_Line ("Dispatch.Wait_Event: " & Client'Img
                       & " <- " & Kind.Kind'Img);
        end if;
      end if;
    end Wait_Event;

  end Dispatcher;

end Dispatch;

