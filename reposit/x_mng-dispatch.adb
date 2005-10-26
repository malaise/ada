with Event_Mng, Date_Image;

separate (X_Mng)
package body Dispatch is

  Register_Waiting : Natural := 0;
  procedure Wake_Up is
  begin
    Register_Waiting := Register_Waiting + 1;
    Event_Mng.Wake_Up;
  end Wake_Up;

  -- From x_process_event
  C_Xevent_Discard        : constant Integer := 0;
  C_Xevent_Mouse_Release  : constant Integer := 1;
  C_Xevent_Mouse_Press    : constant Integer := 2;
  C_Xevent_Keyboard       : constant Integer := 3;
  C_Xevent_Refresh        : constant Integer := 4;
  C_Xevent_Mouse_Motion   : constant Integer := 5;

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
    else
      -- Discard, or Invalid X event
      Event := No_Event;
    end if;
  end Xx_Get_Event;

  -- From x_Select
  C_Select_No_Event   : constant Integer := -01;
  C_Select_Sig_Event  : constant Integer := -02;
  C_Select_Wake_Event : constant Integer := -03;
  C_Select_X_Event    : constant Integer := -10;

  -- The real call to select
  procedure Xx_Select (Exp : in Timers.Expiration_Rec;
                       C_Id : in out Line_For_C;
                       Event : out Event_Kind;
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
             Event_Mng.In_Event_List, Event_Mng.Out_Event_List;
  begin

    if C_Id /= No_Line_For_C then
      -- Try to get a new event from prev line
      Xx_Get_Event (C_Id, Event, Next);
      if Event /= No_Event then
        return;
      end if;
    end if;

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
        Evt_In := (Kind => Event_Mng.Wakeup_Event);
      elsif C_Fd = C_Select_X_Event then
        -- Get X event
        Handle_Event := False;
        Xx_Get_Event (C_Id, Event, Next);
      else
        -- A fd
        Evt_In := (Kind => Event_Mng.Fd_Event,
                   Fd   => Event_Mng.File_Desc(C_Fd),
                   Read => Boolean(C_Read));
      end if;

      -- Handle event
      if Handle_Event then
        Evt_Out := Event_Mng.Handle (Evt_In);
        case Evt_Out is
          when Event_Mng.Timer_Event =>
            Event := Timer_Event;
          when Event_Mng.Fd_Event =>
            Event := Fd_Event;
          when Event_Mng.Signal_Event =>
            Event := Signal_Event;
          when Event_Mng.Wakeup_Event =>
            Event := Wakeup_Event;
          when Event_Mng.No_Event =>
            Event := No_Event;
        end case;

        -- Done on select timeout (No_Event) or an event to report
        exit when Evt_In.Kind = Event_Mng.No_Event
        or else Event /= No_Event;
      else
        -- X event, done if valid
        exit when Event  /= No_Event;
      end if;

    end loop;

    if Debug then
      My_Io.Put_Line ("  Xx_Select -> " & Event'Img & " " & Next'Img);
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
    procedure Check (Client : in Client_Range; Check_X : Boolean) is
    begin
      if not Clients(Client).Used then
        if Debug then
          My_Io.Put_Line ("Dispatch.Check: unknown client " & Client'Img);
        end if;
        raise Dispatch_Error;
      end if;

      if not Clients(Client).Running then
        if Debug then
          My_Io.Put_Line ("Dispatch.Check: unexpected client " & Client'Img);
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

    -- Register: Find free slot, init it, incease client count
    entry Register (Client : out Line_Range) when True is
    begin
      Register_Waiting := Register_Waiting - 1;
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
      Clients(Client).Used := True;
      Clients(Client).Birth := Ada.Calendar.Clock;
      Clients(Client).Running := True;
      Clients(Client).Line_For_C_Id := No_Line_For_C;
      Nb_Clients := Nb_Clients + 1;
      if Debug then
        My_Io.Put_Line ("Dispatch.Register " & Client'Img
                 & " " & Nb_Clients'Img & " " & Nb_Waiting'Img);
      end if;
    end Register;

    -- Unregister. Count clients and refreshh all on Unregister
    procedure Unregister (Client : in out Line_Range) is
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
      -- This client is not used (set it before calling First)
      Nb_Clients := Nb_Clients - 1;
      Clients(Client).Used := False;
      Selected := First;
      Event := Refresh;
      if Selected /= No_Client_No then
        -- At least one client remaining
        Refreshing := True;
        if not Clients(Selected).Running then
          -- The selected client was waiting
          Nb_Waiting := Nb_Waiting - 1;
          Clients(Selected).Running := True;
        end if;
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
    --  and no client ti wake up
    entry Wait (Client : in Client_Range;
                Exp : in Timers.Expiration_Rec) when not In_X is
      First_Client : Client_Range;
      Got_Id : Line_For_C;
      use type System.Address, Event_Mng.Out_Event_List;
    begin
      Check (Client, True);
      -- Update client data
      if Debug then
        My_Io.Put ("Dispatch.Wait: " & Client'Img
                 & " " & Nb_Clients'Img & " " & Nb_Waiting'Img);
        if Exp.Infinite then
          My_Io.Put_Line (" infinite");
        else
           My_Io.Put_Line (" " & Date_Image (Exp.Time));
        end if;
      end if;
      Clients(Client).Wait_Exp := Exp;

      -- Do/Update refreshing
      if Refreshing then
        Nb_X_Events := 0;
        Next_Event := False;
        Selected := Next (Client);
        Event := Refresh;
        if Selected = No_Client_No then
          -- No more client to refresh
          Refreshing := False;
          if Debug then
            My_Io.Put_Line ("Dispatch.Wait: end of refresh");
          end if;
        else
          return;
        end if;
      end if;

      -- All but last client will wait in Get_Event
      Clients(Client).Running := False;
      if Nb_Waiting /= Nb_Clients - 1 then
        Selected := No_Client_No;
        Nb_Waiting := Nb_Waiting + 1;
        if Debug then
          My_Io.Put_Line ("Dispatch.Wait: sleep " & Client'Img);
        end if;
        return;
      end if;

      -- Limit amount of successive X events
      if Next_Event and then Nb_X_Events = Max_Successive_X then
        Nb_X_Events := 0;
        Next_Event := False;
        -- Try to dispatch a non X event
        if Event_Mng.Wait  (0) /= Event_Mng.No_Event then
          Selected := Oldest;
          Clients(Client).Running := True;
          return;
        end if;
      end if;

      -- Try to loop without select on X events (set Got_Id)
      if Next_Event and then Selected /= No_Client_No then
        Got_Id := Clients(Selected).Line_For_C_Id;
      else
        Got_Id := No_Line_For_C;
      end if;

      -- Select on smaller delay
      First_Client := Closest;
      if Debug then
        My_Io.Put_Line ("Dispatch.Wait: selecting " & First_Client'Img);
      end if;

      -- Call the select
      Xx_Select (Clients(First_Client).Wait_Exp, Got_Id, Event, Next_Event);

      -- Dispatch result
      case Event is
        when Keyboard | Tid_Release | Tid_Press | Tid_Motion | Refresh =>
          -- A X event to deliver to proper client
          Selected := Find_From_C (Got_Id);
          Nb_X_Events := Nb_X_Events + 1;
        when Timer_Event | Fd_Event | Signal_Event =>
          -- A general event to deliver to oldest
          Selected := Oldest;
          Nb_X_Events := 0;
        when Wakeup_Event =>
          -- A wake up. Select no client if registration pending
          if Register_Waiting /= 0 then
            Selected := No_Client_No;
            Nb_Waiting := Nb_Waiting + 1;
          else
            Selected := Oldest;
          end if;
          Nb_X_Events := 0;
        when No_Event =>
          -- Tiemout to deliver to closest
          Selected := First_Client;
          Nb_X_Events := 0;
      end case;

      -- One shall be selected or a registration pending
      if Selected /= No_Client_No then
        Clients(Selected).Running := True;
      elsif Event /= Wakeup_Event then
        if Debug then
          My_Io.Put_Line ("Dispatch.Wait: no selected " & Selected'Img);
        end if;
        raise Dispatch_Error;
      end if;
      if Debug then
        My_Io.Put_Line ("Dispatch.Wait: selected " & Selected'Img);
      end if;

    end Wait;

    -- All but one client wait here, eventually getting and event
    --  from select [ process_event ]
    entry Get_Event (for Client in Client_Range) (Kind : out Event_Kind)
          when not In_X and then Client = Selected is
    begin
      Check (Client, True);
      Kind := Event;
    end Get_Event;

  end Dispatcher;

end Dispatch;

