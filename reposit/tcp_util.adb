with Unchecked_Deallocation;
with Ada.Exceptions;
with Environ, Dynamic_List, Timers, Event_Mng, Basic_Proc, Socket_Util;
package body Tcp_Util is

  -- Debugging
  Debug_Init : Boolean := False;
  Debug_Connect_Name   : constant String := "TCP_UTIL_DEBUG_CONNECT";
  Debug_Accept_Name    : constant String := "TCP_UTIL_DEBUG_ACCEPT";
  Debug_Overflow_Name  : constant String := "TCP_UTIL_DEBUG_OVERFLOW";
  Debug_Reception_Name : constant String := "TCP_UTIL_DEBUG_RECEPTION";
  Debug_Connect  : Boolean := False;
  Debug_Accept   : Boolean := False;
  Debug_Overflow : Boolean := False;
  Debug_Reception : Boolean := False;
  procedure Set_Debug (Name : in String; Var : in out Boolean) is
  begin
    Var := Environ.Is_Yes (Name);
  exception
    when others =>
      null;
  end Set_Debug;

  procedure Init_Debug is
  begin
    if Debug_Init then
      return;
    end if;
    Set_Debug (Debug_Connect_Name, Debug_Connect);
    Set_Debug (Debug_Accept_Name, Debug_Accept);
    Set_Debug (Debug_Overflow_Name, Debug_Overflow);
    Set_Debug (Debug_Reception_Name, Debug_Reception);
    Debug_Init := True;
  end Init_Debug;

  -- Connecting connection
  type Connecting_Rec is record
    Protocol : Tcp_Protocol_List;
    Host : Remote_Host;
    Port : Remote_Port;
    Delta_Retry : Duration;
    Nb_Tries : Natural;
    Ttl : Socket.Ttl_Range;
    Cb : Connection_Callback_Access;
    Timer : Timers.Timer_Id;
    Dscr : Socket.Socket_Dscr;
    Fd   : Event_Mng.File_Desc;
    Fd_Set : Boolean;
    Curr_Try : Natural;
  end record;

  package Con_Dyn_List_Mng is new Dynamic_List (Connecting_Rec);
  package Con_List_Mng renames Con_Dyn_List_Mng.Dyn_List;
  Con_List : Con_List_Mng.List_Type;

  -- Search Connecting_Rec by Timer
  function Timer_Match (R1, R2 : Connecting_Rec) return Boolean is
    use type Timers.Timer_Id;
  begin
    return R1.Timer = R2.Timer;
  end Timer_Match;
  function Find_By_Timer is new Con_List_Mng.Search (Timer_Match);

  -- Search Connecting_Rec by Host, Port
  function Dest_Match (R1, R2 : Connecting_Rec) return Boolean is
  begin
    return R1.Host = R2.Host and then R1.Port = R2.Port;
  end Dest_Match;
  function Find_By_Dest is new Con_List_Mng.Search (Dest_Match);

  -- Search Connecting_Rec by Fd
  function Fd_Match (R1, R2 : Connecting_Rec) return Boolean is
    use type Event_Mng.File_Desc;
  begin
    return R1.Fd_Set and then R2.Fd_Set and then R1.Fd = R2.Fd;
  end Fd_Match;
  procedure Find_By_Fd is new Con_List_Mng.Search_Raise (Fd_Match);

  -- Delete current connection rec in list
  procedure Delete_Current_Con is
    Moved : Boolean;
  begin
    Con_List.Delete (Moved => Moved);
  end Delete_Current_Con;

  -- Try to open a socket and connect
  -- Dscr is open and non blocking, or No_Socket if failure
  -- Sets connected if connection established
  procedure Try_Connect (
           Protocol  : in Tcp_Protocol_List;
           Host      : in Remote_Host;
           Port      : in Remote_Port;
           Ttl       : in Socket.Ttl_Range;
           Dscr      : in out Socket.Socket_Dscr;
           Connected : out Boolean) is
    use type  Socket.Socket_Dscr;
  begin

    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Try_Connect start");
    end if;
    -- Open non blocking
    Dscr.Open (Protocol);
    Dscr.Set_Blocking (Socket.Non_Blocking);
    if Ttl /= Default_Ttl then
      Dscr.Set_Ttl (Ttl);
    end if;
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Try_Connect socket open");
    end if;

    -- Connect
    case Host.Kind is
      when Host_Name_Spec =>
        case Port.Kind is
          when Port_Name_Spec =>
            Dscr.Set_Destination_Name_And_Service (False,
                                   Host.Name.Image, Port.Name.Image);
          when Port_Num_Spec =>
            Dscr.Set_Destination_Name_And_Port (False,
                                   Host.Name.Image, Port.Num);
        end case;
      when Host_Id_Spec =>
        case Port.Kind is
          when Port_Name_Spec =>
            Dscr.Set_Destination_Host_And_Service (Host.Id, Port.Name.Image);
          when Port_Num_Spec =>
            Dscr.Set_Destination_Host_And_Port (Host.Id, Port.Num);
        end case;
    end case;
    if Dscr.Is_Connected then
      Connected := True;
    else
      Dscr.Close;
      Connected := False;
    end if;
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Try_Connect result " & Connected'Img);
    end if;
  exception
    when Socket.Soc_Conn_Refused =>
      -- Not open or not connected
      if Dscr.Is_Open then
        Dscr.Close;
      end if;
      Connected := False;
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Try_Connect refused");
      end if;
    when Socket.Soc_Would_Block =>
      Connected := False;
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Try_Connect would block");
      end if;
  end Try_Connect;

  -- Handle a connection success/failure according to Rec.Dscr (open or not)
  -- Cancel timer
  -- Delete connection rec
  -- Get dest if success
  -- Call callback
  procedure Handle_Current_Result (Rec : in out Connecting_Rec) is
    Port : Port_Num;
    Host : Host_Id;
    use type Socket.Socket_Dscr, Timers.Timer_Status;
  begin
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Handle_Current_Result start");
    end if;
    -- Remove management data
    if Rec.Timer.Status /= Timers.Deleted then
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Handle_Current_Result delete timer");
      end if;
      Timers.Delete (Rec.Timer);
    end if;
    Delete_Current_Con;
    if Rec.Dscr.Is_Open then
      -- Connected
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Handle_Current_Result connected");
      end if;
      Host := Rec.Dscr.Get_Destination_Host;
      Port := Rec.Dscr.Get_Destination_Port;
      Rec.Dscr.Set_Blocking (Socket.Blocking_Send);
    else
      -- Giving up
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Handle_Current_Result giving up");
      end if;
      if Rec.Port.Kind = Port_Name_Spec then
        begin
          -- Services may have changed since Connect_To checks
          Port := Socket.Port_Num_Of (Rec.Port.Name.Image, Rec.Protocol);
        exception
          when others =>
            Port := 0;
        end;
      else
        Port := Rec.Port.Num;
      end if;
      if Rec.Host.Kind = Host_Name_Spec then
        begin
          -- Hosts may have changed since Connect_To checks
          Host := Socket.Host_Id_Of (Rec.Host.Name.Image);
        exception
          when others =>
            Host := Socket.No_Host;
        end;
      else
        Host := Rec.Host.Id;
      end if;
    end if;
    -- Inform client
    if Rec.Cb /= null then
      Rec.Cb (Host, Port, Rec.Dscr.Is_Open, Rec.Dscr);
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Handle_Current_Result Cb called");
      end if;
    end if;
  end Handle_Current_Result;

  -- End a successfull or pending or failed async connect
  --  according to Success
  -- Remove Fd callback
  -- If failure: Check tries
  -- Handle global failure if no more try
  procedure End_Async_Connect (Success : in Boolean;
                               Rec : in out Connecting_Rec;
                               Go_On : out Boolean) is
  begin
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.End_Async_Connect start");
    end if;
    -- Remove Fd callbacks. Thery are active at this time
    --  either because called by Fd_Cb or by Timer_Cb with open connection
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.End_Async_Connect del Cbs on fd "
                    & Rec.Fd'Img);
    end if;
    Event_Mng.Del_Fd_Callback (Rec.Fd, True);
    Event_Mng.Del_Fd_Callback (Rec.Fd, False);

    -- Close if failure and still open
    if not Success and then Rec.Dscr.Is_Open then
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.End_Async_Connect closing socket");
      end if;
      Rec.Dscr.Close;
      Rec.Fd_Set := False;
    end if;

    -- Note that if success but Dscr not open, which would be a bug,
    --  is silently handled here as a failure

    -- Success or failure
    if Rec.Dscr.Is_Open then
      -- Connection success
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.End_Async_Connect success");
      end if;
      Handle_Current_Result (Rec);
      Go_On := False;
    else
      -- This try failure
      -- Give up if no more try
      if Rec.Curr_Try = Rec.Nb_Tries then
        if Debug_Connect then
          Basic_Proc.Put_Line_Output ("  Tcp_Util.End_Async_Connect last failure");
        end if;
        Handle_Current_Result (Rec);
        Go_On := False;
      else
        if Debug_Connect then
          Basic_Proc.Put_Line_Output ("  Tcp_Util.End_Async_Connect this failure");
        end if;
        Go_On := True;
      end if;
    end if;
  end End_Async_Connect;

  -- Callback on connect fd
  function Connection_Fd_Cb (Fd : in Event_Mng.File_Desc;
                             Read : in Boolean) return Boolean is
    Rec : Connecting_Rec;
    Go_On : Boolean;
    use type Socket.Socket_Dscr;
  begin
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Fd_Cb start with fd " & Fd'Img
                    & "  read " & Read'Img);
    end if;
    -- Find record by fd
    Rec.Fd := Fd;
    Rec.Fd_Set := True;
    Find_By_Fd (Con_List, Rec, From => Con_List_Mng.Absolute);

    Con_List.Read (Rec, Con_List_Mng.Current);
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Fd_Cb found rec "
                    & Positive'Image (Con_List.Get_Position));
    end if;

    -- This try result?
    if not Rec.Dscr.Is_Connected then
      -- Cancel and close
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Fd_Cb not connected");
      end if;
      End_Async_Connect (False, Rec, Go_On);
    else
      -- Success
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Fd_Cb connected");
      end if;
      End_Async_Connect (True, Rec, Go_On);
    end if;
    if Go_On then
      -- Store closed Dscr for timer callback
      Con_List.Modify (Rec, Con_List_Mng.Current);
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Fd_Cb update rec "
                      & Positive'Image (Con_List.Get_Position));
      end if;
    end if;
    -- Propagate event if no Go_On
    return not Go_On;
  exception
    when Con_List_Mng.Not_In_List =>
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Fd_Cb fd rec not found");
      end if;
      raise;
  end Connection_Fd_Cb;

  -- Timer callback
  function Connection_Timer_Cb (Id : Timers.Timer_Id;
                                Data : Timers.Timer_Data := Timers.No_Data)
           return Boolean is
    pragma Unreferenced (Data);
    Rec : Connecting_Rec;
    Connected : Boolean;
    Go_On : Boolean;
    use type Socket.Socket_Dscr, Timers.Timer_Id;
  begin
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Timer_Cb start on timer");
    end if;
    -- Read rec: Try current
    Con_List.Read (Rec, Con_List_Mng.Current);
    if Rec.Timer /= Id then
      -- No good. Locate it
      Rec.Timer := Id;
      Go_On := Find_By_Timer (Con_List, Rec, From => Con_List_Mng.Absolute);
      if not Go_On then
        if Debug_Connect then
          Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Timer_Cb timer rec not found");
        end if;
        return False;
      end if;
      Con_List.Read (Rec, Con_List_Mng.Current);
    end if;
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Timer_Cb found rec "
                    & Positive'Image (Con_List.Get_Position));
    end if;

    -- Either first try, or previous failed (sync or not)
    --  or async connect is pending
    -- Rec.Curr_Try is current try number

    -- Cancel pending async connect
    if Rec.Dscr.Is_Open then
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Timer_Cb is open");
      end if;
      End_Async_Connect (False, Rec, Go_On);
      if not Go_On then
        return True;
      end if;
    end if;

    -- Try to connect
    Try_Connect (Rec.Protocol, Rec.Host, Rec.Port, Rec.Ttl, Rec.Dscr,
                 Connected);
    Rec.Curr_Try :=  Rec.Curr_Try + 1;

    if Rec.Dscr.Is_Open and then Connected then
      -- Connected synchronous success
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Timer_Cb synchronous success");
      end if;
      Handle_Current_Result (Rec);
      return True;
    elsif not Rec.Dscr.Is_Open then
      -- Connect synchronous failure: Check number of tries
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Timer_Cb synchronous failure");
      end if;
      if Rec.Curr_Try = Rec.Nb_Tries then
        if Debug_Connect then
          Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Timer_Cb last failure");
        end if;
        Handle_Current_Result (Rec);
        return True;
      end if;
    end if;

    -- Asynchronous pending
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Timer_Cb pending");
    end if;
    if Rec.Dscr.Is_Open then
      -- Connection pending
      -- Save Dscr, Fd and pending status
      Rec.Fd := Rec.Dscr.Get_Fd;
      Rec.Fd_Set := True;
      -- Add callback on fd
      Event_Mng.Add_Fd_Callback (Rec.Fd, True, Connection_Fd_Cb'Access);
      Event_Mng.Add_Fd_Callback (Rec.Fd, False, Connection_Fd_Cb'Access);
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Timer_Cb asynchronous pending"
                        & " on fd " & Rec.Fd'Img);
      end if;
    end if;

    -- Synchronous failure or pending: arm timer at first try
    if Rec.Curr_Try = 1 then
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Timer_Cb first try");
      end if;
      -- First attempt failure: start timer
      Rec.Timer.Create (
          Delay_Spec => (Delay_Kind    => Timers.Delay_Sec,
                         Clock         => null,
                         Period        => Rec.Delta_Retry,
                         Delay_Seconds => Rec.Delta_Retry),
          Callback => Connection_Timer_Cb'Access);
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Timer_Cb created timer");
      end if;
    end if;
    -- Store Rec: Fd, Timer_Id, Curr_Try ...
    Con_List.Modify (Rec, Con_List_Mng.Current);
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Connection_Timer_Cb update rec "
                    & Positive'Image (Con_List.Get_Position));
    end if;
    return False;
  end Connection_Timer_Cb;

  -- Connect to a remote Host/Port
  -- May make several tries (one each Delta_Retry) before giving up
  -- Return True if synchronous result
  function Connect_To (Protocol      : in Tcp_Protocol_List;
                       Host          : in Remote_Host;
                       Port          : in Remote_Port;
                       Connection_Cb : in Connection_Callback_Access;
                       Delta_Retry   : in Positive_Duration := 1.0;
                       Nb_Tries      : in Natural := 1;
                       Ttl           : in Socket.Ttl_Range := Default_Ttl)
           return Boolean is
    Rec : Connecting_Rec;
  begin
    Init_Debug;
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Connect_To start");
    end if;
    -- Check port and host name
    if Port.Kind = Port_Name_Spec then
      declare
        Num : Port_Num;
        pragma Unreferenced (Num);
      begin
        Num := Socket.Port_Num_Of (Port.Name.Image, Protocol);
      exception
        when others =>
          raise Name_Error;
      end;
    end if;
    if Host.Kind = Host_Name_Spec then
      declare
        Id : Host_Id;
        pragma Unreferenced (Id);
      begin
        Id := Socket.Host_Id_Of (Host.Name.Image);
      exception
        when others =>
          raise Name_Error;
      end;
    end if;
    -- Initialise record and insert it in list
    Rec.Protocol := Protocol;
    Rec.Host := Host;
    Rec.Port := Port;
    Rec.Delta_Retry := Delta_Retry;
    Rec.Nb_Tries := Nb_Tries;
    Rec.Ttl := Ttl;
    Rec.Cb := Connection_Cb;
    Rec.Timer := Timers.No_Timer;
    Rec.Dscr := Socket.No_Socket;
    Rec.Curr_Try := 0;
    Rec.Fd_Set := False;
    Con_List.Insert (Rec);

    -- Try to connect: call timer callback
    -- Our Rec should be the only one with No_Timer
    return Connection_Timer_Cb (Timers.No_Timer);

  end Connect_To;

  -- Abort a pending connection
  -- May raise No_Such
  procedure Abort_Connect (Host : in Remote_Host;
                           Port : in Remote_Port) is
    Moved : Boolean;
    Rec : Connecting_Rec;
  begin
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Abort_Connect start");
    end if;
    -- Find rec
    Rec.Host := Host;
    Rec.Port := Port;
    if not Find_By_Dest (Con_List, Rec, From => Con_List_Mng.Absolute) then
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Abort_Connect rec not found");
      end if;
      raise No_Such;
    end if;
    Con_List.Read (Rec, Con_List_Mng.Current);
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Abort_Connect found rec "
                    & Positive'Image (Con_List.Get_Position));
    end if;
    if Rec.Fd_Set then
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Abort_Connect del Cbs on fd "
                      & Rec.Fd'Img);
      end if;
      Event_Mng.Del_Fd_Callback (Rec.Fd, True);
      Event_Mng.Del_Fd_Callback (Rec.Fd, False);
      Rec.Dscr.Close;
    end if;
    -- Cancel timer
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Abort_Connect deleting timer");
    end if;
    Rec.Timer.Delete;
    -- Delete rec
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Abort_Connect deleting rec");
    end if;
    Con_List.Delete (Moved => Moved);
  end Abort_Connect;

  -- Synchronously connect to a remote Host/Port
  -- The Ttl is used (if supported by the TCP stack) to establish the
  --  connection and in the established connection
  -- Timeout = 0.0 may be used for infinite attempt
  -- Returns a valid (Open) Dscr if the connection has been established
  -- May raise Name_Error if Host.Name or Port.Name is unknown
  function Connect_To (Protocol      : in Tcp_Protocol_List;
                       Host          : in Remote_Host;
                       Port          : in Remote_Port;
                       Timeout       : in Natural_Duration := 1.0;
                       Ttl           : in Socket.Ttl_Range := Default_Ttl)
           return Socket.Socket_Dscr is
    Dscr : Socket.Socket_Dscr;
  begin
    -- Open blocking
    Dscr.Open (Protocol);
    if Ttl /= Default_Ttl then
      Dscr.Set_Ttl (Ttl);
    end if;
    if Debug_Connect then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Connect_To sync start");
    end if;
    if Timeout = 0.0 then
      -- Synchronous connect infinite
      Socket_Util.Set_Destination (Dscr, False, Host, Port);
    else
      -- Synchronous connect with timeout
      select
        delay Timeout;
          -- Timeout on connect
          if Debug_Connect then
            Basic_Proc.Put_Line_Output ("  Tcp_Util.Connect_To sync timeout");
          end if;
          Dscr.Close;
        then abort
          Socket_Util.Set_Destination (Dscr, False, Host, Port);
      end select;
    end if;
    return Dscr;
  exception
    when Socket.Soc_Conn_Refused =>
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Connect_To sync refused");
      end if;
      Dscr.Close;
      return Dscr;
    when Socket.Soc_Name_Not_Found =>
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Connect_To sync name error");
      end if;
      Dscr.Close;
      raise Name_Error;
    when Error:others =>
      if Dscr.Is_Open then
        Dscr.Close;
      end if;
      if Debug_Connect then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Connect_To sync exception "
              & Ada.Exceptions.Exception_Name (Error));
      end if;
      raise;
  end Connect_To;

  --------------------------------------------------------------------------

  -- Accepting connection
  type Accepting_Rec is record
    Protocol : Tcp_Protocol_List;
    Port     : Port_Num;
    Cb       : Acception_Callback_Access;
    Dscr     : Socket.Socket_Dscr;
    Fd       : Event_Mng.File_Desc;
  end record;
  package Acc_Dyn_List_Mng is new Dynamic_List (Accepting_Rec);
  package Acc_List_Mng renames Acc_Dyn_List_Mng.Dyn_List;
  Acc_List : Acc_List_Mng.List_Type;

  -- Search Accepting_Rec by Fd
  function Fd_Match (R1, R2 : Accepting_Rec) return Boolean is
    use type Event_Mng.File_Desc;
  begin
    return R1.Fd = R2.Fd;
  end Fd_Match;
  procedure Find_By_Fd is new Acc_List_Mng.Search_Raise (Fd_Match);

  --  Search Accepting_Rec by Port_Num
  function Port_Match (R1, R2 : Accepting_Rec) return Boolean is
    use type Socket.Protocol_List, Socket.Port_Num;
  begin
    return R1.Protocol = R2.Protocol and then R1.Port = R2.Port;
  end Port_Match;
  function Find_By_Port is new Acc_List_Mng.Search (Port_Match);

  -- Callback on accept fd
  function Acception_Fd_Cb (Fd : in Event_Mng.File_Desc;
                            Read : in Boolean) return Boolean is
    Rec : Accepting_Rec;
    New_Dscr : Socket.Socket_Dscr;
    Port : Port_Num;
    Host : Host_Id;
  begin
    if Debug_Accept then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Acception_Fd_Cb start with fd " & Fd'Img
                    & "  read " & Read'Img);
    end if;
    -- Find record by fd
    Rec.Fd := Fd;
    Find_By_Fd (Acc_List, Rec, From => Acc_List_Mng.Absolute);
    Acc_List.Read (Rec, Acc_List_Mng.Current);
    if Debug_Accept then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Acception_Fd_Cb found rec "
                    & Positive'Image (Acc_List.Get_Position));
    end if;

    -- Accept
    begin
      Rec.Dscr.Accept_Connection (New_Dscr);
    exception
      when Socket.Soc_Would_Block =>
        -- Connection is not valid any more: discard
        if Debug_Accept then
          Basic_Proc.Put_Line_Output ("  Tcp_Util.Acception_Fd_Cb accept would block");
        end if;
        return False;
    end;
    if Debug_Accept then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Acception_Fd_Cb connection accepted");
    end if;
    -- Set new socket in mode Blocking_Send
    New_Dscr.Set_Blocking (Socket.Blocking_Send);

    -- Call callback
    Host := New_Dscr.Get_Destination_Host;
    Port := New_Dscr.Get_Destination_Port;
    if Rec.Cb /= null then
      Rec.Cb (Rec.Port, Rec.Dscr, Host, Port, New_Dscr);
      if Debug_Accept then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Acception_Fd_Cb Cb called");
      end if;
    end if;
    return True;
  exception
    when Acc_List_Mng.Not_In_List =>
      if Debug_Accept then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Acception_Fd_Cb fd rec not found");
      end if;
      raise;
  end Acception_Fd_Cb;

  -- Accept connections to a local port
  -- Dscr is set to the socket accepting connections
  procedure Accept_From (Protocol     : in Tcp_Protocol_List;
                         Port         : in Local_Port;
                         Acception_Cb : in Acception_Callback_Access;
                         Dscr         : out Socket.Socket_Dscr;
                         Num          : out Port_Num) is
    Rec : Accepting_Rec;
  begin
    Init_Debug;
    if Debug_Accept then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Accept_From start");
    end if;
    -- Initialise Rec
    Rec.Protocol := Protocol;
    Rec.Port := 0;
    Rec.Cb := Acception_Cb;
    Rec.Dscr := Socket.No_Socket;

    -- Open socket in mode Blocking_Send
    Dscr.Open (Protocol);
    Dscr.Set_Blocking (Socket.Blocking_Send);
    Rec.Dscr := Dscr;
    Rec.Fd := Rec.Dscr.Get_Fd;
    -- Bind socket
    case Port.Kind is
      when Port_Name_Spec =>
        begin
          Rec.Dscr.Link_Service (Port.Name.Image);
        exception
          when Socket.Soc_Name_Not_Found =>
            raise Name_Error;
        end;
      when Port_Num_Spec =>
        Rec.Dscr.Link_Port (Port.Num);
      when Port_Dynamic_Spec =>
        Rec.Dscr.Link_Dynamic;
    end case;
    Rec.Port := Rec.Dscr.Get_Linked_To;
    Num := Rec.Port;
    if Debug_Accept then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Accept_From linked");
    end if;

    -- Add callback on fd
    Event_Mng.Add_Fd_Callback (Rec.Fd, True, Acception_Fd_Cb'Access);

    -- Store Rec
    Acc_List.Insert (Rec);
    if Debug_Accept then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Accept_From insert rec "
                    & Positive'Image (Acc_List.Get_Position));
    end if;

  exception
    when others =>
      -- Not open or not bind
      if Dscr.Is_Open then
        Dscr.Close;
      end if;
      raise;
  end Accept_From;

  -- Abort further accepts on port
  -- May raise No_Such
  procedure Abort_Accept (Protocol : in Tcp_Protocol_List; Num : in Port_Num) is
    Rec : Accepting_Rec;
    Moved : Boolean;
  begin
    if Debug_Accept then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Abort_Accept start");
    end if;
    -- Find rec and read
    Rec.Protocol := Protocol;
    Rec.Port := Num;
    if not Find_By_Port (Acc_List, Rec, From => Acc_List_Mng.Absolute) then
      if Debug_Accept then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Abort_Accept rec not found");
      end if;
      raise No_Such;
    end if;
    Acc_List.Read (Rec, Acc_List_Mng.Current);
    if Debug_Accept then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Abort_Accept found rec "
                    & Positive'Image (Acc_List.Get_Position));
    end if;
    -- Del callback, close and delete rec
    Event_Mng.Del_Fd_Callback (Rec.Fd, True);
    Rec.Dscr.Close;
    Acc_List.Delete (Moved => Moved);
    if Debug_Accept then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Abort_Accept socket closed and rec deleted");
    end if;
  end Abort_Accept;

  --------------------------------------------------------------------------

  -- Sending
  type Sending_Rec is record
    Dscr : Socket.Socket_Dscr;
    Fd   : Event_Mng.File_Desc;
    Timer : Timers.Timer_Id;
    Eoo_Cb : End_Overflow_Callback_Access;
    Err_Cb : Send_Error_Callack_Access;
  end record;

  package Sen_Dyn_List_Mng is new Dynamic_List (Sending_Rec);
  package Sen_List_Mng renames Sen_Dyn_List_Mng.Dyn_List;
  Sen_List : Sen_List_Mng.List_Type;

  -- Search Sending_Rec by Dscr
  function Dscr_Match (R1, R2 : Sending_Rec) return Boolean is
    use type Socket.Socket_Dscr;
  begin
    return R1.Dscr = R2.Dscr;
  end Dscr_Match;
  function Find_By_Dscr is new Sen_List_Mng.Search (Dscr_Match);

  -- Search Sending_Rec by Fd
  function Fd_Match (R1, R2 : Sending_Rec) return Boolean is
    use type Event_Mng.File_Desc;
  begin
    return R1.Fd = R2.Fd;
  end Fd_Match;
  procedure Find_By_Fd is new Sen_List_Mng.Search_Raise (Fd_Match);

  -- Search Sending_Rec by Timer Id
  function Timer_Match (R1, R2 : Sending_Rec) return Boolean is
    use type Timers.Timer_Id;
  begin
    return R1.Timer = R2.Timer;
  end Timer_Match;
  procedure Find_By_Timer is new Sen_List_Mng.Search_Raise (Timer_Match);

  -- Delete current sending rec in list
  procedure Delete_Current_Sen is
    Moved : Boolean;
  begin
    Sen_List.Delete (Moved => Moved);
  end Delete_Current_Sen;

  -- Timer expiration callback
  function Timer_Cb (Id : in Timers.Timer_Id;
                     Data : in Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Data);
    Rec : Sending_Rec;
  begin
    -- Find Rec from Timer and read
    Rec.Timer := Id;
    Find_By_Timer (Sen_List, Rec, From => Sen_List_Mng.Absolute);
    Sen_List.Read (Rec, Sen_List_Mng.Current);
    if Debug_Overflow then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Timer_Cb found rec "
                    & Positive'Image (Sen_List.Get_Position));
    end if;

    -- End of processing: unhook callback and del rec
    Event_Mng.Del_Fd_Callback (Rec.Fd, False);
    Delete_Current_Sen;
    if Debug_Overflow then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Timer_Cb cleaned");
    end if;

    -- Call send error callback and close
    if Rec.Err_Cb /= null then
      Rec.Err_Cb (Rec.Dscr, False);
      if Debug_Overflow then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Send_Err_Cb Cb called");
      end if;
    end if;
    Rec.Dscr.Close;
    return True;
  end Timer_Cb;

  -- Sending callback on fd
  function Sending_Cb (Fd : in Event_Mng.File_Desc; Read : in Boolean)
                      return Boolean is
    Rec : Sending_Rec;
    type Send_Result_List is (Ok, Timeout, Overflow, Conn_Lost);
    Result : Send_Result_List;
  begin
    if Debug_Overflow then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Sending_Cb start with fd " & Fd'Img
                    & "  read " & Read'Img);
    end if;
    -- Find Rec from Fd and read
    Rec.Fd := Fd;
    Find_By_Fd (Sen_List, Rec, From => Sen_List_Mng.Absolute);
    Sen_List.Read (Rec, Sen_List_Mng.Current);
    if Debug_Overflow then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Sending_Cb found rec "
                    & Positive'Image (Sen_List.Get_Position));
    end if;

    -- See if socket has been changed to blocking meanwhile
    if Rec.Dscr.Is_Blocking (Emission => True) then
      Result := Timeout;
    else
      -- Try to re send
      begin
        Rec.Dscr.Re_Send;
        Result := Ok;
      exception
        when Socket.Soc_Would_Block =>
          -- Still in overflow
          if Debug_Overflow then
            Basic_Proc.Put_Line_Output ("  Tcp_Util.Sending_Cb still in overflow");
          end if;
          return False;
        when Socket.Soc_Conn_Lost =>
          if Debug_Overflow then
            Basic_Proc.Put_Line_Output ("  Tcp_Util.Sending_Cb still in overflow");
          end if;
          Result := Conn_Lost;
      end;
    end if;

    -- End of overflow: cancel timer, unhook callback and del rec
    if Rec.Timer.Exists then
      Rec.Timer.Delete;
    end if;
    Event_Mng.Del_Fd_Callback (Rec.Fd, False);
    Delete_Current_Sen;
    if Debug_Overflow then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Sending_Cb cleaned");
    end if;

    if Result = Ok then
      -- Success: Call end of overflow callbak
      if Debug_Overflow then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Sending_Cb resent OK");
      end if;
      if Rec.Eoo_Cb /= null then
        Rec.Eoo_Cb (Rec.Dscr);
        if Debug_Overflow then
          Basic_Proc.Put_Line_Output ("  Tcp_Util.Sending_Cb Cb called");
        end if;
      end if;
      return True;
    else
      -- Timeout or Lost_Conn: Call send error callback and close
      if Debug_Overflow then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Sending_Cb error " & Result'Img);
      end if;
      if Rec.Err_Cb /= null then
        Rec.Err_Cb (Rec.Dscr, Result = Conn_Lost);
        if Debug_Overflow then
          Basic_Proc.Put_Line_Output ("  Tcp_Util.Send_Err_Cb Cb called");
        end if;
      end if;
      Rec.Dscr.Close;
      return True;
    end if;
  exception
    when Sen_List_Mng.Not_In_List =>
      if Debug_Overflow then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Sending_Cb fd rec not found");
      end if;
      raise;
    when others =>
      return False;
  end Sending_Cb;

  -- Send message, handling overflow
  function Send (Dscr               : in Socket.Socket_Dscr;
                 End_Of_Overflow_Cb : in End_Overflow_Callback_Access;
                 Send_Error_Cb      : in Send_Error_Callack_Access;
                 Timeout            : in Natural_Duration;
                 Message            : in Message_Type;
                 Length             : in Natural := 0) return Boolean is
    Timer_Delay : Timers.Delay_Rec;
    Rec : Sending_Rec;
    procedure Send is new Socket.Send (Message_Type);
  begin
    Init_Debug;

    -- Try to send
    begin
      if Timeout = 0.0 or else not Dscr.Is_Blocking (Emission => True) then
        -- Inifinite or non blocking sending
        Send (Dscr, Message, Length);
      else
        -- (Re)send during at most Timeout
        select
          delay Timeout;
            -- Timeout on sending
            raise Timeout_Error;
          then abort
            Send (Dscr, Message, Length);
        end select;
      end if;
      return True;
    exception
      when Socket.Soc_Would_Block =>
        -- Overflow (on non blocking) is handled below
        null;
      when Socket.Soc_Conn_Lost | Timeout_Error =>
        raise;
      when Error:others =>
        if Debug_Overflow then
          Basic_Proc.Put_Line_Output ("  Tcp_Util.Send exception "
              & Ada.Exceptions.Exception_Name (Error));
        end if;
        raise;
    end;

    -- Handle overflow : arm timer, build and store rec
    if Debug_Overflow then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Send oveflow");
    end if;
    Rec.Dscr := Dscr;
    Rec.Fd := Dscr.Get_Fd;
    if Timeout /= 0.0 then
      Timer_Delay.Delay_Seconds := Timeout;
      Rec.Timer.Create (Timer_Delay, Timer_Cb'Access);
    end if;
    Rec.Eoo_Cb := End_Of_Overflow_Cb;
    Rec.Err_Cb := Send_Error_Cb;
    Sen_List.Insert (Rec);
    if Debug_Overflow then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Send rec with fd " & Rec.Fd'Img
                    & " inserted at "
                    & Positive'Image (Sen_List.Get_Position));
    end if;

    -- Hook our callback in write
    Event_Mng.Add_Fd_Callback (Rec.Fd, False, Sending_Cb'Access);
    if Debug_Overflow then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Send Cb hooked");
    end if;
    return False;
  end Send;

  -- Cancel overflow management and closes
  procedure Abort_Send_And_Close (Dscr : in out Socket.Socket_Dscr) is
    Rec : Sending_Rec;
  begin
    if Debug_Overflow then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Abort_Send_and_Close start");
    end if;
    -- Find Rec from Dscr and read
    Rec.Dscr := Dscr;
    if not Find_By_Dscr (Sen_List, Rec, From => Sen_List_Mng.Absolute) then
      if Debug_Overflow then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Abort_Send_and_Close rec not found");
      end if;
      raise No_Such;
    end if;
    Sen_List.Read (Rec, Sen_List_Mng.Current);
    if Debug_Overflow then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Abort_Send_and_Close found rec "
                    & Positive'Image (Sen_List.Get_Position));
    end if;

    -- Cancel timer, unhook callback and del rec
    if Rec.Timer.Exists then
      Rec.Timer.Delete;
    end if;
    Event_Mng.Del_Fd_Callback (Rec.Fd, False);
    Delete_Current_Sen;

    -- Close
    Dscr.Close;
    if Debug_Overflow then
      Basic_Proc.Put_Line_Output ("  Tcp_Util.Abort_Send_and_Close done");
    end if;
  end Abort_Send_And_Close;

  --------------------------------------------------------------------------


  package body Reception is

    -- List of Dscrs
    type Rece_Rec is record
      Dscr : Socket.Socket_Dscr;
      Fd   : Event_Mng.File_Desc;
      Read_Cb : Reception_Callback_Access := null;
      Discon_Cb : Disconnection_Callback_Access := null;
    end record;
    package Rece_Dyn_List_Mng is new Dynamic_List (Rece_Rec);
    package Rece_List_Mng renames Rece_Dyn_List_Mng.Dyn_List;
    Rece_List : Rece_List_Mng.List_Type;

    type Message_Access is access Message_Type;
    procedure Free_Message is new Unchecked_Deallocation
      (Message_Type, Message_Access);

    function Dscr_Match (R1, R2 : Rece_Rec) return Boolean is
      use type Socket.Socket_Dscr;
    begin
      return R1.Dscr = R2.Dscr;
    end Dscr_Match;
    function Find_Dscr is new Rece_List_Mng.Search (Dscr_Match);

    function Fd_Match (R1, R2 : Rece_Rec) return Boolean is
      use type Event_Mng.File_Desc;
    begin
      return R1.Fd = R2.Fd;
    end Fd_Match;
    function Find_Fd is new Rece_List_Mng.Search (Fd_Match);

    -- The one to use with Socket
    procedure Read is new Socket.Receive (Message_Type);

    -- Unhook and close a Dscr. Call appli Cb
    procedure Close_Current is
      Rec : Rece_Rec;
      Moved : Boolean;
    begin
      -- Get from list
      Rece_List.Get (Rec, Moved => Moved);
      -- Call appli disconnection Cb
      if Rec.Discon_Cb /= null then
        Rec.Discon_Cb (Rec.Dscr);
      end if;
      -- Unhook and close
      Event_Mng.Del_Fd_Callback (Rec.Fd, True);
      begin
        Abort_Send_And_Close (Rec.Dscr);
      exception
        when No_Such =>
          Rec.Dscr.Close;
      end;
    end Close_Current;

    -- The Cb hooked on Fd
    function Read_Cb (Fd : Event_Mng.File_Desc; For_Read : in Boolean)
                     return Boolean is
      use type Event_Mng.File_Desc;
      The_Rec : Rece_Rec;
      Msg : Message_Access;
      Len : Natural;
      Result : Boolean;
    begin
      if not For_Read then
        if Debug_Reception then
          Basic_Proc.Put_Line_Output ("  Tcp_Util.Read_Cb on writting fd " & Fd'Img);
        end if;
        return False;
      end if;
      -- Find dscr from Fd
      The_Rec.Fd := Fd;
      if not Find_Fd (Rece_List, The_Rec, From => Rece_List_Mng.Absolute) then
        if Debug_Reception then
          Basic_Proc.Put_Line_Output ("  Tcp_Util.Read_Cb no Dscr for Fd " & Fd'Img);
        end if;
        Event_Mng.Del_Fd_Callback (Fd, True);
        return False;
      end if;
      Rece_List.Read (The_Rec, Rece_List_Mng.Current);

      -- Allocate message
      Msg := new Message_Type;
      -- Try to read
      begin
        Read (The_Rec.Dscr, Msg.all, Len);
      exception
        when Socket.Soc_Would_Block =>
          -- Data is not completely ready
          if Debug_Reception then
            Basic_Proc.Put_Line_Output ("  Tcp_Util.Read_Cb soc would block on fd " & Fd'Img);
          end if;
          return False;
        when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
          -- Remote has diconnected
          if Debug_Reception then
            Basic_Proc.Put_Line_Output ("  Tcp_Util.Read_Cb disconnection on fd " & Fd'Img);
          end if;
          -- Notify and close
          Close_Current;
          Free_Message (Msg);
          return True;
        when Socket.Soc_Len_Err =>
          -- Invalid length
          if Debug_Reception then
            Basic_Proc.Put_Line_Output ("  Tcp_Util.Read_Cb invalid length on fd " & Fd'Img);
          end if;
          Close_Current;
          Free_Message (Msg);
          return True;
      end;
      if Debug_Reception then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Read_Cb len " & Len'Img & " on fd " & Fd'Img);
      end if;
      -- Call appli callback
      if The_Rec.Read_Cb /= null then
        Result := The_Rec.Read_Cb (The_Rec.Dscr, Msg.all, Len);
      end if;
      Free_Message (Msg);
      return Result;
    end Read_Cb;

    procedure Set_Callbacks (
                Dscr             : in Socket.Socket_Dscr;
                Reception_Cb     : in Reception_Callback_Access;
                Disconnection_Cb : in Disconnection_Callback_Access) is
      The_Rec : Rece_Rec;
    begin
      Init_Debug;
      if not Dscr.Is_Open then
        raise No_Such;
      end if;

      The_Rec.Dscr := Dscr;
      The_Rec.Fd := Dscr.Get_Fd;
      The_Rec.Read_Cb := Reception_Cb;
      The_Rec.Discon_Cb := Disconnection_Cb;

      -- Append to list
      Rece_List.Insert (The_Rec);

      Event_Mng.Add_Fd_Callback (Dscr.Get_Fd, True,
                 Read_Cb'Unrestricted_Access);

      if Debug_Reception then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Set_Callbacks on Fd " & The_Rec.Fd'Img);
      end if;
    end Set_Callbacks;

    procedure Activate_Callbacks (Dscr   : in Socket.Socket_Dscr;
                                  Active : in Boolean) is
      The_Rec : Rece_Rec;
    begin
      -- Check Dscr is known
      The_Rec.Dscr := Dscr;
      if not Find_Dscr (Rece_List, The_Rec, From => Rece_List_Mng.Absolute) then
        if Debug_Reception then
          Basic_Proc.Put_Line_Output ("  Tcp_Util.Activate_Callbacks Dscr not found");
        end if;
        raise No_Such;
      end if;

      if Active then
        Event_Mng.Add_Fd_Callback (Dscr.Get_Fd, True,
                   Read_Cb'Unrestricted_Access);
      else
        Event_Mng.Del_Fd_Callback (Dscr.Get_Fd, True);
      end if;
    end Activate_Callbacks;

    function Callbacks_Active (Dscr : Socket.Socket_Dscr) return Boolean is
      The_Rec : Rece_Rec;
    begin
      -- Check Dscr is known
      The_Rec.Dscr := Dscr;
      if not Find_Dscr (Rece_List, The_Rec,
                        From => Rece_List_Mng.Absolute) then
        if Debug_Reception then
          Basic_Proc.Put_Line_Output ("  Tcp_Util.Callbacks_Active Dscr not found");
        end if;
        raise No_Such;
      end if;

      return Event_Mng.Fd_Callback_Set (Dscr.Get_Fd, True);
    end Callbacks_Active;

    procedure Remove_Callbacks (Dscr : in Socket.Socket_Dscr) is
      The_Rec : Rece_Rec;
      Moved : Boolean;
    begin
      The_Rec.Dscr := Dscr;
      if not Find_Dscr (Rece_List, The_Rec,
                        From => Rece_List_Mng.Absolute) then
        if Debug_Reception then
          Basic_Proc.Put_Line_Output ("  Tcp_Util.Remove_Callbacks Dscr not found");
        end if;
        raise No_Such;
      end if;

      -- Get from list
      Rece_List.Get (The_Rec, Moved => Moved);
      if Debug_Reception then
        Basic_Proc.Put_Line_Output ("  Tcp_Util.Remove_Callbacks on Fd " & The_Rec.Fd'Img);
      end if;
      Event_Mng.Del_Fd_Callback (Dscr.Get_Fd, True);
    end Remove_Callbacks;

    -- Are callbacks set
    function Callbacks_Set (Dscr : Socket.Socket_Dscr) return Boolean is
      The_Rec : Rece_Rec;
    begin
      The_Rec.Dscr := Dscr;
      return Find_Dscr (Rece_List, The_Rec, From => Rece_List_Mng.Absolute);
    end Callbacks_Set;

  end Reception;

end Tcp_Util;

