with Ada.Exceptions;
with Environ, Dynamic_List, Timers, Event_Mng, My_Io;
package body Tcp_Util is

  -- Remove tailing spaces
  function Parse (Str : String) return String is
  begin
    for I in reverse Str'Range loop
      if Str(I) /= ' ' then
        return Str (Str'First .. I);
      end if;
    end loop;
    return Str;
  end Parse;
    
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
    Cb : Connection_Callback_Access;
    Timer : Timers.Timer_Id;
    Dscr : Socket.Socket_Dscr;
    Fd   : Event_Mng.File_Desc;
    Fd_Set : Boolean;
    Curr_Try : Natural;
  end record;

  package Con_List_Mng is new Dynamic_List (Connecting_Rec);
  Con_List : Con_List_Mng.List_Type;

  -- Search Connecting_Rec by Timer
  function Timer_Match (R1, R2 : Connecting_Rec) return Boolean is
    use type Timers.Timer_Id;
  begin
    return R1.Timer = R2.Timer;
  end Timer_Match;
  procedure Find_By_Timer is new Con_List_Mng.Search (Timer_Match);

  -- Search Connecting_Rec by Host, Port
  function Dest_Match (R1, R2 : Connecting_Rec) return Boolean is
  begin
    return R1.Host = R2.Host and then R1.Port = R2.Port;
  end Dest_Match;
  procedure Find_By_Dest is new Con_List_Mng.Search (Dest_Match);

  -- Search Connecting_Rec by Fd
  function Fd_Match (R1, R2 : Connecting_Rec) return Boolean is
    use type Event_Mng.File_Desc;
  begin
    return R1.Fd_Set and then R2.Fd_Set and then R1.Fd = R2.Fd;
  end Fd_Match;
  procedure Find_By_Fd is new Con_List_Mng.Unsafe_Search (Fd_Match);

  -- Delete current connection rec in list
  procedure Delete_Current_Con is
    Done : Boolean;
  begin
    Con_List_Mng.Delete (Con_List, Done => Done);
  end Delete_Current_Con;

  -- Try to open a socket and connect
  -- Dscr is open and non blocking, or No_Socket if failure
  -- Sets connected if connection established
  procedure Try_Connect (
           Protocol : in Tcp_Protocol_List;
           Host     : in Remote_Host;
           Port     : in Remote_Port;
           Dscr      : in out Socket.Socket_Dscr;
           Connected : out Boolean) is
    use type  Socket.Socket_Dscr;
  begin

    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.Try_Connect start");
    end if;
    -- Open non blocking
    Socket.Open (Dscr, Protocol);
    Socket.Set_Blocking (Dscr, False);
    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.Try_Connect socket open");
    end if;

    -- Connect
    case Host.Kind is
      when Host_Name_Spec =>
        case Port.Kind is
          when Port_Name_Spec =>
            Socket.Set_Destination_Name_And_Service (Dscr, False,
                                   Parse (Host.Name), Parse (Port.Name));
          when Port_Num_Spec =>
            Socket.Set_Destination_Name_And_Port (Dscr, False,
                                   Parse (Host.Name), Port.Num);
        end case;
      when Host_Id_Spec =>
        case Port.Kind is
          when Port_Name_Spec =>
            Socket.Set_Destination_Host_And_Service (Dscr,
                                   Host.Id, Parse(Port.Name));
          when Port_Num_Spec =>
            Socket.Set_Destination_Host_And_Port (Dscr,
                                   Host.Id, Port.Num);
        end case;
    end case;
    if Socket.Is_Connected (Dscr) then
      Connected := True;
    else
      Socket.Close (Dscr);
      Connected := False;
    end if;
    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.Try_Connect result " & Connected'Img);
    end if;
  exception
    when Socket.Soc_Conn_Refused =>
      -- Not open or not connected
      if Socket.Is_Open (Dscr) then
        Socket.Close (Dscr);
      end if;
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Try_Connect refused");
      end if;
    when Socket.Soc_Would_Block =>
      Connected := False;
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Try_Connect would block");
      end if;
  end Try_Connect;

  -- Handle a connection success/failure according to Rec.Dscr (open or not)
  -- Cancel timer
  -- Delete connection rec
  -- Get dest if success
  -- Call callback
  procedure Handle_Current_Result (Rec : in Connecting_Rec) is
    Port : Port_Num;
    Host : Host_Id;
    use type Timers.Timer_Id;
    use type Socket.Socket_Dscr;
  begin
    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.Handle_Current_Result start");
    end if;
    -- Remove management data
    if Rec.Timer /= Timers.No_Timer then
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Handle_Current_Result delete timer"
                      & Timers.Image (Rec.Timer));
      end if;
      Timers.Delete (Rec.Timer);
    end if;
    Delete_Current_Con;
    if Socket.Is_Open (Rec.Dscr) then
      -- Connected
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Handle_Current_Result connected");
      end if;
      Host := Socket.Get_Destination_Host (Rec.Dscr);
      Port := Socket.Get_Destination_Port (Rec.Dscr);
      Socket.Set_Blocking (Rec.Dscr, True);
    else
      -- Giving up
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Handle_Current_Result giving up");
      end if;
      if Rec.Port.Kind = Port_Name_Spec then
        begin
          -- Services may have changed since Connect_To checks
          Port := Socket.Port_Num_Of (Parse (Rec.Port.Name),
                                      Rec.Protocol);
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
          Host := Socket.Host_Id_Of (Parse (Rec.Host.Name));
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
      Rec.Cb (Port, Host,  Socket.Is_Open (Rec.Dscr), Rec.Dscr);
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Handle_Current_Result Cb called");
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
      My_Io.Put_Line ("  Tcp_Util.End_Async_Connect start");
    end if;
    -- Remove Fd callbacks. Thery are active at this time
    --  either because called by Fd_Cb or by Timer_Cb with open connection
    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.End_Async_Connect del Cbs on fd "
                    & Rec.Fd'Img);
    end if;
    Event_Mng.Del_Fd_Callback (Rec.Fd, True);
    Event_Mng.Del_Fd_Callback (Rec.Fd, False);

    -- Close if failure and still open
    if not Success and then Socket.Is_Open (Rec.Dscr) then
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.End_Async_Connect closing socket");
      end if;
      Socket.Close (Rec.Dscr);
      Rec.Fd_Set := False;
    end if;

    -- Note that if success but Dscr not open, which would be a bug,
    --  is silently handled here as a failure

    -- Success or failure
    if Socket.Is_Open (Rec.Dscr) then
      -- Connection success
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.End_Async_Connect success");
      end if;
      Handle_Current_Result (Rec);
      Go_On := False;
    else
      -- This try failure
      -- Give up if no more try
      if Rec.Curr_Try = Rec.Nb_Tries then
        if Debug_Connect then
          My_Io.Put_Line ("  Tcp_Util.End_Async_Connect last failure");
        end if;
        Handle_Current_Result (Rec);
        Go_On := False;
      else
        if Debug_Connect then
          My_Io.Put_Line ("  Tcp_Util.End_Async_Connect this failure");
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
      My_Io.Put_Line ("  Tcp_Util.Connection_Fd_Cb start with fd " & Fd'Img
                    & "  read " & Read'Img);
    end if;
    -- Find record by fd
    Rec.Fd := Fd;
    Rec.Fd_Set := True;
    Find_By_Fd (Con_List, Rec, From => Con_List_Mng.Absolute);

    Con_List_Mng.Read (Con_List, Rec, Con_List_Mng.Current);
    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.Connection_Fd_Cb found rec "
                    & Positive'Image (Con_List_Mng.Get_Position (Con_List)));
    end if;

    -- This try result?
    if not Socket.Is_Connected (Rec.Dscr) then
      -- Cancel and close
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Connection_Fd_Cb not connected");
      end if;
      End_Async_Connect (False, Rec, Go_On);
    else
      -- Success
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Connection_Fd_Cb connected");
      end if;
      End_Async_Connect (True, Rec, Go_On);
    end if;
    if Go_On then
      -- Store closed Dscr for timer callback
      Con_List_Mng.Modify (Con_List, Rec, Con_List_Mng.Current);
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Connection_Fd_Cb update rec "
                      & Positive'Image (Con_List_Mng.Get_Position (Con_List)));
      end if;
    end if;
    -- Propagate event if no Go_On
    return not Go_On;
  exception
    when Con_List_Mng.Not_In_List =>
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Connection_Fd_Cb fd rec not found");
      end if;
      raise;
  end Connection_Fd_Cb;

  -- Timer callback
  function Connection_Timer_Cb (Id : Timers.Timer_Id;
                                Data : Timers.Timer_Data := Timers.No_Data)
           return Boolean is

    Rec : Connecting_Rec;
    Connected : Boolean;
    Go_On : Boolean;
    use type Timers.Timer_Id;
    use type Socket.Socket_Dscr;
  begin
    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.Connection_Timer_Cb start on timer "
                    & Timers.Image (Id));
    end if;
    -- Read rec: Try current
    Con_List_Mng.Read (Con_List, Rec, Con_List_Mng.Current);
    if Rec.Timer /= Id then
      -- No good. Locate it
      Rec.Timer := Id;
      Find_By_Timer (Con_List, Go_On, Rec, From => Con_List_Mng.Absolute);
      if not Go_On then
        if Debug_Connect then
          My_Io.Put_Line ("  Tcp_Util.Connection_Timer_Cb timer rec not found");
        end if;
        return False;
      end if;
      Con_List_Mng.Read (Con_List, Rec, Con_List_Mng.Current);
    end if;
    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.Connection_Timer_Cb found rec "
                    & Positive'Image (Con_List_Mng.Get_Position (Con_List)));
    end if;

    -- Either first try, or previous failed (sync or not)
    --  or async connect is pending
    -- Rec.Curr_Try is current try number

    -- Cancel pending async connect
    if Socket.Is_Open (Rec.Dscr) then
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Connection_Timer_Cb is open");
      end if;
      End_Async_Connect (False, Rec, Go_On);
      if not Go_On then
        return True;
      end if;
    end if;

    -- Try to connect
    Try_Connect (Rec.Protocol, Rec.Host, Rec.Port, Rec.Dscr, Connected);
    Rec.Curr_Try :=  Rec.Curr_Try + 1;

    if Socket.Is_Open (Rec.Dscr) and then Connected then
      -- Connected synchronous success
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Connection_Timer_Cb synchronous success");
      end if;
      Handle_Current_Result (Rec);
      return True;
    elsif not Socket.Is_Open (Rec.Dscr) then
      -- Connect synchronous failure: Check number of tries
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Connection_Timer_Cb synchronous failure");
      end if;
      if Rec.Curr_Try = Rec.Nb_Tries then
        if Debug_Connect then
          My_Io.Put_Line ("  Tcp_Util.Connection_Timer_Cb last failure");
        end if;
        Handle_Current_Result (Rec);
        return True;
      end if;
    end if;

    -- Asynchronous pending
    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.Connection_Timer_Cb pending");
    end if;
    if Socket.Is_Open (Rec.Dscr) then
      -- Connection pending
      -- Save Dscr, Fd and pending status
      Rec.Fd := Socket.Fd_Of (Rec.Dscr);
      Rec.Fd_Set := True;
      -- Add callback on fd
      Event_Mng.Add_Fd_Callback (Rec.Fd, True, Connection_Fd_Cb'Access);
      Event_Mng.Add_Fd_Callback (Rec.Fd, False, Connection_Fd_Cb'Access);
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Connection_Timer_Cb asynchronous pending"
                        & " on fd " & Rec.Fd'Img);
      end if;
    end if;

    -- Synchronous failure or pending: arm timer at first try
    if Rec.Curr_Try = 1 then
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Connection_Timer_Cb first try");
      end if;
      -- First attempt failure: start timer
      Rec.Timer := Timers.Create (
          Delay_Spec => (Delay_Kind    => Timers.Delay_Sec,
                         Period        => Rec.Delta_Retry,
                         Delay_Seconds =>  Rec.Delta_Retry),
          Callback => Connection_Timer_Cb'Access);
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Connection_Timer_Cb created timer "
                      & Timers.Image (Rec.Timer));
      end if;
    end if;  
    -- Store Rec: Fd, Timer_Id, Curr_Try ...
    Con_List_Mng.Modify (Con_List, Rec, Con_List_Mng.Current);
    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.Connection_Timer_Cb update rec "
                    & Positive'Image (Con_List_Mng.Get_Position (Con_List)));
    end if;
    return False;
  end Connection_Timer_Cb;

  -- Connect to a remote Host/Port
  -- May make several tries (one each Delta_Retry) before giving up 
  -- Return True if synchronous result
  function Connect_To (Protocol      : in Tcp_Protocol_List;
                       Host          : in Remote_Host;
                       Port          : in Remote_Port;
                       Delta_Retry   : in Duration := 1.0;
                       Nb_Tries      : in Natural := 1;
                       Connection_Cb : in Connection_Callback_Access)
           return Boolean is
    Rec : Connecting_Rec;
  begin
    Init_Debug;
    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.Connect_To start");
    end if;
    if Delta_Retry <= 0.0 then
      raise Invalid_Delay;
    end if;
    -- Check port and host name
    if Port.Kind = Port_Name_Spec then
      declare
        Num : Port_Num;
      begin
        Num := Socket.Port_Num_Of (Parse (Port.Name), Protocol);
      exception
        when others =>
          raise Name_Error;
      end;
    end if;
    if Host.Kind = Host_Name_Spec then
      declare 
        Id : Host_Id;
      begin
        Id := Socket.Host_Id_Of (Parse (Host.Name));
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
    Rec.Cb := Connection_Cb;
    Rec.Timer := Timers.No_Timer;
    Rec.Dscr := Socket.No_Socket;
    Rec.Curr_Try := 0;
    Rec.Fd_Set := False;
    Con_List_Mng.Insert (Con_List, Rec);

    -- Try to connect: call timer callback
    -- Our Rec should be the only one with No_Timer
    return Connection_Timer_Cb (Timers.No_Timer);

  end Connect_To;

  -- Abort a pending connection
  -- May raise No_Such
  procedure Abort_Connect (Host : in Remote_Host;
                           Port : in Remote_Port) is
    Rec : Connecting_Rec;
    Ok : Boolean;
  begin
    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.Abort_Connect start");
    end if;
    -- Find rec
    Rec.Host := Host;
    Rec.Port := Port;
    Find_By_Dest (Con_List, Ok, Rec, From => Con_List_Mng.Absolute);
    if not Ok then
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Abort_Connect rec not found");
      end if;
      raise No_Such;
    end if;
    Con_List_Mng.Read (Con_List, Rec, Con_List_Mng.Current);
    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.Abort_Connect found rec "
                    & Positive'Image (Con_List_Mng.Get_Position (Con_List)));
    end if;
    if Rec.Fd_Set then
      if Debug_Connect then
        My_Io.Put_Line ("  Tcp_Util.Abort_Connect del Cbs on fd "
                      & Rec.Fd'Img);
      end if;
      Event_Mng.Del_Fd_Callback (Rec.Fd, True);
      Event_Mng.Del_Fd_Callback (Rec.Fd, False);
      Socket.Close (Rec.Dscr);
    end if;
    -- Cancel timer
    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.Abort_Connect deleting timer "
                    & Timers.Image (Rec.Timer));
    end if;
    Timers.Delete (Rec.Timer);
    -- Delete rec
    if Debug_Connect then
      My_Io.Put_Line ("  Tcp_Util.Abort_Connect deleting rec");
    end if;
    Con_List_Mng.Delete (Con_List, Done => Ok);
  end Abort_Connect;

  --------------------------------------------------------------------------

  -- Accepting connection
  type Accepting_Rec is record
    Port : Port_Num;
    Cb   : Acception_Callback_Access;
    Dscr : Socket.Socket_Dscr;
    Fd   : Event_Mng.File_Desc;
  end record;
  package Acc_List_Mng is new Dynamic_List (Accepting_Rec);
  Acc_List : Acc_List_Mng.List_Type;

  -- Search Accepting_Rec by Fd
  function Fd_Match (R1, R2 : Accepting_Rec) return Boolean is
    use type Event_Mng.File_Desc;
  begin
    return R1.Fd = R2.Fd;
  end Fd_Match;
  procedure Find_By_Fd is new Acc_List_Mng.Unsafe_Search (Fd_Match);

  --  Search Accepting_Rec by Port_Num
  function Port_Match (R1, R2 : Accepting_Rec) return Boolean is
    use type Socket.Port_Num;
  begin
    return R1.Port = R2.Port;
  end Port_Match;
  procedure Find_By_Port is new Acc_List_Mng.Search (Port_Match);

  -- Callback on accept fd
  function Acception_Fd_Cb (Fd : in Event_Mng.File_Desc;
                            Read : in Boolean) return Boolean is
    Rec : Accepting_Rec;
    New_Dscr : Socket.Socket_Dscr;
    Port : Port_Num;
    Host : Host_Id;
  begin
    if Debug_Accept then
      My_Io.Put_Line ("  Tcp_Util.Acception_Fd_Cb start with fd " & Fd'Img
                    & "  read " & Read'Img);
    end if;
    -- Find record by fd
    Rec.Fd := Fd;
    Find_By_Fd (Acc_List, Rec, From => Acc_List_Mng.Absolute);
    Acc_List_Mng.Read (Acc_List, Rec, Acc_List_Mng.Current);
    if Debug_Accept then
      My_Io.Put_Line ("  Tcp_Util.Acception_Fd_Cb found rec "
                    & Positive'Image (Acc_List_Mng.Get_Position (Acc_List)));
    end if;

    -- Accept
    Socket.Accept_Connection (Rec.Dscr, New_Dscr);
    if Debug_Accept then
      My_Io.Put_Line ("  Tcp_Util.Acception_Fd_Cb connection accepted");
    end if;

    -- Call callback
    Host := Socket.Get_Destination_Host (New_Dscr);
    Port := Socket.Get_Destination_Port (New_Dscr);
    if Rec.Cb /= null then
      Rec.Cb (Rec.Port, Rec.Dscr, Port, Host, New_Dscr);
      if Debug_Accept then
        My_Io.Put_Line ("  Tcp_Util.Acception_Fd_Cb Cb called");
      end if;
    end if;
    return True;
  exception
    when Acc_List_Mng.Not_In_List =>
      if Debug_Accept then
        My_Io.Put_Line ("  Tcp_Util.Acception_Fd_Cb fd rec not found");
      end if;
      raise;
  end Acception_Fd_Cb;

  -- Accept connections to a local port
  -- Dscr is set to the socket accepting connections
  procedure Accept_From (Protocol     : in Tcp_Protocol_List;
                         Port         : in Local_Port;
                         Acception_Cb : in Acception_Callback_Access;
                         Dscr         : in out Socket.Socket_Dscr;
                         Num          : out Port_Num) is
    Rec : Accepting_Rec;
  begin
    Init_Debug;
    if Debug_Accept then
      My_Io.Put_Line ("  Tcp_Util.Accept_From start");
    end if;
    -- Initialise Rec
    Rec.Port := 0;
    Rec.Cb := Acception_Cb;
    Rec.Dscr := Socket.No_Socket;

    -- Open socket
    Socket.Open (Dscr, Protocol);
    Rec.Dscr := Dscr;
    Rec.Fd := Socket.Fd_Of (Rec.Dscr);
    -- Bind socket
    case Port.Kind is
      when Port_Name_Spec =>
        Socket.Link_Service (Rec.Dscr, Parse(Port.Name));
      when Port_Num_Spec =>
        Socket.Link_Port (Rec.Dscr, Port.Num);
      when Port_Dynamic_Spec =>
        Socket.Link_Dynamic (Rec.Dscr);
    end case;
    Rec.Port := Socket.Get_Linked_To(Rec.Dscr);
    Num := Rec.Port;
    if Debug_Accept then
      My_Io.Put_Line ("  Tcp_Util.Accept_From linked");
    end if;

    -- Add callback on fd
    Event_Mng.Add_Fd_Callback (Rec.Fd, True, Acception_Fd_Cb'Access);

    -- Store Rec
    Acc_List_Mng.Insert (Acc_List, Rec);
    if Debug_Accept then
      My_Io.Put_Line ("  Tcp_Util.Tcp_Util.Accept_From insert rec "
                    & Positive'Image (Acc_List_Mng.Get_Position (Acc_List)));
    end if;
    
  exception
    when others => 
      -- Not open or not bind
      if Socket.Is_Open (Dscr) then
        Socket.Close (Dscr);
      end if;
      raise;
  end Accept_From;

  -- Abort further accepts on port
  -- May raise No_Such
  procedure Abort_Accept (Num : in Port_Num) is
    Rec : Accepting_Rec;
    Ok : Boolean;
  begin
    if Debug_Accept then
      My_Io.Put_Line ("  Tcp_Util.Abort_Accept start");
    end if;
    -- Find rec and read
    Rec.Port := Num;
    Find_By_Port (Acc_List, Ok, Rec, From => Acc_List_Mng.Absolute);
    if not Ok then
      if Debug_Accept then
        My_Io.Put_Line ("  Tcp_Util.Abort_Accept rec not found");
      end if;
      raise No_Such;
    end if;
    Acc_List_Mng.Read (Acc_List, Rec, Acc_List_Mng.Current);
    if Debug_Accept then
      My_Io.Put_Line ("  Tcp_Util.Abort_Accept found rec "
                    & Positive'Image (Acc_List_Mng.Get_Position (Acc_List)));
    end if;
    -- Del callback, close and delete rec
    Event_Mng.Del_Fd_Callback (Rec.Fd, True);
    Socket.Close (Rec.Dscr);
    Acc_List_Mng.Delete (Acc_List, Done => Ok);
    if Debug_Accept then
      My_Io.Put_Line ("  Tcp_Util.Abort_Accept socket closed and rec deleted");
    end if;
  end Abort_Accept;

  --------------------------------------------------------------------------

  -- Sending 
  type Sending_Rec is record
    Dscr : Socket.Socket_Dscr;
    Fd   : Event_Mng.File_Desc;
    Cb : End_Overflow_Callback_Access;
  end record;

  package Sen_List_Mng is new Dynamic_List (Sending_Rec);
  Sen_List : Sen_List_Mng.List_Type;

  -- Search Sending_Rec by Dscr
  function Dscr_Match (R1, R2 : Sending_Rec) return Boolean is
    use type Socket.Socket_Dscr;
  begin
    return R1.Dscr = R2.Dscr;
  end Dscr_Match;
  procedure Find_By_Dscr is new Sen_List_Mng.Search (Dscr_Match);

  -- Search Sending_Rec by Fd
  function Fd_Match (R1, R2 : Sending_Rec) return Boolean is
    use type Event_Mng.File_Desc;
  begin
    return R1.Fd = R2.Fd;
  end Fd_Match;
  procedure Find_By_Fd is new Sen_List_Mng.Unsafe_Search (Fd_Match);

  -- Delete current sending rec in list
  procedure Delete_Current_Sen is
    Done : Boolean;
  begin
    Sen_List_Mng.Delete (Sen_List, Done => Done);
  end Delete_Current_Sen;

  -- Sending callback on fd
  function Sending_Cb (Fd : in Event_Mng.File_Desc; Read : in Boolean)
  return Boolean is
    Rec : Sending_Rec;
  begin
    if Debug_Overflow then
      My_Io.Put_Line ("  Tcp_Util.Sending_Cb start with fd " & Fd'Img
                    & "  read " & Read'Img);
    end if;
    -- Find Rec from Fd and read
    Rec.Fd := Fd;
    Find_By_Fd (Sen_List, Rec, From => Sen_List_Mng.Absolute);
    Sen_List_Mng.Read (Sen_List, Rec, Sen_List_Mng.Current);
    if Debug_Overflow then
      My_Io.Put_Line ("  Tcp_Util.Sending_Cb found rec "
                    & Positive'Image (Sen_List_Mng.Get_Position (Sen_List)));
    end if;

    -- Try to re send
    begin
      Socket.Re_Send (Rec.Dscr);
    exception
      when Socket.Soc_Would_Block =>
        -- Still in overflow
        if Debug_Overflow then
          My_Io.Put_Line ("  Tcp_Util.Sending_Cb still in overflow");
        end if;
        return False;
      when others =>
        if Debug_Overflow then
          My_Io.Put_Line ("  Tcp_Util.Sending_Cb error");
        end if;
    end;
    if Debug_Overflow then
      My_Io.Put_Line ("  Tcp_Util.Sending_Cb resent");
    end if;

    -- End of overflow: unhook callback and del rec
    Event_Mng.Del_Fd_Callback (Rec.Fd, False);
    Delete_Current_Sen;
    if Debug_Overflow then
      My_Io.Put_Line ("  Tcp_Util.Sending_Cb cleaned");
    end if;

    -- Call user callbak
    if Rec.Cb /= null then
      Rec.Cb (Rec.Dscr);
      if Debug_Overflow then
        My_Io.Put_Line ("  Tcp_Util.Sending_Cb Cb called");
      end if;
    end if;
    return False;
  exception
    when Sen_List_Mng.Not_In_List =>
      if Debug_Overflow then
        My_Io.Put_Line ("  Tcp_Util.Sending_Cb fd rec not found");
      end if;
    raise;
  end Sending_Cb;

  -- Send message, handling overflow
  function Send (Dscr               : in Socket.Socket_Dscr;
                 End_Of_Overflow_Cb : in End_Overflow_Callback_Access;
                 Message            : in Message_Type;
                 Length             : in Natural := 0) return Boolean is
    Rec : Sending_Rec;
    procedure Send is new Socket.Send (Message_Type);
  begin
    Init_Debug;
    -- Try to send
    begin
      Send (Dscr, Message, Length);
      return True;
    exception
      when Socket.Soc_Would_Block =>
        null;
    end;

    -- Handle overflow : build and store rec
    if Debug_Overflow then
      My_Io.Put_Line ("  Tcp_Util.Send oveflow");
    end if;
    Rec.Dscr := Dscr;
    Rec.Fd := Socket.Fd_Of (Dscr);
    Rec.Cb := End_Of_Overflow_Cb;
    Sen_List_Mng.Insert (Sen_List, Rec);
    if Debug_Overflow then
      My_Io.Put_Line ("  Tcp_Util.Send rec with fd " & Rec.Fd'Img
                    & " inserted at "
                    & Positive'Image (Sen_List_Mng.Get_Position (Sen_List)));
    end if;

    -- Hook our callback in write
    Event_Mng.Add_Fd_Callback (Rec.Fd, False, Sending_Cb'Access);
    if Debug_Overflow then
      My_Io.Put_Line ("  Tcp_Util.Send Cb hooked");
    end if;
    return False;
  exception
    when Error:others =>
      if Debug_Overflow then
        My_Io.Put_Line ("  Tcp_Util.Send exception "
            & Ada.Exceptions.Exception_Name (Error));
      end if;
      raise;
  end Send;

  -- Cancel overflow management and closes
  procedure Abort_Send_And_Close (Dscr : in out Socket.Socket_Dscr) is
    Rec : Sending_Rec;
    Ok : Boolean;
  begin
    if Debug_Overflow then
      My_Io.Put_Line ("  Tcp_Util.Abort_Send_and_Close start");
    end if;
    -- Find Rec from Dscr and read
    Rec.Dscr := Dscr;
    Find_By_Dscr (Sen_List, Ok, Rec, From => Sen_List_Mng.Absolute);
    if not Ok then
      if Debug_Overflow then
        My_Io.Put_Line ("  Tcp_Util.Abort_Send_and_Close rec not found");
      end if;
      raise No_Such;
    end if;
    Sen_List_Mng.Read (Sen_List, Rec, Sen_List_Mng.Current);
    if Debug_Overflow then
      My_Io.Put_Line ("  Tcp_Util.Abort_Send_and_Close found rec "
                    & Positive'Image (Sen_List_Mng.Get_Position (Sen_List)));
    end if;

    -- Unhook callback and del rec
    Event_Mng.Del_Fd_Callback (Rec.Fd, False);
    Delete_Current_Sen;

    -- Close
    Socket.Close (Dscr);
    if Debug_Overflow then
      My_Io.Put_Line ("  Tcp_Util.Abort_Send_and_Close done");
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
    package Rece_List_Mng is new Dynamic_List (Rece_Rec);
    Rece_List : Rece_List_Mng.List_Type;

    function Dscr_Match (R1, R2 : Rece_Rec) return Boolean is
      use type Socket.Socket_Dscr;
    begin
      return R1.Dscr = R2.Dscr;
    end Dscr_Match;
    procedure Find_Dscr is new Rece_List_Mng.Search (Dscr_Match);

    function Fd_Match (R1, R2 : Rece_Rec) return Boolean is
      use type Event_Mng.File_Desc;
    begin
      return R1.Fd = R2.Fd;
    end Fd_Match;
    procedure Find_Fd is new Rece_List_Mng.Search (Fd_Match);

    -- The one to use with Socket
    procedure Read is new Socket.Receive (Message_Type);

    -- Unhook and close a Dscr. Call appli Cb
    procedure Close_Current is
      Rec : Rece_Rec;
      Done : Boolean;
    begin
      -- Get from list
      Rece_List_Mng.Get (Rece_List, Rec, Done => Done);
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
          Socket.Close (Rec.Dscr);
      end;
    end Close_Current;

    -- The Cb hooked on Fd
    function Read_Cb (Fd : Event_Mng.File_Desc; For_Read : in Boolean)
                     return Boolean is
      use type Event_Mng.File_Desc;
      The_Rec : Rece_Rec;
      Found : Boolean;
      Msg : Message_Type;
      Len : Natural;
    begin
      if not For_Read then
        if Debug_Reception then
          My_Io.Put_Line ("  Tcp_Util.Read_Cb on writting fd " & Fd'Img);
        end if;
        return False;
      end if;
      -- Find dscr from Fd
      The_Rec.Fd := Fd;
      Find_Fd (Rece_List, Found, The_Rec, From => Rece_List_Mng.Absolute);
      if not Found then
        if Debug_Reception then
          My_Io.Put_Line ("  Tcp_Util.Read_Cb no Dscr for Fd " & Fd'Img);
        end if;
        Event_Mng.Del_Fd_Callback (Fd, True);
        return False;
      end if;
      Rece_List_Mng.Read (Rece_List, The_Rec, Rece_List_Mng.Current);

      -- Try to read
      begin
        Read (The_Rec.Dscr, Msg, Len);
      exception
        when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
          -- Remote has diconnected
          if Debug_Reception then
            My_Io.Put_Line ("  Tcp_Util.Read_Cb disconnection on fd " & Fd'Img);
          end if;
          Close_Current;
          return True;
        when Socket.Soc_Len_Err =>
          -- Invalid length
          if Debug_Reception then
            My_Io.Put_Line ("  Tcp_Util.Read_Cb invalid length on fd " & Fd'Img);
          end if;
          Close_Current;
          return True;
      end;
      if Debug_Reception then
        My_Io.Put_Line ("  Tcp_Util.Read_Cb len " & Len'Img & " on fd " & Fd'Img);
      end if;
      -- Call appli callback
      if The_Rec.Read_Cb /= null then
        The_Rec.Read_Cb (The_Rec.Dscr, Msg, Len);
      end if;
      return True;
    end Read_Cb;

    procedure Set_Callbacks (
                Dscr             : in Socket.Socket_Dscr;
                Reception_Cb     : in Reception_Callback_Access;
                Disconnection_Cb : in Disconnection_Callback_Access) is
      The_Rec : Rece_Rec;
    begin
      Init_Debug;
      if not Socket.Is_Open (Dscr) then
        raise No_Such;
      end if;

      The_Rec.Dscr := Dscr;
      The_Rec.Fd := Socket.Fd_Of (Dscr);
      The_Rec.Read_Cb := Reception_Cb;
      The_Rec.Discon_Cb := Disconnection_Cb;

      -- Append to list
      Rece_List_Mng.Insert (Rece_List, The_Rec);

      Event_Mng.Add_Fd_Callback (Socket.Fd_Of (Dscr), True,
                 Read_Cb'Unrestricted_Access);
        
      if Debug_Reception then
        My_Io.Put_Line ("  Tcp_Util.Set_Callbacks on Fd " & The_Rec.Fd'Img);
      end if;
    end Set_Callbacks;

    procedure Remove_Callbacks (Dscr : in Socket.Socket_Dscr) is
      The_Rec : Rece_Rec;
      Ok : Boolean;
    begin
      The_Rec.Dscr := Dscr;
        Find_Dscr (Rece_List, Ok, The_Rec, From => Rece_List_Mng.Absolute);
      if not Ok then
        if Debug_Reception then
          My_Io.Put_Line ("  Tcp_Util.Remove_Callbacks Dscr not found");
        end if;
        raise No_Such;
      end if;
        
      -- Get from list
      Rece_List_Mng.Get (Rece_List, The_Rec, Done => Ok);
      if Debug_Reception then
        My_Io.Put_Line ("  Tcp_Util.Remove_Callbacks on Fd " & The_Rec.Fd'Img);
      end if;
      Event_Mng.Del_Fd_Callback (Socket.Fd_Of (Dscr), True);
    end Remove_Callbacks;

  end Reception;

end Tcp_Util;

