with Dynamic_List, Timers, X_Mng;
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
    

  -- Connecting or connected connection
  type Connecting_Rec is record
    Protocol : Tcp_Protocol_List;
    Host : Remote_Host;
    Port : Remote_Port;
    Delta_Retry : Duration;
    Nb_Tries : Natural;
    Cb : Connection_Callback_Access;
    Timer : Timers.Timer_Id;
    Dscr : Socket.Socket_Dscr;
    Fd   : X_Mng.File_Desc;
    Curr_Try : Natural;
  end record;

  Package Con_List_Mng is new Dynamic_List (Connecting_Rec);
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
    use type X_Mng.File_Desc;
  begin
    return R1.Fd = R2.Fd;
  end Fd_Match;
  procedure Find_By_Fd is new Con_List_Mng.Search (Fd_Match);

  -- Delete current connection rec in list
  procedure Delete_Current_Con is
  begin
    if Con_List_Mng.Get_Position (Con_List) = 1 then
       Con_List_Mng.Delete (Con_List, Con_List_Mng.Next);
    else
       Con_List_Mng.Delete (Con_List, Con_List_Mng.Prev);
    end if;
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

    -- Open non blocking
    Socket.Open (Dscr, Protocol);
    Socket.Set_Blocking (Dscr, False);

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
  exception
    when Socket.Soc_Conn_Refused =>
      -- Not open or not connected
      if Socket.Is_Open (Dscr) then
        Socket.Close (Dscr);
      end if;
    when Socket.Soc_Would_Block =>
      Connected := False;
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
    -- Remove management data
    if Rec.Timer /= Timers.No_Timer then
      Timers.Delete (Rec.Timer);
    end if;
    Delete_Current_Con;
    if Socket.Is_Open (Rec.Dscr) then
      -- Connected
      Host := Socket.Get_Destination_Host (Rec.Dscr);
      Port := Socket.Get_Destination_Port (Rec.Dscr);
      Socket.Set_Blocking (Rec.Dscr, True);
    else
      -- Giving up
      Port := 0;
      host := Socket.No_Host;
    end if;
    -- Inform client
    if Rec.Cb /= null then
      Rec.Cb (Port, Host,  Socket.Is_Open (Rec.Dscr), Rec.Dscr);
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
    -- Remove Fd callback
    X_Mng.X_Del_CallBack (Rec.Fd, True);
    X_Mng.X_Del_CallBack (Rec.Fd, False);

    -- Close if failure and still open
    if not Success and then Socket.Is_Open (Rec.Dscr) then
      Socket.Close (Rec.Dscr);
    end if;

    -- Note that if success but Dscr not open, which would be a bug,
    --  is silently handled here as a failure

    -- Success or failure
    if Socket.Is_Open (Rec.Dscr) then
      -- Connection success
      Handle_Current_Result (Rec);
      Go_On := False;
    else
      -- This try failure
      -- Give up if no more try
      if Rec.Curr_Try = Rec.Nb_Tries then
        Handle_Current_Result (Rec);
        Go_On := False;
      else
        Go_On := True;
      end if;
    end if;
  end End_Async_Connect;

  -- Callback on connect fd
  function Connection_Fd_Cb (Fd : in X_Mng.File_Desc;
                             Read : in Boolean) return Boolean is
    Rec : Connecting_Rec;
    Go_On : Boolean;
    use type Socket.Socket_Dscr;
  begin
    -- Find record by fd
    Rec.Fd := Fd;
    Find_By_Fd (Con_List, Rec, From_Current => False);
    Con_List_Mng.Read (Con_List, Rec, Con_List_Mng.Current);

    -- This try result?
    if not Socket.Is_Connected (Rec.Dscr) then
      -- Cancel and close
      End_Async_Connect (False, Rec, Go_On);
    else
      -- Success
      End_Async_Connect (True, Rec, Go_On);
    end if;
    if Go_On then
      -- Store closed Dscr for timer callback
      Con_List_Mng.Modify (Con_List, Rec, Con_List_Mng.Current);
    end if;
    -- Propagate event if no Go_On
    return not Go_On;
  end Connection_Fd_Cb;

  -- Timer callback
  function Connection_Timer_Cb (Id : in Timers.Timer_Id) return Boolean is
    Rec : Connecting_Rec;
    Connected : Boolean;
    Go_On : Boolean;
    use type Timers.Timer_Id;
    use type Socket.Socket_Dscr;
  begin
    -- Read rec: Try current
    Con_List_Mng.Read (Con_List, Rec, Con_List_Mng.Current);
    if Rec.Timer /= Id then
      -- No good. Locate it
      Rec.Timer := Id;
      Find_By_Timer (Con_List, Rec, From_Current => False);
      Con_List_Mng.Read (Con_List, Rec, Con_List_Mng.Current);
    end if;

    -- Either first try, or previous failed (sync or not)
    --  or async connect is pending
    -- Rec.Curr_Try is current try number

    -- Cancel pending async connect. Check end of tries
    if Socket.Is_Open (Rec.Dscr) then
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
      Handle_Current_Result (Rec);
      return True;
    elsif not Socket.Is_Open (Rec.Dscr) then
      -- Connect synchronous failure: Check number of tries
      if Rec.Curr_Try = Rec.Nb_Tries then
        Handle_Current_Result (Rec);
        return True;
      end if;
    end if;

    -- Asynchronous pending
    if Socket.Is_Open (Rec.Dscr) then
      -- Connection pending
      -- Save Dscr, Fd and pending status
      Rec.Fd := Socket.Fd_Of (Rec.Dscr);
      -- Add callback on fd
      X_Mng.X_Add_CallBack (Rec.Fd, True, Connection_Fd_Cb'access);
      X_Mng.X_Add_CallBack (Rec.Fd, False, Connection_Fd_Cb'access);
    end if;

    -- Synchronous failure or pending: arm timer at first try
    if Rec.Curr_Try = 1 then
      -- First attempt failure: start timer
      Rec.Timer := Timers.Create (
          Delay_Spec => (Delay_Kind    => Timers.Delay_Sec,
                         Period        => Rec.Delta_Retry,
                         Delay_Seconds =>  Rec.Delta_Retry),
          Callback => Connection_Timer_Cb'access);
    end if;  
    -- Store Rec: Fd, Timer_Id, Curr_Try ...
    Con_List_Mng.Modify (Con_List, Rec, Con_List_Mng.Current);
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
  begin
    -- Find rec
    Rec.Host := Host;
    Rec.Port := Port;
    Find_By_Dest (Con_List, Rec, From_Current => False);
    -- Cancel timer
    Timers.Delete (Rec.Timer);
    -- Delete rec
    if Con_List_Mng.Get_Position (Con_List) /= 1 then
      Con_List_Mng.Delete (Con_List, Con_List_Mng.Prev);
    else
      Con_List_Mng.Delete (Con_List, Con_List_Mng.Next);
    end if;
  exception
    when Con_List_Mng.Not_In_List =>
      raise No_Such;
  end Abort_Connect;

  --------------------------------------------------------------------------

  -- Accepting connection
  type Accepting_Rec is record
    Port : Port_Num;
    Cb   : Acception_Callback_Access;
    Dscr : Socket.Socket_Dscr;
    Fd   : X_Mng.File_Desc;
  end record;
  package Acc_List_Mng is new Dynamic_List (Accepting_Rec);
  Acc_List : Acc_List_Mng.List_Type;

  -- Search Accepting_Rec by Fd
  function Fd_Match (R1, R2 : Accepting_Rec) return Boolean is
    use type X_Mng.File_Desc;
  begin
    return R1.Fd = R2.Fd;
  end Fd_Match;
  procedure Find_By_Fd is new Acc_List_Mng.Search (Fd_Match);

  --  Search Accepting_Rec by Port_Num
  function Port_Match (R1, R2 : Accepting_Rec) return Boolean is
    use type Socket.Port_Num;
  begin
    return R1.Port = R2.Port;
  end Port_Match;
  procedure Find_By_Port is new Acc_List_Mng.Search (Port_Match);

  -- Callback on accept fd
  function Acception_Fd_Cb (Fd : in X_Mng.File_Desc;
                            Read : in Boolean) return Boolean is
    Rec : Accepting_Rec;
    New_Dscr : Socket.Socket_Dscr;
    Port : Port_Num;
    Host : Host_Id;
  begin
    -- Find record by fd
    Rec.Fd := Fd;
    Find_By_Fd (Acc_List, Rec, From_Current => False);
    Acc_List_Mng.Read (Acc_List, Rec, Acc_List_Mng.Current);

    -- Accept
    Socket.Accept_Connection (Rec.Dscr, New_Dscr);

    -- Call callback
    Host := Socket.Get_Destination_Host (New_Dscr);
    Port := Socket.Get_Destination_Port (New_Dscr);
    if Rec.Cb /= null then
      Rec.Cb (Rec.Port, Rec.Dscr, Port, Host, New_Dscr);
    end if;
    return True;
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

    -- Add callback on fd
    X_Mng.X_Add_CallBack (Rec.Fd, True, Acception_Fd_Cb'access);

    -- Store Rec
    Acc_List_Mng.Insert (Acc_List, Rec);
    
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
  begin
    -- Find rec and read
    Rec.Port := Num;
    Find_By_Port (Acc_List, Rec, From_Current => False);
    Acc_List_Mng.Read (Acc_List, Rec, Acc_List_Mng.Current);
    -- Del callback and close
    X_Mng.X_Del_CallBack (Rec.Fd, True);
    Socket.Close (Rec.Dscr);
  exception
    when Con_List_Mng.Not_In_List =>
      raise No_Such;
  end Abort_Accept;

  --------------------------------------------------------------------------

  -- Sending 
  type Sending_Rec is record
    Dscr : Socket.Socket_Dscr;
    Fd   : X_Mng.File_Desc;
    Cb : End_Overflow_Callback_Access;
  end record;

  Package Sen_List_Mng is new Dynamic_List (Sending_Rec);
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
    use type X_Mng.File_Desc;
  begin
    return R1.Fd = R2.Fd;
  end Fd_Match;
  procedure Find_By_Fd is new Sen_List_Mng.Search (Fd_Match);

  -- Delete current sending rec in list
  procedure Delete_Current_Sen is
  begin
    if Sen_List_Mng.Get_Position (Sen_List) = 1 then
       Sen_List_Mng.Delete (Sen_List, Sen_List_Mng.Next);
    else
       Sen_List_Mng.Delete (Sen_List, Sen_List_Mng.Prev);
    end if;
  end Delete_Current_Sen;

  -- Sending callback on fd
  function Sending_Cb (Fd : in X_Mng.File_Desc; Read : in Boolean)
  return Boolean is
    Rec : Sending_Rec;
  begin
    -- Find Rec from Fd and read
    Rec.Fd := Fd;
    Find_By_Fd (Sen_List, Rec, From_Current => False);
    Sen_List_Mng.Read (Sen_List, Rec, Sen_List_Mng.Current);

    -- Try to re send
    begin
      Socket.Re_Send (Rec.Dscr);
    exception
      when Socket.Soc_Would_Block =>
        -- Still in overflow
        return False;
    end;

    -- End of overflow: unhook callback and del rec
    X_Mng.X_Del_CallBack (Rec.Fd, False);
    Delete_Current_Sen;

    -- Call user callbak
    if Rec.Cb /= null then
      Rec.Cb (Rec.Dscr);
    end if;
    return False;
  end Sending_Cb;

  -- Send message, handling overflow
  function Send (Dscr               : in Socket.Socket_Dscr;
                 End_Of_Overflow_Cb : in End_Overflow_Callback_Access;
                 Message            : in Message_Type;
                 Length             : in Natural := 0) return Boolean is
    Rec : Sending_Rec;
    procedure Send is new Socket.Send (Message_Type);
  begin
    -- Try to send
    begin
      Send (Dscr, Message, Length);
      return True;
    exception
      when Socket.Soc_Would_Block =>
        null;
    end;

    -- Handle overflow : build and store rec
    Rec.Dscr := Dscr;
    Rec.Fd := Socket.Fd_Of (Dscr);
    Rec.Cb := End_Of_Overflow_Cb;
    Sen_List_Mng.Insert (Sen_List, Rec);

    -- Hook our callback in write
    X_Mng.X_Add_CallBack (Rec.Fd, False, Sending_Cb'access);
    return False;
  end Send;
      
  -- Cancel overflow management and closes
  procedure Abort_Send_and_Close (Dscr : in out Socket.Socket_Dscr) is
    Rec : Sending_Rec;
  begin
    -- Find Rec from Dscr and read
    Rec.Dscr := Dscr;
    begin
      Find_By_Dscr (Sen_List, Rec, From_Current => False);
    exception
      when Sen_List_Mng.Not_In_List =>
        raise No_Such;
    end;
    Sen_List_Mng.Read (Sen_List, Rec, Sen_List_Mng.Current);

    -- Unhook callback and del rec
    X_Mng.X_Del_CallBack (Rec.Fd, False);
    Delete_Current_Sen;

    -- Close
    Socket.Close (Dscr);
  end Abort_Send_and_Close;

end Tcp_Util;

