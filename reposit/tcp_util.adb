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
    CB : Connection_Callback_Access;
    Timer : Timers.Timer_Id;
    Dscr : Socket.Socket_Dscr;
    Curr_Try : Positive;
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

  --  Search Connecting_Rec by Host, Port
  function Dest_Match (R1, R2 : Connecting_Rec) return Boolean is
  begin
    return R1.Host = R2.Host and then R1.Port = R2.Port;
  end Dest_Match;
  procedure Find_By_Dest is new Con_List_Mng.Search (Dest_Match);


  -- Try to open a socket and connect
  -- Return Socket.No_Scket is failed
  function Try_Connect (Protocol : in Tcp_Protocol_List;
                        Host     : in Remote_Host;
                        Port     : in Remote_Port) return Socket.Socket_Dscr is
    Dscr :  Socket.Socket_Dscr;
  begin
    -- Open
    Socket.Open (Dscr, Protocol);
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
    if not Socket.Is_Connected (Dscr) then
       Socket.Close (Dscr);
    end if;
    return Dscr;
  exception
    when Socket.Socket_Error =>
      -- Not open or not connected
      if Socket.Is_Open (Dscr) then
        Socket.Close (Dscr);
      end if;
      return Socket.No_Socket;
  end;

  procedure Connection_Timer_CB (Id : in Timers.Timer_Id) is
    Rec : Connecting_Rec;
    Port : Port_Num;
    Host : Host_Id;
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

    -- Try to connect
    Rec.Dscr :=  Try_Connect (Rec.Protocol, Rec.Host, Rec.Port);

    if Rec.Dscr /= Socket.No_Socket then
      -- Yes. Connected. Cancel timer and call callback
      if Rec.Timer /= Timers.No_Timer then
        Timers.Delete (Rec.Timer);
      end if;
      Host := Socket.Get_Destination_Host (Rec.Dscr);
      Port := Socket.Get_Destination_Port (Rec.Dscr);
      if Rec.CB /= null then
        Rec.CB (Port, Host, True, Rec.Dscr);
      end if;
    else
      -- Nop. Check number of tries
      if Rec.Curr_Try = Rec.Nb_Tries then
        -- Give up
        Timers.Delete (Rec.Timer);
        if Rec.CB /= null then
          Rec.CB (0, Socket.No_Host, False, Socket.No_Socket);
        end if;
      else
        -- Allow further retries
        if Rec.Curr_Try = 1 then
          -- First attempt failure: start timer
          Rec.Timer := Timers.Create (
              Delay_Spec => (Delay_Kind    => Timers.Delay_Sec,
                             Period        => Rec.Delta_Retry,
                             Delay_Seconds =>  Rec.Delta_Retry),
              Callback => Connection_Timer_CB'access);
        end if;  
        Rec.Curr_Try :=  Rec.Curr_Try + 1;
        -- Store timer id or at least Curr_Try
        Con_List_Mng.Modify (Con_List, Rec, Con_List_Mng.Current);
      end if;
    end if;
       
  end Connection_Timer_CB;


  -- Connect to a remote Host/Port
  -- May make several tries (one each Delta_Retry) before giving up 
  procedure Connect_To (Protocol      : in Tcp_Protocol_List;
                        Host          : in Remote_Host;
                        Port          : in Remote_Port;
                        Delta_Retry   : in Duration := 1.0;
                        Nb_Tries      : in Natural := 1;
                        Connection_CB : in Connection_Callback_Access) is
    Rec : Connecting_Rec;
  begin
    -- Initialise record and insert it in list
    Rec.Protocol := Protocol;
    Rec.Host := Host;
    Rec.Port := Port;
    Rec.Delta_Retry := Delta_Retry;
    Rec.Nb_Tries := Nb_Tries;
    Rec.CB := Connection_CB;
    Rec.Timer := Timers.No_Timer;
    Rec.Dscr := Socket.No_Socket;
    Rec.Curr_Try := 1;
    Con_List_Mng.Insert (Con_List, Rec);

    -- Try to connect: call timer callback
    -- Our Rec should be the only one with No_Timer
    Connection_Timer_CB (Timers.No_Timer);

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


  -- Accepting connection
  type Accepting_Rec is record
    Port : Port_Num;
    CB   : Acception_Callback_Access;
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
  procedure Acception_Fd_CB (Fd : in X_Mng.File_Desc) is
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
    if Rec.CB /= null then
      Rec.CB (Rec.Port, Rec.Dscr, Port, Host, New_Dscr);
    end if;
  end Acception_Fd_CB;

  -- Accept connections to a local port
  -- Dscr is set to the socket accepting connections
  procedure Accept_From (Protocol     : in Tcp_Protocol_List;
                         Port         : in Local_Port;
                         Acception_CB : in Acception_Callback_Access;
                         Dscr         : in out Socket.Socket_Dscr;
                         Num          : out Port_Num) is
    Rec : Accepting_Rec;
  begin
    -- Initialise Rec
    Rec.Port := 0;
    Rec.CB := Acception_CB;
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
    X_Mng.X_Add_CallBack (Rec.Fd, Acception_Fd_CB'access);

    -- Store Rec
    Acc_List_Mng.Insert (Acc_List, Rec);
    
  exception
    when Socket.Socket_Error => 
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
    -- Find rec
    Rec.Port := Num;
    Find_By_Port (Acc_List, Rec, From_Current => False);
    -- Del callback and close
    X_Mng.X_Del_CallBack (Rec.Fd);
    Socket.Close (Rec.Dscr);
  exception
    when Con_List_Mng.Not_In_List =>
      raise No_Such;
  end Abort_Accept;

end Tcp_Util;

