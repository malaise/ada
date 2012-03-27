with Basic_Proc, Socket, Tcp_Util, Ip_Addr, Dynamic_List, Mixed_Str;
with Debug, Partner;
package body Clients is

  -- Accepting or accepted connection from local to us
  --  or Connecting or Connected from us to local
  type Client_Status_List is (Accepting, Accepted, Connected);
  -- Descriptor of a client connection
  type Client_Rec is record
    -- The port associted to the client
    Port : Common.Port_Num;
    Status : Client_Status_List;
    -- When accepting: the accept Dscr, when accepted or connected:
    --  the connection Dscr
    Dscr : Socket.Socket_Dscr;
  end record;

  -- Dynamic list of clients (accepting/accepted/connected)
  package Client_List_Mng is new Dynamic_List (Client_Rec);
  package Client_Mng renames Client_List_Mng.Dyn_List;
  Client_List : Client_Mng.List_Type;
  function Match (Current, Criteria : Client_Rec) return Boolean is
    use type Socket.Port_Num;
  begin
    return Current.Port = Criteria.Port;
  end Match;
  procedure Search is new Client_Mng.Search (Match);
  function Search (Port : Socket.Port_Num) return Boolean is
    Client : Client_Rec;
    Found : Boolean;
  begin
    Client.Port := Port;
    Search (Client_List, Found, Client, From => Client_Mng.Absolute);
    return Found;
  end Search;

  -- Maps Port -> Client_Rec (Dscr)
  type Client_Access is access all Client_Rec;
  Accept_Port_Map  : array (Socket.Port_Num) of Client_Access;
  Connect_Port_Map : array (Socket.Port_Num) of Client_Access;

  -- Set reception callback on connection from/to client
  procedure Set_Callbacks (Dscr : in Socket.Socket_Dscr;
                           Local : in Boolean);

  -- Accept callback: Canncel accept, set connected and set reception callbacks
  procedure Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                       Local_Dscr      : in Socket.Socket_Dscr;
                       Remote_Host_Id  : in Tcp_Util.Host_Id;
                       Remote_Port_Num : in Tcp_Util.Port_Num;
                       New_Dscr        : in Socket.Socket_Dscr) is
    pragma Unreferenced (Local_Dscr);
    Acc : Client_Access;
  begin
    -- Check that this Dscr exists and is not connected
    if not Search (Local_Port_Num) then
      Basic_Proc.Put_Line_Error (
         "ERROR: Accepting dscr not found for port "
         & Ip_Addr.Image (Local_Port_Num) & ".");
      raise Common.Fatal_Error;
    end if;
    Acc := Client_Access(Client_List.Access_Current);
    if Acc.Status /= Accepting then
      Basic_Proc.Put_Line_Error (
         "ERROR: Port " & Ip_Addr.Image (Local_Port_Num)
         & " is " & Mixed_Str (Acc.Status'Img)
         & " instead of accepting.");
      raise Common.Fatal_Error;
    end if;
    -- Update
    Acc.Status := Accepted;
    Acc.Dscr := New_Dscr;
    -- Cancel Accept and set reception callbacks
    Tcp_Util.Abort_Accept (Socket.Tcp, Local_Port_Num);
    Set_Callbacks (New_Dscr, True);
    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Tcpipe: accepted client ("
        & Ip_Addr.Image (Socket.Id2Addr(Remote_Host_Id))
        & ":" & Ip_Addr.Image (Remote_Port_Num)
        & ") on port "
        & Ip_Addr.Image (Local_Port_Num));
    end if;
  end Accept_Cb;

  -- Accept on a local port
  -- When a client connects stop accepting until it disconnects (then restart
  --  accepting)
  -- When a client is connected relay data from/to it
  -- Invalid_Port : exception;
  procedure Accept_Client (Port : in String) is
    Client : Client_Rec;
    Loc_Port : Tcp_Util.Local_Port;
    use type Tcp_Util.Local_Port_List;
  begin
    -- Get port num
    Loc_Port := Ip_Addr.Parse (Port);
    if Loc_Port.Kind = Tcp_Util.Port_Name_Spec then
      begin
        Client.Port := Socket.Port_Num_Of (Loc_Port.Name.Image, Socket.Tcp);
      exception
        when Socket.Soc_Name_Not_Found =>
          Basic_Proc.Put_Line_Error ("Unknown port name "
                                   & Loc_Port.Name.Image & ".");
          raise Invalid_Port;
      end;
    else
      Client.Port := Loc_Port.Num;
    end if;

    -- Check unicity
    if Accept_Port_Map (Client.Port) /= null then
      Basic_Proc.Put_Line_Error ("Port num " & Ip_Addr.Image (Client.Port)
                                 & " already used.");
      raise Invalid_Port;
    end if;

    -- Accept
    Tcp_Util.Accept_From (Socket.Tcp, Loc_Port, Accept_Cb'Access,
                          Client.Dscr, Client.Port);

    -- Insert
    Client.Status := Accepting;
    Client_List.Insert (Client, Client_Mng.Prev);
    Accept_Port_Map(Client.Port) := Client_Access(Client_List.Access_Current);

  exception
    when Ip_Addr.Parse_Error =>
      raise Invalid_Port;
  end Accept_Client;

  -- Set target
  -- Invalid_Target : exception;
  Target_Id : Socket.Host_Id := Socket.Local_Host_Id;
  procedure Set_Target (Target : in String) is
    Host_Rec : Tcp_Util.Remote_Host;
    use type Tcp_Util.Remote_Host_List;
  begin
    Host_Rec := Ip_Addr.Parse (Target);
    if Host_Rec.Kind = Tcp_Util.Host_Name_Spec then
      begin
        Target_Id := Socket.Host_Id_Of (Host_Rec.Name.Image);
      exception
        when Socket.Soc_Name_Not_Found =>
          Basic_Proc.Put_Line_Error ("Unknown target host name "
                                   & Host_Rec.Name.Image & ".");
          raise Invalid_Target;
      end;
    else
      Target_Id := Host_Rec.Id;
    end if;

  exception
    when Ip_Addr.Parse_Error =>
      raise Invalid_Target;
  end Set_Target;

  -- Connect to a port on target
  -- If Ok then relay data from/to it
  -- Else send a Disconnect to partner
  -- May raise Invalid_Target if Target is not set
  procedure Connect_Client (Port : in Common.Port_Num) is
    Target_Host : Tcp_Util.Remote_Host (Tcp_Util.Host_Id_Spec);
    Target_Port : Tcp_Util.Remote_Port (Tcp_Util.Port_Num_Spec);
    Client : Client_Rec;
    Msg : Partner.Message;
  begin
    -- Nothing if already connected
    if Connect_Port_Map(Port) /= null then
      return;
    end if;
    Target_Host.Id := Target_Id;
    Target_Port.Num := Port;
    -- Synchronous connect with timeout
    Client.Dscr := Tcp_Util.Connect_To (Socket.Tcp, Target_Host, Target_Port,
                                 Timeout => 0.2);

    if Client.Dscr.Is_Open then
      if Debug.Is_Set then
        Basic_Proc.Put_Line_Output ("Tcpipe: connected to client ("
          & Ip_Addr.Image (Socket.Id2Addr(Target_Host.Id))
          & ":" & Ip_Addr.Image (Target_Port.Num)
          & ")");
      end if;
      -- Insert
      Client.Status := Connected;
      Client.Port := Port;
      Client_List.Insert (Client, Client_Mng.Prev);
      Connect_Port_Map(Port) := Client_Access(Client_List.Access_Current);
    else
      if Debug.Is_Set then
        Basic_Proc.Put_Line_Output ("Tcpipe: connection failure to client ("
          & Ip_Addr.Image (Socket.Id2Addr(Target_Host.Id))
          & ":" & Ip_Addr.Image (Target_Port.Num)
          & ")");
      end if;
      -- Send disconnect_local
      Msg.Head.Kind := Partner.Disconnect;
      Msg.Head.Local := True;
      Msg.Head.Port := Port;
      Partner.Send (Msg, 0);
    end if;
  end Connect_Client;

  -- Disconnect from a client
  -- If local then start accepting again
  procedure Disconnect (Port : in Common.Port_Num; Local : in Boolean) is
    Loc_Port : Tcp_Util.Local_Port (Tcp_Util.Port_Num_Spec);
    Found : Boolean;
  begin
    if Local then
      -- See if this Port is accepted
      if Accept_Port_Map(Port) = null
      or else Accept_Port_Map(Port).Status = Accepting then
        return;
      end if;
      -- Close
      Accept_Port_Map(Port).Dscr.Close;
      -- Accept
      Tcp_Util.Accept_From (Socket.Tcp, Loc_Port, Accept_Cb'Access,
                            Accept_Port_Map(Port).Dscr,
                            Accept_Port_Map(Port).Port);

      Accept_Port_Map(Port).Status := Accepting;
    else
      -- See if this Port is connected
      if Connect_Port_Map(Port) = null then
        return;
      end if;
      Client_List.Search_Access (Found, Connect_Port_Map(Port));
      if not Found then
        Basic_Proc.Put_Line_Error (
           "ERROR: Connected port " & Ip_Addr.Image (Port) & " not found.");
        raise Common.Fatal_Error;
      end if;
      -- Close and delete
      Connect_Port_Map(Port).Dscr.Close;
      Client_List.Delete (Moved => Found);
    end if;
  end Disconnect;

  -- Disconnect all clients
  -- If local then start accepting again
  procedure Disconnect_All is
    Acc : Client_Access;
    Moved : Boolean;
    Port : Tcp_Util.Local_Port (Tcp_Util.Port_Num_Spec);
  begin
    if Client_List.Is_Empty then
      return;
    end if;
    Client_List.Rewind;
    loop
      Acc := Client_Access(Client_List.Access_Current);
      case Acc.Status is
        when Accepting =>
          -- Skip and move to next
          null;
        when Accepted =>
          -- Close and Accept and move to next
          Acc.Dscr.Close;
          Port.Num := Acc.Port;
          Tcp_Util.Accept_From (Socket.Tcp, Port, Accept_Cb'Access,
                                Acc.Dscr, Acc.Port);
          Acc.Status := Accepting;
        when Connected =>
          -- Close and delete
          Acc.Dscr.Close;
          Client_List.Delete (Moved => Moved);
          exit when not Moved;
      end case;
      exit when not Client_List.Check_Move;
      Client_List.Move_To;
    end loop;
  end Disconnect_All;

  -- Send data to a client
  -- If error then disconnect and send a Disconnect to partner
  function My_Send is new Tcp_Util.Send (Common.Data_Type);
  procedure Send (Port : in Common.Port_Num;
                  Local : Boolean;
                  Len : in Natural;
                  Data : in Common.Data_Type) is
    Acc : Client_Access;
    Res : Boolean;
    pragma Unreferenced (Res);
    Msg : Partner.Message;
  begin
    if Local then
      Acc := Accept_Port_Map(Port);
    else
      Acc := Connect_Port_Map(Port);
    end if;
    Res := My_Send (Acc.Dscr, null, null, 0.1, Data, Len);
  exception
    when Tcp_Util.Timeout_Error | Socket.Soc_Conn_Lost =>
      if Debug.Is_Set then
        Basic_Proc.Put_Output ("Tcpipe: sending failure to ");
        if Local then
          Basic_Proc.Put_Output ("local ");
        else
          Basic_Proc.Put_Output ("remote ");
        end if;
        Basic_Proc.Put_Line_Output ("client  port " & Ip_Addr.Image (Port));
      end if;
      -- On error disconnect client and send to partner
      Disconnect (Port, Local);
      Msg.Head.Kind := Partner.Disconnect;
      Msg.Head.Local := not Local;
      Msg.Head.Port := Port;
      Partner.Send (Msg, 0);
  end Send;

  -- Set reception callback on connection from/to client
  procedure Set_Callbacks (Dscr : in Socket.Socket_Dscr;
                           Local : in Boolean) is
  begin
    -- @@@
    null;
  end Set_Callbacks;
end Clients;

