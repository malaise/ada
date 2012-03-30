with Basic_Proc, Socket, Tcp_Util, Ip_Addr, Hashed_List.Unique;
with Debug, Partner;
package body Clients is

  -- Descriptor of a client connection
  type Client_Rec is record
    -- The port associted to the client
    Port : Common.Port_Num;
    -- Local when accepted, otherwise connected
    Local : Boolean;
    -- When accepting: the accept Dscr, when accepted or connected:
    --  the connection Dscr
    Dscr : Socket.Socket_Dscr;
  end record;
  type Client_Access is access all Client_Rec;

  -- List of clients (accepting/accepted/connected), hashed on Dscr
  procedure Set (To : out Client_Rec; Val : in Client_Rec) is
  begin
    To := Val;
  end Set;
  function "=" (Current : Client_Rec; Criteria : Client_Rec) return Boolean is
  begin
    return Current.Dscr.Image = Criteria.Dscr.Image;
  end "=";
  function Image (Element : Client_Rec) return String is
  begin
    return Element.Dscr.Image;
  end Image;

  package Client_List_Mng is new Hashed_List (Client_Rec, Client_Access,
                                               Set, "=", Image);
  package Client_Mng is new Client_List_Mng.Unique;
  Client_List : Client_Mng.Unique_List_Type;

  -- Maps: Port -> Client Dscr
  Acceptings : array (Socket.Port_Num) of Socket.Socket_Dscr;
  Connecteds : array (Boolean, Socket.Port_Num) of Socket.Socket_Dscr;

  -- Set reception callback on connection from/to client
  package My_Reception is new Tcp_Util.Reception (Common.Data_Type);
  procedure Set_Callbacks (Dscr : in Socket.Socket_Dscr;
                           Local : in Boolean);

  -- Accept callback: Canncel accept, set connected and set reception callbacks
  procedure Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                       Local_Dscr      : in Socket.Socket_Dscr;
                       Remote_Host_Id  : in Tcp_Util.Host_Id;
                       Remote_Port_Num : in Tcp_Util.Port_Num;
                       New_Dscr        : in Socket.Socket_Dscr) is
    pragma Unreferenced (Local_Dscr);
    Msg : Partner.Message;
    Client : Client_Rec;
    Loc_Dscr : Socket.Socket_Dscr;
    use type Socket.Socket_Dscr;
  begin
    -- Accept only if partner is connected
    if not Partner.Is_Connected then
      Loc_Dscr := New_Dscr;
      Loc_Dscr.Close;
      return;
    end if;
    -- Cancel Accept
    Tcp_Util.Abort_Accept (Socket.Tcp, Local_Port_Num);
    Acceptings(Local_Port_Num) := Socket.No_Socket;
    -- Update map, insert record and set reception callbacks
    Connecteds(True, Local_Port_Num) := New_Dscr;
    Client.Port := Local_Port_Num;
    Client.Local := True;
    Client.Dscr := New_Dscr;
    Client_List.Insert (Client);
    Set_Callbacks (New_Dscr, True);

    -- Send symetric connect message
    Msg.Head.Kind := Partner.Connect;
    Msg.Head.Local := False;
    Msg.Head.Port := Local_Port_Num;
    Partner.Send (Msg, 0);

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
    Dscr : Socket.Socket_Dscr;
    Port_Num : Tcp_Util.Port_Num;
    Loc_Port : Tcp_Util.Local_Port;
    use type Tcp_Util.Local_Port_List;
  begin
    -- Get port num
    Loc_Port := Ip_Addr.Parse (Port);
    if Loc_Port.Kind = Tcp_Util.Port_Name_Spec then
      begin
        Port_Num := Socket.Port_Num_Of (Loc_Port.Name.Image, Socket.Tcp);
      exception
        when Socket.Soc_Name_Not_Found =>
          Basic_Proc.Put_Line_Error ("Unknown port name "
                                   & Loc_Port.Name.Image & ".");
          raise Invalid_Port;
      end;
    else
      Port_Num := Loc_Port.Num;
    end if;

    -- Check unicity
    if Acceptings (Port_Num).Is_Open then
      Basic_Proc.Put_Line_Error ("Port num " & Ip_Addr.Image (Port_Num)
                                 & " already used.");
      raise Invalid_Port;
    end if;

    -- Accept
    Tcp_Util.Accept_From (Socket.Tcp, Loc_Port, Accept_Cb'Access,
                          Dscr, Port_Num);

    -- Insert accepting Dscr in map
    Acceptings(Port_Num) := Dscr;

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
    if Connecteds(False, Port).Is_Open then
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
      -- Insert record and set reception callbacks
      Client.Port := Port;
      Client.Local := False;
      Client_List.Insert (Client);
      Connecteds(False, Port) := Client.Dscr;
      Set_Callbacks (Client.Dscr, False);
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
  procedure Close_Accept (Port : in Common.Port_Num; Local : in Boolean) is
    Loc_Port : Tcp_Util.Local_Port (Tcp_Util.Port_Num_Spec);
  begin
    -- See if this Port is accepted/connected
    if not Connecteds(Local, Port).Is_Open then
      return;
    end if;
    -- Close if needed
    if My_Reception.Callbacks_Set (Connecteds(Local, Port)) then
      -- If send error then callbacks must be removed then Dscr must be closed
      My_Reception.Remove_Callbacks (Connecteds(Local, Port));
      Connecteds(Local, Port).Close;
    else
      -- If client quit then callbacks are already removed and Dscr is closed
      Connecteds(Local, Port) := Socket.No_Socket;
    end if;
    if Local then
      -- Accept again
      Loc_Port.Num := Port;
      Tcp_Util.Accept_From (Socket.Tcp, Loc_Port, Accept_Cb'Access,
                            Acceptings(Port), Loc_Port.Num);
    end if;
  end Close_Accept;

  -- Disconnect (re-accept) and remove entry
  procedure Disconnect (Port : in Common.Port_Num; Local : in Boolean) is
    Client : Client_Rec;
  begin
    -- See if this Port is accepted/connected
    if not Connecteds(Local, Port).Is_Open then
      return;
    end if;
    -- Save Dscr and Close
    Client.Dscr := Connecteds(Local, Port);
    Close_Accept (Port, Local);
    -- Delete entry
    Client_List.Delete (Client);
  exception
    when Client_Mng.Not_In_List =>
      if Local then
        Basic_Proc.Put_Error ("ERROR: Connected ");
      else
        Basic_Proc.Put_Error ("ERROR: Accepted ");
      end if;
      Basic_Proc.Put_Line_Error ("port " & Ip_Addr.Image (Port)
                                 & " not found.");
      raise Common.Fatal_Error;
  end Disconnect;

  -- Disconnect all clients
  -- If local then start accepting again
  procedure Disconnect_All is
    Client : Client_Rec;
    Moved : Boolean;
  begin
    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Tcpipe: disconnecting all clients");
    end if;
    if Client_List.Is_Empty then
      return;
    end if;
    -- Close and accept accepted, close connected
    Client_List.Rewind;
    loop
      Client_List.Read_Next (Client, Moved);
      -- Close
      Close_Accept (Client.Port, Client.Local);
      exit when not Moved;
    end loop;
    -- Delete whole list
    Client_List.Delete_List;
  end Disconnect_All;

  -- Send data to a client
  -- If error then disconnect and send a Disconnect to partner
  function My_Send is new Tcp_Util.Send (Common.Data_Type);
  procedure Send (Port : in Common.Port_Num;
                  Local : Boolean;
                  Len : in Natural;
                  Data : in Common.Data_Type) is
    Dscr : Socket.Socket_Dscr;
    Res : Boolean;
    pragma Unreferenced (Res);
    Msg : Partner.Message;
  begin
    Dscr := Connecteds(Local, Port);
    if not Dscr.Is_Open then
      Basic_Proc.Put_Error ("ERROR: Sending port not ");
      if Local then
        Basic_Proc.Put_Error ("accepted ");
      else
        Basic_Proc.Put_Error ("connected ");
      end if;
      Basic_Proc.Put_Line_Error (Ip_Addr.Image (Port) & ".");
      raise Common.Fatal_Error;
    end if;
    Res := My_Send (Dscr, null, null, 0.1, Data, Len);
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
      -- On error disconnect client and send to partner a symetric disconnect
      Disconnect (Port, Local);
      Msg.Head.Kind := Partner.Disconnect;
      Msg.Head.Local := not Local;
      Msg.Head.Port := Port;
      Partner.Send (Msg, 0);
  end Send;

  -- Disconnection callbacks
  procedure Disconnect (Dscr : in Socket.Socket_Dscr; Local : in Boolean) is
    Client : Client_Rec;
    Msg : Partner.Message;
  begin
    -- Search client by Dscr
    Client.Dscr := Dscr;
    Client_List.Read (Client);
    -- Disconnect client and send to partner a symetric disconnect
    Disconnect (Client.Port, Local);
    Msg.Head.Kind := Partner.Disconnect;
    Msg.Head.Local := not Local;
    Msg.Head.Port := Client.Port;
    Partner.Send (Msg, 0);
  exception
    when Client_List_Mng.Not_In_List =>
      Basic_Proc.Put_Error ("ERROR: ");
      if Local then
        Basic_Proc.Put_Error ("accepted ");
      else
        Basic_Proc.Put_Error ("connected ");
      end if;
      Basic_Proc.Put_Line_Error ("client not found on disconnection.");
      raise Common.Fatal_Error;
  end Disconnect;
  procedure Local_Disconnect (Dscr : in Socket.Socket_Dscr) is
  begin
    Disconnect (Dscr, True);
  end Local_Disconnect;
  procedure Remote_Disconnect (Dscr : in Socket.Socket_Dscr) is
  begin
    Disconnect (Dscr, False);
  end Remote_Disconnect;

  -- Reception callbacks
  function Receive (Dscr    : Socket.Socket_Dscr;
                    Local   : in Boolean;
                    Message : Common.Data_Type;
                    Length  : Natural) return Boolean is
    Msg : Partner.Message;
    Client : Client_Rec;
  begin
    -- Search client by Dscr
    Client.Dscr := Dscr;
    Client_List.Read (Client);
    -- Send data
    Msg.Head.Kind := Partner.Data;
    Msg.Head.Local := not Local;
    Msg.Head.Port := Client.Port;
    Msg.Data := Message;
    Partner.Send (Msg, Length);
    return False;
  exception
    when Client_List_Mng.Not_In_List =>
      Basic_Proc.Put_Error ("ERROR: ");
      if Local then
        Basic_Proc.Put_Error ("accepted ");
      else
        Basic_Proc.Put_Error ("connected ");
      end if;
      Basic_Proc.Put_Line_Error ("client not found for sending.");
      raise Common.Fatal_Error;
  end Receive;
  function Local_Receive (Dscr    : Socket.Socket_Dscr;
                          Message : Common.Data_Type;
                          Length  : Natural) return Boolean is
  begin
    return Receive (Dscr, True, Message, Length);
  end Local_Receive;
  function Remote_Receive (Dscr    : Socket.Socket_Dscr;
                          Message : Common.Data_Type;
                          Length  : Natural) return Boolean is
  begin
    return Receive (Dscr, False, Message, Length);
  end Remote_Receive;

  -- Set reception callback on connection from/to client
  procedure Set_Callbacks (Dscr : in Socket.Socket_Dscr;
                           Local : in Boolean) is
  begin
    if Local then
      My_Reception.Set_Callbacks (Dscr, Local_Receive'Access,
                                        Local_Disconnect'Access);
    else
      My_Reception.Set_Callbacks (Dscr, Remote_Receive'Access,
                                        Remote_Disconnect'Access);
    end if;
  end Set_Callbacks;

end Clients;

