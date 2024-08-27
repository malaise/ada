with Ada.Exceptions;
with Basic_Proc, Address_Ops, Ip_Addr, Socket, Socket_Util, Tcp_Util;
with Debug, Clients;
package body Partner is

  -- Header size (offset of data)
  Dummy_Msg : Message;
  Head_Len : constant Natural
           := Natural (
                Address_Ops."-" (Dummy_Msg.Data'Address, Dummy_Msg'Address));

  -- Message reception
  procedure Disconnection_Cb (Unused_Dscr : in Socket.Socket_Dscr);
  package My_Reception is new Tcp_Util.Reception (Message);
  function Reception_Cb (Unused_Dscr : Socket.Socket_Dscr;
                         Msg  : Message;
                         Len  : Natural) return Boolean;
  procedure Set_Callbacks;

  -- Init connection with partner
  -- Try to reconnect infinitely
  -- When receiving a service message handle it
  -- when receiveg a data emssage relay it
  -- Invalid_Addr : exception;
  Client_Mode : Boolean;
  Loc_Port: Socket_Util.Local_Port (Socket_Util.Port_Num_Spec);
  Rem_Dscr : Socket.Socket_Dscr;
  Rem_Host : Socket_Util.Remote_Host (Socket_Util.Host_Id_Spec);
  Rem_Port: Socket_Util.Remote_Port (Socket_Util.Port_Num_Spec);

  -- Connect and Accept Callbacks
  procedure Connect_Cb (Remote_Host_Id  : in Socket_Util.Host_Id;
                        Remote_Port_Num : in Socket_Util.Port_Num;
                        Connected       : in Boolean;
                        Dscr            : in Socket.Socket_Dscr);
  procedure Accept_Cb (Local_Port_Num    : in Socket_Util.Port_Num;
                       Unused_Local_Dscr : in Socket.Socket_Dscr;
                       Remote_Host_Id    : in Socket_Util.Host_Id;
                       Remote_Port_Num : in Socket_Util.Port_Num;
                       New_Dscr        : in Socket.Socket_Dscr);
  -- Connect/Accept
  procedure Connect_Accept is
    Dummy_Res : Boolean;
    Port: Socket_Util.Port_Num;
    Dscr : Socket.Socket_Dscr;
  begin
    if Client_Mode then
      Debug.Logger.Log_Debug ("Connecting to remote host: "
          & Ip_Addr.Image (Rem_Host.Id, Rem_Port.Num)
          & " from port " & Ip_Addr.Image (Loc_Port.Num) );
      Dummy_Res := Tcp_Util.Connect_To (Socket.Tcp_Header, Rem_Host, Rem_Port,
                                     Connect_Cb'Access,
                                     Nb_Tries => 0,
                                     Local_Port => Loc_Port);
    else
      Debug.Logger.Log_Debug ("Accepting on port: "
          & Ip_Addr.Image (Loc_Port.Num));
      Tcp_Util.Accept_From (Socket.Tcp_Header, Loc_Port, Accept_Cb'Access,
                            Dscr, Port);
    end if;
  end Connect_Accept;


  procedure Init (Client : in Boolean; Addr : in String) is
    Host_Rec : Socket_Util.Remote_Host;
    Port_Rec : Socket_Util.Remote_Port;
    Loc_Port_Rec : Socket_Util.Local_Port;
    Port_Num : Socket.Port_Num;
    use type Socket_Util.Remote_Host_List, Socket_Util.Remote_Port_List;
  begin
    -- Parse Addr and resolve
    Client_Mode := Client;
    if Client then
      -- Client
      Ip_Addr.Parse (Addr, Loc_Port_Rec, Host_Rec, Port_Rec);
      if Host_Rec.Kind = Socket_Util.Host_Name_Spec then
        begin
          Rem_Host.Id := Socket.Host_Id_Of (Host_Rec.Name.Image);
        exception
          when Socket.Soc_Name_Not_Found =>
            Basic_Proc.Put_Line_Error ("Unknown host name "
                                     & Host_Rec.Name.Image & ".");
            raise Invalid_Addr;
        end;
      else
        Rem_Host.Id := Host_Rec.Id;
      end if;

      -- Optional local port for client
      if Loc_Port_Rec.Kind = Socket_Util.Port_Name_Spec then
        if Loc_Port_Rec.Name.Is_Null then
          Loc_Port.Num := 0;
        else
          begin
            Loc_Port.Num := Socket.Port_Num_Of (Loc_Port_Rec.Name.Image,
                                                Socket.Tcp);
          exception
            when Socket.Soc_Name_Not_Found =>
              Basic_Proc.Put_Line_Error ("Unknown port name "
                                       & Loc_Port_Rec.Name.Image & ".");
              raise Invalid_Addr;
          end;
        end if;
      else
        Loc_Port.Num := Loc_Port_Rec.Num;
      end if;

    else
      -- Server
      Port_Rec := Ip_Addr.Parse (Addr);
    end if;

    -- Client remote port and server local port
    if Port_Rec.Kind = Socket_Util.Port_Name_Spec then
      begin
        Port_Num := Socket.Port_Num_Of (Port_Rec.Name.Image, Socket.Tcp);
      exception
        when Socket.Soc_Name_Not_Found =>
          Basic_Proc.Put_Line_Error ("Unknown port name "
                                   & Port_Rec.Name.Image & ".");
          raise Invalid_Addr;
      end;
    else
      Port_Num := Port_Rec.Num;
    end if;
    if Client then
      Rem_Port.Num := Port_Num;
    else
      Loc_Port.Num := Port_Num;
    end if;

    -- Connect or accept
    Connect_Accept;
  exception
    when Ip_Addr.Parse_Error =>
      raise Invalid_Addr;
  end Init;

  function Is_Connected return Boolean is (Rem_Dscr.Is_Open);

  -- Close connection or cancel attempts
  -- No error
  procedure Close is
  begin
    if Rem_Dscr.Is_Open then
      Debug.Logger.Log_Debug ("Closing connection with partner");
      My_Reception.Remove_Callbacks (Rem_Dscr);
      Rem_Dscr.Close;
    else
      Debug.Logger.Log_Debug ("Cancelling connection with partner");
      if Client_Mode then
        Tcp_Util.Abort_Connect (Rem_Host, Rem_Port);
      else
        Tcp_Util.Abort_Accept (Socket.Tcp_Header, Loc_Port.Num);
      end if;
    end if;
  exception
    when Tcp_Util.No_Such =>
      Basic_Proc.Put_Line_Error ("ERROR: Exception Tcp_Util.So_Such on Close");
  end Close;

  -- Connect Callback
  procedure Connect_Cb (Remote_Host_Id  : in Socket_Util.Host_Id;
                        Remote_Port_Num : in Socket_Util.Port_Num;
                        Connected       : in Boolean;
                        Dscr            : in Socket.Socket_Dscr) is
  begin
    Rem_Host.Id := Remote_Host_Id;
    Rem_Port.Num := Remote_Port_Num;
    Rem_Dscr := Dscr;
    Set_Callbacks;
    if Connected then
      Debug.Logger.Log_Debug ("Connected to remote host: "
          & Ip_Addr.Image (Rem_Host.Id, Rem_Port.Num));
    else
      Debug.Logger.Log_Fatal (
          "Unexpected failure of connection to remote host: "
        & Ip_Addr.Image (Rem_Host.Id, Rem_Port.Num));
      raise Common.Fatal_Error;
    end if;
  end Connect_Cb;

  -- Accept Callback
  procedure Accept_Cb (Local_Port_Num    : in Socket_Util.Port_Num;
                       Unused_Local_Dscr : in Socket.Socket_Dscr;
                       Remote_Host_Id    : in Socket_Util.Host_Id;
                       Remote_Port_Num   : in Socket_Util.Port_Num;
                       New_Dscr          : in Socket.Socket_Dscr) is
  begin
    -- Stop accepting
    Tcp_Util.Abort_Accept (Socket.Tcp_Header, Local_Port_Num);
    -- Receive from partner
    Rem_Host.Id := Remote_Host_Id;
    Rem_Port.Num := Remote_Port_Num;
    Rem_Dscr := New_Dscr;
    Set_Callbacks;
    Debug.Logger.Log_Debug ("Accepted connection from remote host: "
                          & Ip_Addr.Image (Rem_Host.Id, Rem_Port.Num));
  end Accept_Cb;

  -- Close and restart
  procedure Close_Restart (On_Send : in Boolean) is
  begin
    -- The Dscr is always already closed and reception callbacks are already
    --  removed by My_Reception
    Rem_Dscr := Socket.No_Socket;
    if On_Send then
      -- Error on send does not close Dscr nor remove reception CBs
      Rem_Dscr.Close;
      My_Reception.Remove_Callbacks (Rem_Dscr);
    else
      -- The Dscr is already closed and reception CBs are already
      --  removed by My_Reception
      Rem_Dscr := Socket.No_Socket;
    end if;
    -- Reconnect
    Connect_Accept;
    -- Disconnect all local client
    Clients.Disconnect_All;
  end Close_Restart;

  -- Send connection/disconnection/data
  -- If sending error then
  --  close to partner and reconnect/accept to partner
  -- close to client,
  function My_Send is new Tcp_Util.Send (Message);
  procedure Send (Msg : in Message; Len : in Natural) is
    Dummy_Res : Boolean;
  begin
    Dummy_Res := My_Send (Rem_Dscr, null, null, 0.2, Msg, Len + Head_Len);
  exception
    when Error: Socket.Soc_Conn_Lost | Tcp_Util.Timeout_Error =>
      Debug.Logger.Log_Debug ("Exception "
            & Ada.Exceptions.Exception_Name(Error)
            & " when sending to remote host: "
            & Ip_Addr.Image (Rem_Host.Id, Rem_Port.Num));
      Close_Restart (True);
  end Send;

  -- Message reception
  procedure Disconnection_Cb (Unused_Dscr : in Socket.Socket_Dscr) is
  begin
     Debug.Logger.Log_Debug ("Tcpipe: disconnection from remote host: "
                           & Ip_Addr.Image (Rem_Host.Id, Rem_Port.Num));
    Close_Restart (False);
  end Disconnection_Cb;

  procedure Set_Callbacks is
  begin
    My_Reception.Set_Callbacks (Rem_Dscr, Reception_Cb'Access,
                                Disconnection_Cb'Access);
  end Set_Callbacks;

  function Reception_Cb (Unused_Dscr : Socket.Socket_Dscr;
                         Msg  : Message;
                         Len  : Natural) return Boolean is
  begin
    case Msg.Head.Kind is
      when Connect =>
         Debug.Logger.Log_Debug ("Receive a connect request to port: "
                               & Ip_Addr.Image (Msg.Head.Port));
        Clients.Connect_Client (Msg.Head.Port);
      when Disconnect =>
         Debug.Logger.Log_Debug ("Receive a disconnect "
              & (if Msg.Head.Local then "local" else "remote")
              & " request to port: "
              & Ip_Addr.Image (Msg.Head.Port));
        Clients.Disconnect (Msg.Head.Port, Msg.Head.Local);
      when Data =>
        Clients.Send (Msg.Head.Port, Msg.Head.Local, Len - Head_Len, Msg.Data);
    end case;
    return False;
  end Reception_Cb;

end Partner;

