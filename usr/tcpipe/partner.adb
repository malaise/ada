with Ada.Exceptions;
with Basic_Proc, Address_Ops, Ip_Addr, Socket, Tcp_Util;
with Debug, Clients;
package body Partner is

  -- Header size (offset of data)
  Dummy_Msg : Message;
  Head_Len : constant Natural
           := Natural (
                Address_Ops."-" (Dummy_Msg.Data'Address, Dummy_Msg'Address));

  -- Message reception
  procedure Disconnection_Cb (Dscr : in Socket.Socket_Dscr);
  package My_Reception is new Tcp_Util.Reception (Message);
  function Reception_Cb (Dscr : Socket.Socket_Dscr;
                         Msg  : Message;
                         Len  : Natural) return Boolean;
  procedure Set_Callbacks;

  -- Init connection with partner
  -- Try to reconnect infinitely
  -- When receiving a service message handle it
  -- when receiveg a data emssage relay it
  -- Invalid_Addr : exception;
  Client_Mode : Boolean;
  Loc_Port: Tcp_Util.Local_Port (Tcp_Util.Port_Num_Spec);
  Rem_Dscr : Socket.Socket_Dscr;
  Rem_Host : Tcp_Util.Remote_Host (Tcp_Util.Host_Id_Spec);
  Rem_Port: Tcp_Util.Remote_Port (Tcp_Util.Port_Num_Spec);

  -- Connect and Accept Callbacks
  procedure Connect_Cb (Remote_Host_Id  : in Tcp_Util.Host_Id;
                        Remote_Port_Num : in Tcp_Util.Port_Num;
                        Connected       : in Boolean;
                        Dscr            : in Socket.Socket_Dscr);
  procedure Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                       Local_Dscr      : in Socket.Socket_Dscr;
                       Remote_Host_Id  : in Tcp_Util.Host_Id;
                       Remote_Port_Num : in Tcp_Util.Port_Num;
                       New_Dscr        : in Socket.Socket_Dscr);
  -- Connect/Accept
  procedure Connect_Accept is
    Result : Boolean;
    pragma Unreferenced (Result);
    Port: Tcp_Util.Port_Num;
    Dscr : Socket.Socket_Dscr;
  begin
    if Client_Mode then
      if Debug.Is_Set then
        Basic_Proc.Put_Line_Output ("Tcpipe: connecting to remote"
         & " host: " & Ip_Addr.Image (Socket.Id2Addr (Rem_Host.Id))
         & " port: " & Ip_Addr.Image (Rem_Port.Num));
      end if;
      Result := Tcp_Util.Connect_To (Socket.Tcp_Header, Rem_Host, Rem_Port,
                                     Connect_Cb'Access, Nb_Tries => 0);
    else
      if Debug.Is_Set then
        Basic_Proc.Put_Line_Output ("Tcpipe: accepting on"
         & " port: " & Ip_Addr.Image (Loc_Port.Num));
      end if;
      Tcp_Util.Accept_From (Socket.Tcp_Header, Loc_Port, Accept_Cb'Access,
                            Dscr, Port);
    end if;
  end Connect_Accept;


  procedure Init (Client : in Boolean; Addr : in String) is
    Host_Rec : Tcp_Util.Remote_Host;
    Port_Rec : Tcp_Util.Remote_Port;
    Host: Tcp_Util.Host_Id;
    Port: Tcp_Util.Port_Num;
    use type Tcp_Util.Remote_Host_List, Tcp_Util.Remote_Port_List;
  begin
    -- Parse Addr and resolve
    Client_Mode := Client;
    if Client then
      Ip_Addr.Parse (Addr, Host_Rec, Port_Rec);
      if Host_Rec.Kind = Tcp_Util.Host_Name_Spec then
        begin
          Host := Socket.Host_Id_Of (Host_Rec.Name.Image);
        exception
          when Socket.Soc_Name_Not_Found =>
            Basic_Proc.Put_Line_Error ("Unknown host name "
                                     & Host_Rec.Name.Image & ".");
            raise Invalid_Addr;
        end;
      else
        Host := Host_Rec.Id;
      end if;
      Rem_Host.Id := Host;
    else
      Port_Rec := Ip_Addr.Parse (Addr);
    end if;
    if Port_Rec.Kind = Tcp_Util.Port_Name_Spec then
      begin
        Port := Socket.Port_Num_Of (Port_Rec.Name.Image, Socket.Tcp);
      exception
        when Socket.Soc_Name_Not_Found =>
          Basic_Proc.Put_Line_Error ("Unknown port name "
                                   & Port_Rec.Name.Image & ".");
          raise Invalid_Addr;
      end;
    else
      Port := Port_Rec.Num;
    end if;
    Loc_Port.Num := Port;
    Rem_Port.Num := Port;

    -- Connect or accept
    Connect_Accept;
  exception
    when Ip_Addr.Parse_Error =>
      raise Invalid_Addr;
  end Init;

  function Is_Connected return Boolean is
  begin
    return Rem_Dscr.Is_Open;
  end Is_Connected;

  -- Close connection or cancel attempts
  -- No error
  procedure Close is
  begin
    if Rem_Dscr.Is_Open then
      if Debug.Is_Set then
        Basic_Proc.Put_Line_Output (
          "Tcpipe: closing connection with partner");
      end if;
      My_Reception.Remove_Callbacks (Rem_Dscr);
      Rem_Dscr.Close;
    else
      if Debug.Is_Set then
        Basic_Proc.Put_Line_Output (
          "Tcpipe: cancelling connection with partner");
      end if;
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
  procedure Connect_Cb (Remote_Host_Id  : in Tcp_Util.Host_Id;
                        Remote_Port_Num : in Tcp_Util.Port_Num;
                        Connected       : in Boolean;
                        Dscr            : in Socket.Socket_Dscr) is
  begin
    Rem_Host.Id := Remote_Host_Id;
    Rem_Port.Num := Remote_Port_Num;
    Rem_Dscr := Dscr;
    Set_Callbacks;
    if Connected then
      if Debug.Is_Set then
        Basic_Proc.Put_Line_Output ("Tcpipe: connected to remote"
         & " host: " & Ip_Addr.Image (Socket.Id2Addr (Rem_Host.Id))
         & " port: " & Ip_Addr.Image (Rem_Port.Num));
      end if;
    else
      Basic_Proc.Put_Line_Error (
         "ERROR: Unexpected failure of connection to remote"
         & " host: " & Ip_Addr.Image (Socket.Id2Addr(Rem_Host.Id))
         & " port: " & Ip_Addr.Image (Rem_Port.Num));
      raise Common.Fatal_Error;
    end if;
  end Connect_Cb;

  -- Accept Callback
  procedure Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                       Local_Dscr      : in Socket.Socket_Dscr;
                       Remote_Host_Id  : in Tcp_Util.Host_Id;
                       Remote_Port_Num : in Tcp_Util.Port_Num;
                       New_Dscr        : in Socket.Socket_Dscr) is
    pragma Unreferenced (Local_Dscr);
  begin
    -- Stop accepting
    Tcp_Util.Abort_Accept (Socket.Tcp_Header, Local_Port_Num);
    -- Receive from partner
    Rem_Host.Id := Remote_Host_Id;
    Rem_Port.Num := Remote_Port_Num;
    Rem_Dscr := New_Dscr;
    Set_Callbacks;
    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Tcpipe: accepted connection from remote"
       & " host: " & Ip_Addr.Image (Socket.Id2Addr(Rem_Host.Id))
       & " port: " & Ip_Addr.Image (Rem_Port.Num));
    end if;
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
    Result : Boolean;
    pragma Unreferenced (Result);
  begin
    Result := My_Send (Rem_Dscr, null, null, 0.2, Msg, Len + Head_Len);
  exception
    when Error: Socket.Soc_Conn_Lost | Tcp_Util.Timeout_Error =>
      if Debug.Is_Set then
        Basic_Proc.Put_Line_Output ("Tcpipe: Exception "
         & Ada.Exceptions.Exception_Name(Error) & " when sending to remote"
         & " host: " & Ip_Addr.Image (Socket.Id2Addr(Rem_Host.Id))
         & " port: " & Ip_Addr.Image (Rem_Port.Num));
      end if;
      Close_Restart (True);
  end Send;

  -- Message reception
  procedure Disconnection_Cb (Dscr : in Socket.Socket_Dscr) is
    pragma Unreferenced (Dscr);
  begin
    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Tcpipe: disconnection from remote"
       & " host: " & Ip_Addr.Image (Socket.Id2Addr(Rem_Host.Id))
       & " port: " & Ip_Addr.Image (Rem_Port.Num));
    end if;
    Close_Restart (False);
  end Disconnection_Cb;

  procedure Set_Callbacks is
  begin
    My_Reception.Set_Callbacks (Rem_Dscr, Reception_Cb'Access,
                                Disconnection_Cb'Access);
  end Set_Callbacks;

  function Reception_Cb (Dscr : Socket.Socket_Dscr;
                         Msg  : Message;
                         Len  : Natural) return Boolean is
    pragma Unreferenced (Dscr);
  begin
    case Msg.Head.Kind is
      when Connect =>
        if Debug.Is_Set then
          Basic_Proc.Put_Line_Output ("Tcpipe: Receive a connect request to"
            & " port: " & Ip_Addr.Image (Msg.Head.Port));
        end if;
        Clients.Connect_Client (Msg.Head.Port);
      when Disconnect =>
        if Debug.Is_Set then
          Basic_Proc.Put_Output ("Tcpipe: Receive a disconnect ");
          if Msg.Head.Local then
            Basic_Proc.Put_Output ("local ");
          else
            Basic_Proc.Put_Output ("remote ");
          end if;
          Basic_Proc.Put_Line_Output ("request to "
            & "port: " & Ip_Addr.Image (Msg.Head.Port));
        end if;
        Clients.Disconnect (Msg.Head.Port, Msg.Head.Local);
      when Data =>
        Clients.Send (Msg.Head.Port, Msg.Head.Local, Len - Head_Len, Msg.Data);
    end case;
    return False;
  end Reception_Cb;

end Partner;

