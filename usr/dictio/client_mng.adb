with Socket, Tcp_Util, Dynamic_List, Event_Mng;
with Args, Parse, Notify, Client_Fd, Client_Com, Debug, Intra_Dictio,
     Sync_Mng, Versions, Status;
package body Client_Mng is


  type State_List is (Not_Init, Waiting, Allow);
  State : State_List := Not_Init;

  Dictio_Status : Status.Stable_Status_List := Status.Dead;
  procedure Send_Status (Dscr : in Socket.Socket_Dscr);

  Accept_Port : Tcp_Util.Port_Num;

  function Read_Cb (Fd : in Event_Mng.File_Desc; Read : in Boolean)
                   return Boolean is
    Dscr : Socket.Socket_Dscr;
    Msg : Client_Com.Dictio_Client_Rec;
    Len : Natural;
    Send_Res : Boolean;
    use type Socket.Socket_Dscr;
  begin
    Dscr := Client_Fd.Socket_Of (Fd);
    if Dscr = Socket.No_Socket then
      if Debug.Level_Array(Debug.Client) then
        Debug.Put ("Client: ERROR unknown fd " & Fd'Img);
      end if;
      return False;
    end if;
    begin
      Client_Com.Dictio_Receive (Dscr, Msg, Len);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
        if Debug.Level_Array(Debug.Client) then
          Debug.Put ("Client: disconnection of " & Fd'Img);
        end if;
        Notify.Del_Client (Dscr);
        Client_Fd.Del_Client (Dscr);
        return False;
      when Socket.Soc_Len_Err =>
        if Debug.Level_Array(Debug.Client) then
          Debug.Put ("Client: invalid size from " & Fd'Img);
        end if;
        Notify.Del_Client (Dscr);
        Client_Fd.Del_Client (Dscr);
        return False;
    end;
    if Debug.Level_Array(Debug.Client) then
      Debug.Put ("Client: request " & Msg.Action'Img & " >"
               & Parse (Msg.Item.Name) & "<");
    end if;
    case Msg.Action is
      when Client_Com.Version =>
        if Parse (Msg.Item.Name) /= Versions.Lib then
          if Debug.Level_Array(Debug.Client) then
            Debug.Put ("Client: received invalid version: "
                     & Parse (Msg.Item.Name)
                     & " being: " & Versions.Lib);
          end if;
          Notify.Del_Client (Dscr);
          Client_Fd.Del_Client (Dscr);
        end if;
        return False;
      when Client_Com.State =>
        if Debug.Level_Array(Debug.Client) then
          Debug.Put ("Client: received status. "
                   & Parse (Msg.Item.Name));
        end if;
        Notify.Del_Client (Dscr);
        Client_Fd.Del_Client (Dscr);
        return False;
      when Client_Com.Read =>
        Data_Base.Get (Msg.Item.Name, Msg.Item);
        begin
          Send_Res := Client_Com.Dictio_Send (Dscr, null, Msg);
          if Debug.Level_Array(Debug.Client) then
            Debug.Put ("Client: read reply result " & Send_Res'Img);
          end if;
        exception
          when Socket.Soc_Tail_Err =>
            if Debug.Level_Array(Debug.Client) then
              Debug.Put ("Client: read lost cause client in overflow");
            end if;
            Notify.Del_Client (Dscr);
            Client_Fd.Del_Client (Dscr);
            return False;
        when Socket.Soc_Conn_Lost =>
          if Debug.Level_Array(Debug.Client) then
            Debug.Put ("Client: lost connection with " & Fd'Img);
          end if;
          Notify.Del_Client (Dscr);
          Client_Fd.Del_Client (Dscr);
          return False;
        end;
      when Client_Com.Write =>
        Modified (Intra_Dictio.Data_Kind, Msg.Item);
        Intra_Dictio.Send_Data (Msg.Item);
      when Client_Com.Notif_On =>
        Notify.Add (Dscr, Msg.Item.Name);
      when Client_Com.Notif_Off =>
        Notify.Del (Dscr, Msg.Item.Name);
    end case;
    if Debug.Level_Array(Debug.Client) then
      Debug.Put ("Client: request done");
    end if;
    return False;
  end Read_Cb;
  
  procedure Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                       Local_Dscr      : in Socket.Socket_Dscr;
                       Remote_Port_Num : in Tcp_Util.Port_Num;
                       Remote_Host_Id  : in Tcp_Util.Host_Id;
                       New_Dscr        : in Socket.Socket_Dscr) is
    use type Tcp_Util.Port_Num;
  begin
    if Local_Port_Num /= Accept_Port then
      if Debug.Level_Array(Debug.Client) then
        Debug.Put ("Client: ERROR unexpected accept");
      end if;
      declare
        Close_Dscr : Socket.Socket_Dscr := New_Dscr;
      begin
        Socket.Close (Close_Dscr);
      end;
      return;
    end if;
    if Debug.Level_Array(Debug.Client) then
      Debug.Put ("Client: new client accepted -> "
               & Event_Mng.File_Desc'Image(Socket.Fd_Of (New_Dscr)));
    end if;
    Client_Fd.Add_Client (New_Dscr);
    Event_Mng.Add_Fd_Callback (Socket.Fd_Of (New_Dscr), True, Read_Cb'access);

    -- Send version and status
    declare
      Msg : Client_Com.Dictio_Client_Rec;
      Dummy : Boolean;
    begin
      -- Send version then status
      Msg.Action := Client_Com.Version;
      Msg.Item.Name := (others => ' ');
      Msg.Item.Name(1 .. Versions.Lib'Length) := Versions.Lib;
      Msg.Item.Data_Len := 0;
      Dummy := Client_Com.Dictio_Send (New_Dscr, null, Msg);

      Dictio_Status := Status.Get_Stable;
      Send_Status (New_Dscr);
    exception
      when Socket.Soc_Tail_Err =>
        null;
      when Socket.Soc_Conn_Lost =>
        if Debug.Level_Array(Debug.Client) then
          Debug.Put ("Client: lost connection with "
                   & Event_Mng.File_Desc'Image(Socket.Fd_Of (New_Dscr)));
        end if;
        Client_Fd.Del_Client (New_Dscr);
    end;
  end Accept_Cb;


  procedure Allow_Clients is
    Port : Tcp_Util.Local_Port;
    Port_Name : constant String := Args.Get_Client_Port;
    Dscr : Socket.Socket_Dscr;
  begin
    if Debug.Level_Array(Debug.Client) then
      Debug.Put ("Client: allow");
    end if;
    Port.Name(1 .. Port_Name'Length) := Port_Name;
    Tcp_Util.Accept_From (Socket.Tcp_Header, Port, Accept_Cb'access,
                          Dscr, Accept_Port);
    State := Allow;
  end Allow_Clients;


  procedure Start is
  begin
    if State /= Not_Init then
      return;
    end if;
    if Debug.Level_Array(Debug.Client) then
      Debug.Put ("Client: start");
    end if;
    Sync_Mng.Start (Allow_Clients'access);
    State := Waiting;
  end Start;


  procedure Quit is
    Dscr : Socket.Socket_Dscr;
  begin
    if Debug.Level_Array(Debug.Client) then
      Debug.Put ("Client: quit");
    end if;
    case State is
      when Not_Init =>
        null;
      when Waiting =>
        Sync_Mng.Cancel;
      when Allow =>
        -- Delete all notify
        Notify.Del_All;
        -- Abort accept and close all client sockets
        Tcp_Util.Abort_Accept (Accept_Port);
        Client_Fd.Del_All;
      end case;
  end Quit;

  procedure Modified (Kind : in Character; Item : Data_Base.Item_Rec) is
  begin
    if Kind = Intra_Dictio.Sync_Kind then
      if Debug.Level_Array(Debug.Client) then
        Debug.Put ("Client: receive sync " & Parse(Item.Name));
      end if;
      Sync_Mng.Sync_Received;
    else 
      if Debug.Level_Array(Debug.Client) then
        Debug.Put ("Client: modified data " & Parse(Item.Name));
      end if;
    end if;
    Data_Base.Set (Item);
    Notify.Send (Item);
  end Modified;


  procedure Send_Status (Dscr : in Socket.Socket_Dscr) is
    Msg : Client_Com.Dictio_Client_Rec;
    Dummy : Boolean;
    State : constant String
          := Status.Stable_Status_List'Image(Dictio_Status);
    
  begin
    if Debug.Level_Array(Debug.Client) then
      Debug.Put ("Client: sending dictio status: " & State);
    end if;
    Msg.Action := Client_Com.State;
    Msg.Item.Name := (others => ' ');
    Msg.Item.Name(1 .. State'Length) := State;
    Msg.Item.Data_Len := 0;
    Dummy := Client_Com.Dictio_Send (Dscr, null, Msg);
  end Send_Status;

  procedure New_Status is
    Got_New_Status : Status.Stable_Status_List := Status.Get_Stable;
    Dscr : Socket.Socket_Dscr;
    use type Status.Stable_Status_List, Socket.Socket_Dscr;
  begin
    if Got_New_Status = Dictio_Status then
      return;
    end if;
    Dictio_Status := Got_New_Status;
    -- Notify all clients
    Client_Fd.Read_First (Dscr);
    while Dscr /= Socket.No_Socket loop
      Send_Status (Dscr);
      Client_Fd.Read_Next (Dscr);
    end loop;
  end New_Status;

end Client_Mng;

