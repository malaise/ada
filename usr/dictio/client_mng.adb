with Timers, Socket, Tcp_Util, Dynamic_List, X_Mng, Sys_Calls;
with Args, Parse, Notify, Client_Fd, Client_Com, Debug, Intra_Dictio;
package body Client_Mng is


  type State_List is (Not_Init, Waiting, Allow);
  State : State_List := Not_Init;

  Accept_Port : Tcp_Util.Port_Num;
  Timer_Id : Timers.Timer_Id := Timers.No_Timer;
  Sync_Received : Boolean := True;

  function Read_Cb (Fd : in X_Mng.File_Desc; Read : in Boolean)
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
               & Sys_Calls.File_Desc'Image(Socket.Fd_Of (New_Dscr)));
    end if;
    Client_Fd.Add_Client (New_Dscr);
    X_Mng.X_Add_Callback (Socket.Fd_Of (New_Dscr), True, Read_Cb'access);
  end Accept_Cb;


  function Timer_Cb (Id : in Timers.Timer_Id) return Boolean is
    Port : Tcp_Util.Local_Port;
    Port_Name : constant String := Args.Get_Client_Port;
    Dscr : Socket.Socket_Dscr;
  begin
    if Sync_Received then
      -- Still in sync
      Sync_Received := False;
      return False;
    end if;
    Timers.Delete (Id);
    if Debug.Level_Array(Debug.Client) then
      Debug.Put ("Client: allow");
    end if;
    Port.Name(1 .. Port_Name'Length) := Port_Name;
    Tcp_Util.Accept_From (Socket.Tcp_Header, Port, Accept_Cb'access,
                          Dscr, Accept_Port);
    State := Allow;
    return False;
  end Timer_Cb;


  procedure Start is
  begin
    if State /= Not_Init then
      return;
    end if;
    if Debug.Level_Array(Debug.Client) then
      Debug.Put ("Client: start");
    end if;
    Timer_Id := Timers.Create ( (Timers.Delay_Sec, 1.0, 1.0),
                                Timer_Cb'access);
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
        Timers.Delete (Timer_Id);
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
      Sync_Received := True;
    else 
      if Debug.Level_Array(Debug.Client) then
        Debug.Put ("Client: modified data " & Parse(Item.Name));
      end if;
    end if;
    Data_Base.Set (Item);
    Notify.Send (Item);
  end Modified;

end Client_Mng;

