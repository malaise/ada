with Ada.Calendar;
with Socket, Tcp_Util, Dynamic_List, Event_Mng, Sys_Calls;
with Args, Parse, Notify, Client_Fd, Client_Com, Debug, Intra_Dictio,
     Versions, Status;
package body Client_Mng is

  -- Init done?
  Init : Boolean := False;

  -- Each time a modif is done in data base
  Modif_Stamp : Ada.Calendar.Time := Ada.Calendar.Clock;

  Dictio_Status : Status.Stable_Status_List := Status.Dead;
  procedure Send_Status (Dscr : in Socket.Socket_Dscr);

  Accept_Port : Tcp_Util.Port_Num;

  -- Send notification according to item kind
  procedure Send_Notify (Item : Data_Base.Item_Rec)  is
    use type Data_Base.Item_Rec;
  begin
    if Item.Kind = "d" then
      Notify.Send (Item);
    end if;
  end Send_Notify;

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
    if Debug.Level_Array(Debug.Client_Data) then
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
      when Client_Com.Get =>
        Data_Base.Get (Msg.Item.Name, Msg.Item.Kind, Msg.Item);
        begin
          Send_Res := Client_Com.Dictio_Send (Dscr, null, Msg);
          if Debug.Level_Array(Debug.Client_Data) then
            Debug.Put ("Client: get reply result " & Send_Res'Img);
          end if;
        exception
          when Socket.Soc_Tail_Err =>
            if Debug.Level_Array(Debug.Client) then
              Debug.Put ("Client: get lost cause client in overflow");
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
      when Client_Com.Set =>
        Modif_Stamp := Ada.Calendar.Clock;
        -- Store, get new Crc
        Data_Base.Set_Then_Get_Crc (Msg.Item);
        -- Send notifications and diffuse
        Send_Notify (Msg.Item);
        Intra_Dictio.Send_Data (Msg.Item);
      when Client_Com.Notif_On =>
        Notify.Add (Dscr, Msg.Item.Name);
      when Client_Com.Notif_Off =>
        Notify.Del (Dscr, Msg.Item.Name);
      when Client_Com.Add_Host =>
        Intra_Dictio.Add_Host (Msg.Item.Data(1 .. Msg.Item.Data_Len));
      when Client_Com.Del_Host =>
        Intra_Dictio.Del_Host (Msg.Item.Data(1 .. Msg.Item.Data_Len));
    end case;
    if Debug.Level_Array(Debug.Client_Data) then
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
    Event_Mng.Add_Fd_Callback (Socket.Fd_Of (New_Dscr), True, Read_Cb'Access);

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

  Stable_Delay : Duration := 0.0;

  procedure Set_Delay is
    Default_Val_Ms : constant Positive := 200;
    Val_Ms : Positive;
    Val : String (1 .. 5);
    Set, Trunc : Boolean;
    Len : Natural;
  begin
    Sys_Calls.Getenv ("DICTIO_STABLE_DELAY", Set, Trunc, Val, Len);
    if not Set or else Len = 0 or else Trunc then
      Val_Ms := Default_Val_Ms;
    else
      begin
        Stable_Delay := Duration'Value (Val(1 .. Len));
        Val_Ms := Positive (Stable_Delay * 1000);
      exception
        when others =>
          Val_Ms := Default_Val_Ms;
      end;
    end if;
    if Debug.Level_Array(Debug.Client) then
      Debug.Put ("Client: Stable delay set to " & Val_Ms'Img & " ms");
    end if;
    Stable_Delay := Duration(Val_Ms) / 1000.0;
  end Set_Delay;


  procedure Start is
    Port : Tcp_Util.Local_Port;
    Port_Name : constant String := Args.Get_Client_Port;
    Dscr : Socket.Socket_Dscr;
  begin
    if Init then
      return;
    end if;
    if Debug.Level_Array(Debug.Client) then
      Debug.Put ("Client: start");
    end if;
    Port.Name(1 .. Port_Name'Length) := Port_Name;
    Tcp_Util.Accept_From (Socket.Tcp_Header, Port, Accept_Cb'Access,
                          Dscr, Accept_Port);
    Set_Delay;
    Init := True;
  end Start;


  procedure Quit is
    Dscr : Socket.Socket_Dscr;
  begin
    if Debug.Level_Array(Debug.Client) then
      Debug.Put ("Client: quit");
    end if;
    if not Init then
      return;
    end if;
    -- Delete all notify
    Notify.Del_All;
    -- Abort accept and close all client sockets
    Tcp_Util.Abort_Accept (Accept_Port);
    Client_Fd.Del_All;
  end Quit;


  procedure Modified (Item : in Data_Base.Item_Rec) is
    use type Data_Base.Item_Rec;
  begin
    Modif_Stamp := Ada.Calendar.Clock;
    -- Intra data
    Data_Base.Set (Item);
    Send_Notify (Item);
  end Modified;

  function Stable return Boolean is
    use type Ada.Calendar.Time;
  begin
    return Ada.Calendar.Clock - Modif_Stamp >= Stable_Delay;
  end Stable;

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

