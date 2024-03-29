with Ada.Calendar;
with As.U, Socket, Socket_Util, Tcp_Util, Event_Mng, Environ;
with Args, Parse, Notify, Client_Fd, Client_Com, Dictio_Debug, Intra_Dictio,
     Versions, Status, Alias;
package body Client_Mng is

  -- Init done?
  Init : Boolean := False;

  -- Each time a modif is done in data base
  Modif_Stamp : Ada.Calendar.Time := Ada.Calendar.Clock;

  Dictio_Status : Status.Stable_Status_List := Status.Dead;
  procedure Send_Status (Dscr : in Socket.Socket_Dscr);

  Accept_Port : Socket_Util.Port_Num;

  -- Send notification
  procedure Send_Notify (Item : Data_Base.Item_Rec)  is
  begin
    Notify.Send (Item);
  end Send_Notify;

  function Read_Cb (Fd : in Event_Mng.File_Desc; Unused_Read : in Boolean)
                   return Boolean is
    Dscr : Socket.Socket_Dscr;
    Msg : Client_Com.Dictio_Client_Rec;
    Len : Natural;
    Send_Res : Boolean;
    use type Socket.Socket_Dscr;
  begin
    Dscr := Client_Fd.Socket_Of (Fd);
    if Dscr = Socket.No_Socket then
      Dictio_Debug.Put_Error (Dictio_Debug.Client, "Unknown fd " & Fd'Img);
      return False;
    end if;
    begin
      Client_Com.Dictio_Receive (Dscr, Msg, Len);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
        Dictio_Debug.Put (Dictio_Debug.Client, "Disconnection of " & Fd'Img);
        Notify.Del_Client (Dscr);
        Client_Fd.Del_Client (Dscr);
        return False;
      when Socket.Soc_Len_Err =>
        Dictio_Debug.Put (Dictio_Debug.Client, "Invalid size from " & Fd'Img);
        Notify.Del_Client (Dscr);
        Client_Fd.Del_Client (Dscr);
        return False;
    end;
    Dictio_Debug.Put (Dictio_Debug.Client, "Request " & Msg.Action'Img & " >"
                                         & Parse (Msg.Item.Name) & "<");
    case Msg.Action is
      when Client_Com.Version =>
        if Parse (Msg.Item.Name) /= Versions.Lib then
          Dictio_Debug.Put (Dictio_Debug.Client, "Received invalid version: "
                                               & Parse (Msg.Item.Name)
                     & " being: " & Versions.Lib);
          Notify.Del_Client (Dscr);
          Client_Fd.Del_Client (Dscr);
        end if;
        return False;
      when Client_Com.State =>
        Dictio_Debug.Put (Dictio_Debug.Client, "Received status. "
                                             & Parse (Msg.Item.Name));
        Notify.Del_Client (Dscr);
        Client_Fd.Del_Client (Dscr);
        return False;
      when Client_Com.Get =>
        Alias.Resolve (Msg.Item);
        Msg.Item := Data_Base.Get (Msg.Item.Name, Msg.Item.Kind);
        begin
          Send_Res := Client_Com.Dictio_Send (Dscr, null, null, 0.0, Msg);
          Dictio_Debug.Put (Dictio_Debug.Client, "Get reply result "
                                               & Send_Res'Img);
        exception
          when Socket.Soc_Tail_Err =>
            Dictio_Debug.Put (Dictio_Debug.Client,
                             "Get lost cause client in overflow");
            Notify.Del_Client (Dscr);
            Client_Fd.Del_Client (Dscr);
            return False;
        when Socket.Soc_Conn_Lost =>
          Dictio_Debug.Put (Dictio_Debug.Client,
                           "Lost connection with " & Fd'Img);
          Notify.Del_Client (Dscr);
          Client_Fd.Del_Client (Dscr);
          return False;
        end;
      when Client_Com.Set =>
        Modif_Stamp := Ada.Calendar.Clock;
        -- Store, get new Crc
        Alias.Resolve (Msg.Item);
        Data_Base.Set_Then_Get_Crc (Msg.Item);
        -- Send notifications and diffuse
        Send_Notify (Msg.Item);
        Intra_Dictio.Send_Data (Msg.Item);
      when Client_Com.Notif_On =>
        Notify.Add (Dscr, Msg.Item.Name, Msg.Item.Kind);
      when Client_Com.Notif_Off =>
        Notify.Del (Dscr, Msg.Item.Name, Msg.Item.Kind);
    end case;
    Dictio_Debug.Put (Dictio_Debug.Client, "Request done");
    return False;
  end Read_Cb;

  procedure Accept_Cb (Local_Port_Num         : in Socket_Util.Port_Num;
                       Unused_Local_Dscr      : in Socket.Socket_Dscr;
                       Unused_Remote_Host_Id  : in Socket_Util.Host_Id;
                       Unused_Remote_Port_Num : in Socket_Util.Port_Num;
                       New_Dscr               : in Socket.Socket_Dscr) is
    use type Socket_Util.Port_Num;
  begin
    if Local_Port_Num /= Accept_Port then
      Dictio_Debug.Put_Error (Dictio_Debug.Client, "Unexpected accept");
      declare
        Close_Dscr : Socket.Socket_Dscr := New_Dscr;
      begin
        Close_Dscr.Close;
      end;
      return;
    end if;
    Dictio_Debug.Put (Dictio_Debug.Client,
        "New client accepted -> " & Event_Mng.File_Desc'Image(New_Dscr.Get_Fd));
    Client_Fd.Add_Client (New_Dscr);
    Event_Mng.Add_Fd_Callback (New_Dscr.Get_Fd, True, Read_Cb'Access);

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
      Dummy := Client_Com.Dictio_Send (New_Dscr, null, null, 0.0, Msg);

      Dictio_Status := Status.Get_Stable;
      Send_Status (New_Dscr);
    exception
      when Socket.Soc_Tail_Err =>
        null;
      when Socket.Soc_Conn_Lost =>
        Dictio_Debug.Put (Dictio_Debug.Client, "Lost connection with "
                   & Event_Mng.File_Desc'Image(New_Dscr.Get_Fd));
        Client_Fd.Del_Client (New_Dscr);
    end;
  end Accept_Cb;

  Stable_Delay : Duration := 0.0;

  procedure Set_Delay is
    Default_Stable_Delay : constant Duration := 0.2;
    Val_Ms : Positive;
  begin
    begin
      Stable_Delay := Default_Stable_Delay;
      Environ.Get_Dur ("DICTIO_STABLE_DELAY", Stable_Delay);
      -- Check that value in ms makes a valid positive
      Val_Ms := Positive (Stable_Delay * 1000);
    exception
      when others =>
        Stable_Delay := Default_Stable_Delay;
        Val_Ms := Positive(Default_Stable_Delay * 1000);
    end;
    Dictio_Debug.Put (Dictio_Debug.Client, "Stable delay set to " & Val_Ms'Img
                                         & " ms");
  end Set_Delay;


  procedure Start is
    Port : Socket_Util.Local_Port;
    Port_Name : constant String := Args.Get_Client_Port;
    Dscr : Socket.Socket_Dscr;
    Dscr_Afux : Socket.Socket_Dscr;
  begin
    if Init then
      return;
    end if;
    Dictio_Debug.Put (Dictio_Debug.Client, "Start");
    Port.Name := As.U.Tus (Port_Name);
    Tcp_Util.Accept_From (Socket.Tcp_Header, Port, Accept_Cb'Access,
                          Dscr, Accept_Port);
    Tcp_Util.Accept_From (Socket.Tcp_Header_Afux, Port, Accept_Cb'Access,
                          Dscr_Afux, Accept_Port);
    Set_Delay;
    Init := True;
  end Start;


  procedure Quit is
  begin
    Dictio_Debug.Put (Dictio_Debug.Client, "Quit");
    if not Init then
      return;
    end if;
    -- Delete all notify
    Notify.Del_All;
    -- Abort accept and close all client sockets
    Tcp_Util.Abort_Accept (Socket.Tcp_Header, Accept_Port);
    Tcp_Util.Abort_Accept (Socket.Tcp_Header_Afux, Accept_Port);
    Client_Fd.Del_All;
  end Quit;


  procedure Modified (Item : in Data_Base.Item_Rec) is
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
    Dictio_Debug.Put (Dictio_Debug.Client, "Sending dictio status: " & State);
    Msg.Action := Client_Com.State;
    Msg.Item.Name := (others => ' ');
    Msg.Item.Name(1 .. State'Length) := State;
    Msg.Item.Data_Len := 0;
    Dummy := Client_Com.Dictio_Send (Dscr, null, null, 0.0, Msg);
  end Send_Status;

  procedure New_Status is
    Got_New_Status : constant Status.Stable_Status_List := Status.Get_Stable;
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

