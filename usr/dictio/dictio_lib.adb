with Ada.Exceptions;
with As.U, Socket, Tcp_Util, Event_Mng, Sys_Calls, Environ, Virtual_Time;
with Dictio_Debug, Parse, Client_Com, Versions, Status, Names;
package body Dictio_Lib is

  Dictio_Env_Host : constant String := "DICTIO_HOST";
  Dictio_Env_Port : constant String := "DICTIO_PORT";
  Local_Host_Name :  constant String := "localhost";
  Default_Host :  constant String := Local_Host_Name;
  Default_Port :  constant String := "dictio";
  Host : Tcp_Util.Remote_Host(Tcp_Util.Host_Name_Spec);
  Port : Tcp_Util.Remote_Port(Tcp_Util.Port_Name_Spec);
  Protocol : Socket.Protocol_List := Socket.Tcp_Header;

  Dictio_Dscr : Socket.Socket_Dscr := Socket.No_Socket;
  Dictio_State : Dictio_State_List := Unavailable;


  procedure Connect_To_Dictio;

  Msg : Client_Com.Dictio_Client_Rec;

  -- Send Msg on Dictio_Dscr
  procedure Send_Request;

  procedure Close is
  begin
    Dictio_Debug.Put (Dictio_Debug.Lib, "Closing");
    if Dictio_Dscr.Is_Open then
      Event_Mng.Del_Fd_Callback (Dictio_Dscr.Get_Fd, True);
      Dictio_Dscr.Close;
    else
      begin
        Tcp_Util.Abort_Connect (Host, Port);
      exception
        when Tcp_Util.No_Such =>
          null;
      end;
    end if;
    if Dictio_State /= Unavailable then
      Dictio_State := Unavailable;
      if Dictio_State_Cb /= null then
        Dictio_State_Cb (Dictio_State);
      end if;
    end if;
  end Close;

  function Read_Cb (Fd : in Event_Mng.File_Desc; Unused_Read : in Boolean)
           return Boolean is
    Len : Natural;
    State : Status.Stable_Status_List;
    New_Dictio_State : Dictio_State_List;
    use type Sys_Calls.File_Desc;
  begin

    Msg.Item := Data_Base.No_Item;

    Read_Msg:
    begin
      Client_Com.Dictio_Receive (Dictio_Dscr, Msg, Len);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
        Dictio_Debug.Put (Dictio_Debug.Lib, "Disconnected");
        Close;
        begin
          Connect_To_Dictio;
        exception
          when others =>
            null;
        end;
        return Fd = Sys_Calls.Stdin;
      when Socket.Soc_Len_Err =>
        Dictio_Debug.Put_Warning (Dictio_Debug.Lib,
                                  "Received invalid size, givig up");
        Close;
        return Fd = Sys_Calls.Stdin;
    end Read_Msg;

    case Msg.Action is
      when Client_Com.Version =>
        if Parse (Msg.Item.Name) /= Versions.Lib then
          Dictio_Debug.Put_Warning (Dictio_Debug.Lib,
                                    "Received invalid version: "
                                  & Parse (Msg.Item.Name)
                                  & " being: " & Versions.Lib);
          Close;
        end if;
        Dictio_Debug.Put (Dictio_Debug.Lib,
                          "received lib version: "
                        & Parse(Msg.Item.Name));
        return False;
      when Client_Com.State =>
        begin
          State := Status.Stable_Status_List'Value(Parse(Msg.Item.Name));
        exception
          when others =>
            Dictio_Debug.Put_Warning (Dictio_Debug.Lib,
                                      "Received invalid status: "
                                    & Parse (Msg.Item.Name));
            Close;
            return False;
        end;
        Dictio_Debug.Put (Dictio_Debug.Lib, "Received dictio status: "
                                          & Parse(Msg.Item.Name));
        case State is
          when Status.Slave =>
            New_Dictio_State := Slave;
          when Status.Master =>
            New_Dictio_State := Master;
          when Status.Dead =>
            New_Dictio_State := Unavailable;
        end case;
        if Dictio_State /= New_Dictio_State then
          Dictio_State := New_Dictio_State;
          if Dictio_State_Cb /= null then
            Dictio_State_Cb (Dictio_State);
          end if;
        end if;
        return False;
      when Client_Com.Get =>
          Dictio_Debug.Put (Dictio_Debug.Lib, "Received get reply:"
                                            & Parse(Msg.Item.Data));
        return True;
      when Client_Com.Notif_On =>
        Dictio_Debug.Put (Dictio_Debug.Lib,
                          "Received notify on: " & Parse(Msg.Item.Name) & " "
                        & Msg.Item.Kind & " "
                        & Msg.Item.Data(1 .. Msg.Item.Data_Len));
        if Msg.Item.Kind /= Data_Base.Data_Kind
        and then Msg.Item.Kind /= Data_Base.Alias_Kind then
          return False;
        end if;
        if Notify_Cb /= null then
          Notify_Cb (Parse(Msg.Item.Name),
                     Msg.Item.Kind = Data_Base.Data_Kind,
                     Msg.Item.Data(1 .. Msg.Item.Data_Len));
        end if;
        return False;
      when Client_Com.Set | Client_Com.Notif_Off =>
        Dictio_Debug.Put_Warning (Dictio_Debug.Lib,
                                  "Received invalid kind: "
                                & Msg.Action'Img & ", giving up");
        Close;
        return False;
     end case;
  end Read_Cb;

  procedure Connection_Cb (Unused_Remote_Host_Id  : in Tcp_Util.Host_Id;
                           Unused_Remote_Port_Num : in Tcp_Util.Port_Num;
                           Connected              : in Boolean;
                           Dscr                   : in Socket.Socket_Dscr) is
  begin
    if not Connected then
      Dictio_Debug.Put_Error (Dictio_Debug.Lib, "Connection_Cb(not connected)");
      return;
    end if;
    Dictio_Debug.Put (Dictio_Debug.Lib, "Connected");
    Dictio_Dscr := Dscr;
    Event_Mng.Add_Fd_Callback (Dictio_Dscr.Get_Fd, True, Read_Cb'Access);

    Msg.Action := Client_Com.Version;
    Msg.Item.Name := (others => ' ');
    Msg.Item.Name(1 .. Versions.Lib'Length) := Versions.Lib;
    Msg.Item.Data_Len := 0;
    Send_Request;
  end Connection_Cb;

  procedure Connect_To_Dictio is
    Dummy : Boolean;
  begin
    Dummy := Tcp_Util.Connect_To (Protocol, Host, Port,
                                  Connection_Cb'Access, 1.0, 0);
  exception
    when Error:others =>
      Dictio_Debug.Put (Dictio_Debug.Lib, "Connect fails on exception "
          & Ada.Exceptions.Exception_Name(Error));
  end Connect_To_Dictio;

  procedure Check_Available is
    use type Socket.Socket_Dscr;
  begin
    if Dictio_Dscr = Socket.No_Socket then
      raise No_Dictio;
    end if;
  end Check_Available;

  procedure Check_Name (Name : in String) is
  begin
    if Name'Length > Max_Name_Len then
      raise Name_Too_Long;
    end if;
    if not Names.Is_Valid_Name (Name) then
      raise Invalid_Name;
    end if;
  end Check_Name;

  procedure Check_Data (Data : in String) is
  begin
    if Data'Length > Max_Data_Len then
      raise Data_Too_Long;
    end if;
  end Check_Data;

  Init_Done : Boolean := False;

  procedure Init is
    Local_Host : Tcp_Util.Remote_Host(Tcp_Util.Host_Name_Spec);
    Expiration : Virtual_Time.Time;
    use type Event_Mng.Out_Event_List, Virtual_Time.Time, Tcp_Util.Remote_Host;
  begin
    if Init_Done then
      return;
    end if;

    Dictio_Debug.Init;

    -- Getenv host and port
    if Environ.Is_Set (Dictio_Env_Host) then
      Environ.Get_Us (Dictio_Env_Host, Host.Name);
    else
      Host.Name := As.U.Tus (Default_Host);
    end if;
    Local_Host.Name := As.U.Tus (Local_Host_Name);
    if Host = Local_Host then
      Protocol := Socket.Tcp_Header_Afux;
    else
      Protocol := Socket.Tcp_Header;
    end if;

    if Environ.Is_Set (Dictio_Env_Port) then
      Environ.Get_Us (Dictio_Env_Port, Port.Name);
    else
      Port.Name := As.U.Tus (Default_Port);
    end if;

    Dictio_Debug.Put (Dictio_Debug.Lib, "Init to " & Host.Name.Image
                                      & " / " & Port.Name.Image);

    Connect_To_Dictio;

    -- Wait a bit for connection to establish
    Event_Mng.Pause (100);

    -- Wait until connected or delay
    Expiration := Virtual_Time.Current_Time + 0.5;
    loop --## rule line off Loop_While
      exit when Dictio_State /= Unavailable
           or else Event_Mng.Wait (100) = Event_Mng.Signal_Event
           or else Virtual_Time.Is_Reached (Expiration);
    end loop;

    -- Call state callback if still not available
    if Dictio_State = Unavailable and then Dictio_State_Cb /= null then
      Dictio_State_Cb (Dictio_State);
    end if;
    Init_Done := True;
  exception
    when Error:others =>
      Dictio_Debug.Put_Error (Dictio_Debug.Lib,
                              "Init fails on exception "
                            & Ada.Exceptions.Exception_Name(Error));
      raise No_Dictio;
  end Init;

  procedure Lib_Send is new Socket.Send (Client_Com.Dictio_Client_Rec);
  procedure Send_Request is
  begin
    Lib_Send (Dictio_Dscr, Msg);
  exception
    when Error:others =>
      Dictio_Debug.Put_Error (Dictio_Debug.Lib,
                              "Send fails on exception "
                            & Ada.Exceptions.Exception_Name(Error));
      Close;
      raise No_Dictio;
  end Send_Request;

  -- Check item name validity
  --   <ident> [ { .<ident> } ]
  function Is_Valid_Item_Name (Name : in String) return Boolean is
  begin
    Check_Name (Name);
    return True;
  exception
    when Name_Too_Long | Invalid_Name =>
      return False;
  end Is_Valid_Item_Name;

  -- Get Item data or alias: Sets Msg!
  -- May raise Name_Too_Long or No_Item
  procedure Set_Msg_To (Name : in String; Kind : in Data_Base.Item_Kind) is
  begin
    Check_Available;
    Check_Name (Name);
    -- Send request
    Msg.Action := Client_Com.Get;
    Msg.Item.Name := (others => ' ');
    Msg.Item.Name(1 .. Name'Length) := Name;
    Msg.Item.Kind := Kind;
    Send_Request;
    -- Wait for reply
    loop --## rule line off Loop_While
      exit when Read_Cb(Sys_Calls.Stdin, True);
    end loop;
    Check_Available;
  end Set_Msg_To;


  -- Create/Modify Item
  -- May raise Name_Too_Long or Data_Too_Long
  procedure Set (Name : in String; Data : in String) is
  begin
    Dictio_Debug.Put (Dictio_Debug.Lib, "Set " & Name & " " & Data);
    Check_Available;
    Check_Name (Name);
    Check_Data (Data);
    Msg.Action := Client_Com.Set;
    Msg.Item.Name := (others => ' ');
    Msg.Item.Name(1 .. Name'Length) := Name;
    Msg.Item.Kind := Data_Base.Data_Kind;
    Msg.Item.Data_Len := Data'Length;
    Msg.Item.Data(1 .. Msg.Item.Data_Len) := Data;
    Msg.Item.Crc := Data_Base.No_Item.Crc;
    Send_Request;
  end Set;

  -- Get Item data
  -- May raise Name_Too_Long or No_Item
  function Get (Name : in String) return String is
    use type Data_Base.Item_Rec;
  begin
    Dictio_Debug.Put (Dictio_Debug.Lib, "Get " & Name);
    Set_Msg_To (Name, Data_Base.Data_Kind);
    if Msg.Item = Data_Base.No_Item then
      raise No_Item;
    end if;
    Dictio_Debug.Put (Dictio_Debug.Lib, "Get -> "
                                      & Msg.Item.Data(1 .. Msg.Item.Data_Len));
    return Msg.Item.Data(1 .. Msg.Item.Data_Len);
  end Get;

  -- (Un)Notify on Item name
  -- May raise Name_Too_Long
  procedure Notify (Name : in String; Item : in Boolean; On : in Boolean) is
  begin
    Dictio_Debug.Put (Dictio_Debug.Lib, "Notify " & Name & " item: "
                                      & Item'Img & " on: " & On'Img);
    Check_Available;
    if Name'Length > Max_Name_Len then
      raise Name_Too_Long;
    end if;
    if not Names.Is_Valid_Notify (Name) then
      raise Invalid_Name;
    end if;
    if On then
      Msg.Action := Client_Com.Notif_On;
    else
      Msg.Action := Client_Com.Notif_Off;
    end if;
    Msg.Item.Name := (others => ' ');
    Msg.Item.Name(1 .. Name'Length) := Name;
    if Item then
      Msg.Item.Kind := Data_Base.Data_Kind;
    else
      Msg.Item.Kind := Data_Base.Alias_Kind;
    end if;
    Send_Request;
  end Notify;

  -- Create/Modify delete alias
  -- May raise Name_Too_Long or Data_Too_Long
  procedure Set_Alias (Alias : in String; What : in String) is
  begin
    Dictio_Debug.Put (Dictio_Debug.Lib, "Set alias" & Alias & " " & What);
    Check_Available;
    Check_Name (Alias);
    if What /= "" then
      Check_Name (What);
    end if;
    Msg.Action := Client_Com.Set;
    Msg.Item.Name := (others => ' ');
    Msg.Item.Name(1 .. Alias'Length) := Alias;
    Msg.Item.Kind := Data_Base.Alias_Kind;
    Msg.Item.Data_Len := What'Length;
    Msg.Item.Data(1 .. Msg.Item.Data_Len) := What;
    Msg.Item.Crc := Data_Base.No_Item.Crc;
    Send_Request;
  end Set_Alias;

  -- Get Item alias
  -- May raise Name_Too_Long
  function Get_Alias (Alias : in String) return String is
  begin
    Dictio_Debug.Put (Dictio_Debug.Lib, "Get_alias " & Alias);
    Set_Msg_To (Alias, Data_Base.Alias_Kind);
    Dictio_Debug.Put (Dictio_Debug.Lib, "Get_alias -> "
                                      & Msg.Item.Data(1 .. Msg.Item.Data_Len));
    return Msg.Item.Data(1 .. Msg.Item.Data_Len);
  end Get_Alias;

end Dictio_Lib;

