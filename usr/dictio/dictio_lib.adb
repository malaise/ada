with Ada.Exceptions;
with Socket, Tcp_Util, Event_Mng, Sys_Calls;
with Debug, Parse, Client_Com, Versions;
package body Dictio_Lib is

  Dictio_Env_Host : constant String := "DICTIO_HOST";
  Dictio_Env_Port : constant String := "DICTIO_PORT";
  Default_Host :  constant String := "localhost";
  Default_Port :  constant String := "dictio";
  Host : Tcp_Util.Remote_Host(Tcp_Util.Host_Name_Spec);
  Port : Tcp_Util.Remote_Port(Tcp_Util.Port_Name_Spec);

  Dictio_Dscr : Socket.Socket_Dscr := Socket.No_Socket;


  procedure Connect_To_Dictio;

  Msg : Client_Com.Dictio_Client_Rec;

  -- Send Msg on Dictio_Dscr
  procedure Send_Request;

  procedure Close is
  begin
    Event_Mng.Del_Fd_Callback (Socket.fd_Of (Dictio_Dscr), True);
    Socket.Close (Dictio_Dscr);
    if Available_Cb /= null then
      Available_Cb (False);
    end if;
  end Close;

  function Read_Cb (Fd : in Event_Mng.File_Desc; Read : in Boolean) return Boolean is
    Len : Natural;
  begin
    Read_Msg:
    begin
      Client_Com.Dictio_Receive (Dictio_Dscr, Msg, Len);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
        if Debug.Level_Array(Debug.Lib) then
          Debug.Put ("Dictio_Lib: disconnected");
        end if;
        Close;
        begin
          Connect_To_Dictio;
        exception
          when others =>
            null;
        end;
        return False;
      when Socket.Soc_Len_Err =>
        if Debug.Level_Array(Debug.Lib) then
          Debug.Put ("Dictio_Lib: received invalid size, givig up");
        end if;
        Close;
        return False;
    end Read_Msg;

    case Msg.Action is
      when Client_Com.Version =>
        if Parse (Msg.Item.Name) /= Versions.Lib then
          if Debug.Level_Array(Debug.Lib) then
            Debug.Put ("Dictio_Lib: received invalid version: "
                     & Parse (Msg.Item.Name)
                     & " being: " & Versions.Lib);
          end if;
          Close;
        end if;
        return False;
      when Client_Com.Read =>
        if Debug.Level_Array(Debug.Lib) then
          Debug.Put ("Dictio_Lib: received read reply:"
                   & Parse(Msg.Item.Data));
        end if;
        return True;
      when Client_Com.Notif_On =>
        if Debug.Level_Array(Debug.Lib) then
          Debug.Put ("Dictio_Lib: received notify on: "
                   & Parse(Msg.Item.Name) & " "
                   & Msg.Item.Data(1 .. Msg.Item.Data_Len));
        end if;
        if Notify_Cb /= null then
          Notify_Cb (Parse(Msg.Item.Name),
                     Msg.Item.Data(1 .. Msg.Item.Data_Len));
        end if;
        return False;
      when Client_Com.Write | Client_Com.Notif_Off =>
        if Debug.Level_Array(Debug.Lib) then
          Debug.Put ("Dictio_Lib: received invalid kind: "
                   & Msg.Action'Img & ", giving up");
        end if;
        Close;
        return False;
     end case; 
  end Read_Cb;

  procedure Connection_Cb (Remote_Port_Num : in Tcp_Util.Port_Num;
                           Remote_Host_Id  : in Tcp_Util.Host_Id;
                           Connected       : in Boolean;
                           Dscr            : in Socket.Socket_Dscr) is
  begin
    if not Connected then
      if Debug.Level_Array(Debug.Lib) then
        Debug.Put ("Dictio_Lib: ERROR, Connection_Cb(not connected)");
      end if;
      return;
    end if;
    if Debug.Level_Array(Debug.Lib) then
      Debug.Put ("Dictio_Lib: connected");
    end if;
    Dictio_Dscr := Dscr;
    Event_Mng.Add_Fd_Callback (Socket.fd_Of (Dictio_Dscr), True, Read_Cb'access);

    Msg.Action := Client_Com.Version;
    Msg.Item.Name := (others => ' ');
    Msg.Item.Name(1 .. Versions.Lib'Length) := Versions.Lib;
    Msg.Item.Data_Len := 0;
    Send_Request;
    if Available_Cb /= null then
      Available_Cb (True);
    end if;
  end Connection_Cb;

  procedure Connect_To_Dictio is
    Connected : Boolean;
  begin
    Connected := Tcp_Util.Connect_To (Socket.Tcp_Header,
                                      Host, Port, 1.0, 0,
                                      Connection_Cb'access);
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
  end Check_Name;

  procedure Check_Data (Data : in String) is
  begin
    if Data'Length > Max_Data_Len then
      raise Data_Too_Long;
    end if;
  end Check_Data;

  procedure Init is
    Env_Set, Env_Trunc : Boolean;
    Env_Len : Natural;
  begin
    Debug.Init;
    Sys_Calls.Getenv (Dictio_Env_Host, Env_Set, Env_Trunc,
                      Host.Name, Env_Len);
    if Env_Set and then not Env_Trunc then
      Host.Name (Env_Len+1 .. Host.Name'Last) := (others => ' ');
    else
      Host.Name := (others => ' ');
      Host.Name (1 .. Default_Host'Length) := Default_Host;
    end if;
    Sys_Calls.Getenv (Dictio_Env_Port, Env_Set, Env_Trunc,
                      Port.Name, Env_Len);
    if Env_Set and then not Env_Trunc then
      Port.Name (Env_Len+1 .. Port.Name'Last) := (others => ' ');
    else
      Port.Name := (others => ' ');
      Port.Name (1 .. Default_Port'Length) := Default_Port;
    end if;
    if Debug.Level_Array(Debug.Lib) then
      Debug.Put ("Dictio_Lib: init to " & Parse(Host.Name)
               & " / " & Parse(Port.Name));
    end if;
    
    Connect_To_Dictio;
  exception
    when Error:others =>
      if Debug.Level_Array(Debug.Lib) then
        Debug.Put ("Dictio_Lib: init fails on exception "
                 & Ada.Exceptions.Exception_name(Error));
      end if;
      raise No_Dictio; 
  end Init;

  procedure Lib_Send is new Socket.Send (Client_Com.Dictio_Client_Rec);
  procedure Send_Request is
  begin
    Lib_Send (Dictio_Dscr, Msg);
  exception
    when Error:others =>
      if Debug.Level_Array(Debug.Lib) then
        Debug.Put ("Dictio_Lib: send fails on exception "
                 & Ada.Exceptions.Exception_name(Error));
      end if;
      Close;
      raise No_Dictio;
  end Send_Request;

  -- Get Item data
  -- May raise Name_Too_Long or No_Item 
  function Get (Name : in String) return String is
    use type Data_base.Item_Rec;
  begin
    if Debug.Level_Array(Debug.Lib) then
      Debug.Put ("Dictio_Lib: get " & Name);
    end if;
    Check_Available;
    Check_Name (Name);
    -- Send request
    Msg.Action := Client_Com.Read;
    Msg.Item.Name := (others => ' ');
    Msg.Item.Name(1 .. Name'Length) := Name;
    Send_Request;
    -- Wait for reply
    loop
      exit when Read_Cb(0, True);
    end loop;
    Check_Available;
    if Msg.Item = Data_Base.No_Item then
      raise no_Item;
    end if;
    if Debug.Level_Array(Debug.Lib) then
      Debug.Put ("Dictio_Lib: get -> " & Msg.Item.Data(1 .. Msg.Item.Data_Len));
    end if;
    return Msg.Item.Data(1 .. Msg.Item.Data_Len);
  end Get;


  -- Create/Modify Item
  -- May raise Name_Too_Long or Data_Too_Long
  procedure Set (Name : in String; Data : in String) is
  begin
    if Debug.Level_Array(Debug.Lib) then
      Debug.Put ("Dictio_Lib: set " & Name & " " & Data);
    end if;
    Check_Available;
    Check_Name (Name);
    Check_Data (Data);
    Msg.Action := Client_Com.Write;
    Msg.Item.Name := (others => ' ');
    Msg.Item.Name(1 .. Name'Length) := Name;
    Msg.Item.Data_Len := Data'Length;
    Msg.Item.Data(1 .. Msg.Item.Data_Len) := Data;
    Send_Request;
  end Set;

  -- (Un)Notify on Item name
  -- May raise Name_Too_Long
  procedure Notify (Name : in String; On : in Boolean) is
  begin
    if Debug.Level_Array(Debug.Lib) then
      Debug.Put ("Dictio_Lib: notify " & On'Img & " " & Name);
    end if;
    Check_Available;
    Check_Name (Name);
    if On then
      Msg.Action := Client_Com.Notif_On;
    else
      Msg.Action := Client_Com.Notif_Off;
    end if;
    Msg.Item.Name := (others => ' ');
    Msg.Item.Name(1 .. Name'Length) := Name;
    Send_Request;
  end Notify;

end Dictio_Lib;

