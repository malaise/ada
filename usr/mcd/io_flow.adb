with Ada.Characters.Latin_1;
with As.U.Utils, Argument, Event_Mng, Sys_Calls, Async_Stdin, Text_Line,
     Socket, Socket_Util, Tcp_Util, Ip_Addr, Autobus;
with Io_Data, Debug;
package body Io_Flow is

  -- Io mode
  type Io_Mode_List is (
    Unknown,
    Stdio_Tty,
    Stdio_Not_Tty,
    Udp,
    Tcp,
    Abus);
  Io_Mode : Io_Mode_List := Unknown;

  -- Data read
  Input_Data : As.U.Asu_Us;

  -- Tempo data until Line Feed
  Tmp_Data : As.U.Asu_Us;

  -- Udp or tcp socket
  Host : Tcp_Util.Remote_Host;
  Port : Tcp_Util.Remote_Port;
  Send_Port : Tcp_Util.Remote_Port (Tcp_Util.Port_Num_Spec);
  Is_Ipm : Boolean;
  Soc : Socket.Socket_Dscr;
  Send_Soc : Socket.Socket_Dscr;
  Accepting_Soc : Socket.Socket_Dscr;
  function Socket_Is_Active return Boolean;
  procedure Activate_Socket (Active : in Boolean);
  procedure Open_Udp_Sockets (Send_Next : in Boolean);
  procedure Open_Tcp_Socket (And_Activate : in Boolean);
  procedure Soc_Send is new Socket.Send (Io_Data.Message_Type);
  procedure Soc_Receive is new Socket.Receive (Io_Data.Message_Type);

  -- Bus
  Bus_Addr : As.U.Asu_Us;
  Bus : aliased Autobus.Bus_Type;
  Messages : As.U.Utils.Asu_Dyn_List_Mng.List_Type;
  -- Observer recevier of messages
  Bus_Subscriber : Autobus.Subscriber_Type;
  type Bus_Observer_Type is new Autobus.Observer_Type with null record;
  procedure Receive (Observer : in out Bus_Observer_Type;
                     Subscriber : in Autobus.Subscriber_Access_Type;
                     Message : in String);
  Bus_Observer : aliased Bus_Observer_Type;

  -- Async Stdin
  function Stdin_Cb (Str : in String) return Boolean;

  -- Input flow when stdin is not a tty
  Input_Flow : Text_Line.File_Type;

  ----------------------------------------------------
  -- Init fifo, tcp, udp or stdin (async or not)
  ----------------------------------------------------
  -- Default init when sync stdin or forced
  procedure Init_Default is
  begin
    Io_Mode := Stdio_Not_Tty;
    Input_Flow.Open (Text_Line.In_File, Sys_Calls.Stdin);
  end Init_Default;

  procedure Init (Default : in Boolean := False) is
    use type Sys_Calls.File_Desc_Kind_List;
  begin
    if Io_Mode /= Unknown then
      return;
    end if;
    if Default then
      Init_Default;
      return;
    end if;

    -- Get bus address argument if set
    if Argument.Is_Set (1, "a") then
      -- Parse (auto)bus address
      Argument.Get_Parameter (Bus_Addr, 1, "a");
      if Bus_Addr.Is_Null then
        Async_Stdin.Put_Line_Err ("Missing autobus address.");
        raise Init_Error;
      end if;
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Init on autobus " & Bus_Addr.Image);
      end if;
      begin
        Bus.Init (Bus_Addr.Image);
      exception
        when Autobus.Invalid_Address =>
          Async_Stdin.Put_Line_Err ("Invalid autobus address "
                                  & Bus_Addr.Image & ".");
          raise Init_Error;
        when Autobus.Name_Error =>
          Async_Stdin.Put_Line_Err ("Name not found in autobus address "
                                  & Bus_Addr.Image & ".");
          raise Init_Error;
        when Autobus.Config_Error =>
          Async_Stdin.Put_Line_Err ("Invalid autobus config for address "
                                  & Bus_Addr.Image & ".");
          raise Init_Error;
      end;
      Bus_Subscriber.Init (Bus'Access, Bus_Observer'Access);
      Io_Mode := Abus;
    end if;

    -- Get tcp port if set
    if Argument.Is_Set (1, "t") then
      -- Parse TCP port
      begin
        Port := Ip_Addr.Parse (Argument.Get_Parameter (1, "t"));
      exception
        when Ip_Addr.Parse_Error =>
          Async_Stdin.Put_Line_Err ("Invalid tcp port.");
          raise Init_Error;
      end;
      Open_Tcp_Socket (True);
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Init on tcp port " &
          Ip_Addr.Image (Accepting_Soc.Get_Linked_To));
      end if;
      Io_Mode := Tcp;
    end if;

    -- Get udp/ipm spec if set
    if Argument.Is_Set (1, "u")
    or else Argument.Is_Set (1, "U") then
      -- Parse udp port or ipm address
      begin
        if Argument.Is_Set (1, "u") then
          Ip_Addr.Parse (Argument.Get_Parameter (1, "u"), Host, Port);
        else
          Ip_Addr.Parse (Argument.Get_Parameter (1, "U"), Host, Port);
        end if;
      exception
        when Ip_Addr.Parse_Error =>
          Async_Stdin.Put_Line_Err ("Invalid udp spec.");
          raise Init_Error;
      end;
      Open_Udp_Sockets (Argument.Is_Set (1, "U"));
      Io_Mode := Udp;
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Err ("Flow: Init on ");
        if Is_Ipm then
          Async_Stdin.Put_Err ("ipm LAN " &
            Ip_Addr.Image (Soc.Get_Destination_Host) & " ");
        else
          Async_Stdin.Put_Err ("udp ");
        end if;
        Async_Stdin.Put_Err ("port " & Ip_Addr.Image (Soc.Get_Linked_To));
        Async_Stdin.Put_Line_Err (" sending on port "
           & Ip_Addr.Image (Send_Soc.Get_Destination_Port));
      end if;
    end if;

    -- If no arg => Stdin
    if Io_Mode /= Unknown then
      if Argument.Get_Nbre_Arg /= 1 then
        Async_Stdin.Put_Line_Err ("Too many arguments.");
        raise Init_Error;
      end if;
      -- Mode is set
      return;
    else
      if Argument.Get_Nbre_Arg /= 0 then
        Async_Stdin.Put_Line_Err ("Invalid argument.");
        raise Init_Error;
      end if;
    end if;

    -- Stdin
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Line_Err ("Flow: Init on stdio");
    end if;
    -- Set stdin/out asynchronous if it is a Tty
    if Sys_Calls.File_Desc_Kind (Sys_Calls.Stdin)  = Sys_Calls.Tty
    and then Sys_Calls.File_Desc_Kind (Sys_Calls.Stdout) = Sys_Calls.Tty then
      Io_Mode := Stdio_Tty;
      Async_Stdin.Set_Async (Stdin_Cb'Access, 0);
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Stdio is a tty");
      end if;
    else
      Init_Default;
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Stdio is a not a tty");
      end if;
    end if;

  end Init;

  function Is_Interactive return Boolean  is
  begin
    if Io_Mode = Unknown then
      raise Init_Error;
    else
      return Io_Mode = Stdio_Tty;
    end if;
  end Is_Interactive;

   procedure Clear_Interactive is
   begin
     if Io_Mode /= Stdio_Tty then
       raise Init_Error;
    end if;
    Async_Stdin.Clear;
  end Clear_Interactive;

  ----------------------------------------------------
  -- Get data from fifo, tcp, udp or stdin (async or not)
  ----------------------------------------------------
  procedure Next_Line (Str : out As.U.Asu_Us) is
    Evt : Event_Mng.Out_Event_List;
    Len : Natural;
    use type Event_Mng.Out_Event_List;
  begin
    if Io_Mode = Stdio_Not_Tty then
      Input_Data.Set_Null;
      -- Get next non empty line from Stdin (not a tty)
      loop
        -- Get next line
        Input_Data := Input_Flow.Get;
        -- End of flow when got an empty line
        Len := Input_Data.Length;
        exit when Len = 0;
        -- Remove trailing Line_Feed
        if Input_Data.Element (Len) = Text_Line.Line_Feed_Char then
          Input_Data.Delete (Len, Len);
          Len := Len - 1;
        end if;
        -- This line is Ok if not empty
        exit when Len /= 0;
      end loop;
    elsif Io_Mode = Abus then
      -- Get data from buffer of autobus messages
      loop
        if not Messages.Is_Empty then
          Messages.Rewind;
          Messages.Get (Str);
          exit;
        end if;
        Evt := Event_Mng.Wait (Event_Mng.Infinite_Ms);

        if Evt = Event_Mng.Signal_Event then
          -- Give up on signal
          Input_Data.Set_Null;
          if Debug.Debug_Level_Array(Debug.Flow) then
            Async_Stdin.Put_Line_Err ("Flow: Got signal");
          end if;
          exit;
        else
          if Debug.Debug_Level_Array(Debug.Flow) then
            Async_Stdin.Put_Line_Err ("Flow: Got event " & Evt'Img);
          end if;
        end if;
      end loop;
    else
      -- Get next data on async stdin, socket
      loop
        Input_Data.Set_Null;
        if Debug.Debug_Level_Array(Debug.Flow) then
          Async_Stdin.Put_Line_Err ("Flow: Waiting on bus/socket/tty");
        end if;
        Evt := Event_Mng.Wait (Event_Mng.Infinite_Ms);

        if Evt = Event_Mng.Fd_Event
        and then not Input_Data.Is_Null then
          -- New string
          exit;
        elsif Evt = Event_Mng.Signal_Event then
          -- Give up on signal
          Input_Data.Set_Null;
          if Debug.Debug_Level_Array(Debug.Flow) then
            Async_Stdin.Put_Line_Err ("Flow: Got signal");
          end if;
          exit;
        else
          if Debug.Debug_Level_Array(Debug.Flow) then
            Async_Stdin.Put_Line_Err ("Flow: Got event " & Evt'Img);
          end if;
        end if;
      end loop;
    end if;
    Str := Input_Data;
    -- Allow input data to be overwritten
    case Io_Mode is
      when Stdio_Tty =>
        Async_Stdin.Activate (True);
      when Abus =>
        null;
      when Udp | Tcp =>
        Activate_Socket (True);
      when others =>
        null;
    end case;
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Line_Err ("Flow: Next_Line -> " & Str.Image);
    end if;
  end Next_Line;

  ----------------------------------------------------
  -- Pet data on fifo, tcp, udp or stdin (async or not)
  ----------------------------------------------------
  -- Send one message on socket or Autobus
  Message_To_Put : Io_Data.Message_Type;
  procedure Send_Message (Str : in String) is
    Active : Boolean;
  begin
    Message_To_Put (1 .. Str'Length) := Str;
    if Io_Mode = Abus then
      Bus.Send (Str);
    elsif Io_Mode = Tcp then
      Active := Socket_Is_Active;
      Soc_Send (Soc, Message_To_Put, Str'Length);
    else
      Soc_Send (Send_Soc, Message_To_Put, Str'Length);
    end if;
  exception
    when Socket.Soc_Would_Block =>
      -- TCP
      Activate_Socket (False);
      Soc.Close;
      Open_Tcp_Socket (Active);
  end Send_Message;

  -- Put or send string
  procedure Put (Str : in String) is
    F, L : Natural;
  begin
    case Io_Mode is
      when Unknown =>
        null;
      when Stdio_Tty | Stdio_Not_Tty =>
        -- Put on stdout (tty or not)
        begin
          Async_Stdin.Put_Out (Str);
        exception
          -- Hide error, like on other flows
          when Async_Stdin.Io_Error => null;
        end;
      when Udp | Tcp | Abus =>
        -- Skip if channel is not active
        if (Io_Mode = Udp or else Io_Mode = Tcp)
        and then not Soc.Is_Open then
          return;
        end if;
        -- Send on Socket/AutoBus several messages
        F := Str'First;
        loop
          L := F + Autobus.Message_Max_Length - 1;
          if L > Str'Last then
            L := Str'Last;
          end if;
          if Debug.Debug_Level_Array(Debug.Flow) then
            Async_Stdin.Put_Line_Err ("Flow: Sending -> " & Str(F .. L) & "<");
          end if;
          Send_Message (Str(F .. L));
          if Debug.Debug_Level_Array(Debug.Flow) then
            Async_Stdin.Put_Line_Err ("Flow: Sent -> " & Str(F .. L) & "<");
          end if;
          exit when L = Str'Last;
          F := L + 1;
        end loop;
    end case;
  end Put;

  procedure New_Line is
  begin
    Put_Line ("");
  end New_Line;

  procedure Put_Line (Str : in String) is
  begin
    -- Send/put Lf (the Unix standard)
    Put (Str & Ada.Characters.Latin_1.Lf);
  end Put_Line;

  procedure Close is
  begin
    case Io_Mode is
      when Stdio_Tty =>
        -- Reset tty blocking
        Async_Stdin.Set_Async;
        Async_Stdin.Flush_Out;
      when Stdio_Not_Tty =>
        -- Close input flow
        Input_Flow.Close;
        Async_Stdin.Flush_Out;
      when Abus =>
        Bus_Subscriber.Reset;
        Bus.Reset;
      when Udp =>
        if Debug.Debug_Level_Array(Debug.Flow) then
          Async_Stdin.Put_Line_Err ("Flow: Closing udp");
        end if;
        if Soc.Is_Open then
          if Socket_Is_Active then
            Activate_Socket (False);
          end if;
          Soc.Close;
        end if;
      when Tcp =>
        if Debug.Debug_Level_Array(Debug.Flow) then
          Async_Stdin.Put_Line_Err ("Flow: Closing tcp");
        end if;
        if Soc.Is_Open then
          Activate_Socket (False);
          Soc.Close;
        end if;
        if Accepting_Soc.Is_Open then
          Event_Mng.Del_Fd_Callback (Accepting_Soc.Get_Fd, True);
          Accepting_Soc.Close;
        end if;
      when others =>
        null;
    end case;
  end Close;

  ----------------------------------------------------
  -- Bus reception callback
  ----------------------------------------------------
  procedure Receive (Observer : in out Bus_Observer_Type;
                     Subscriber : in Autobus.Subscriber_Access_Type;
                     Message : in String) is
    pragma Unreferenced (Observer, Subscriber);
  begin
    if Message'Length = 0 then
      return;
    end if;
    -- Add this chunk
    Tmp_Data.Append (Message);
    if      Message(Message'Last) = Ada.Characters.Latin_1.Cr
    or else Message(Message'Last) = Ada.Characters.Latin_1.Lf then
      -- Validate the overall string and append it to list
      Input_Data := Tmp_Data;
      Tmp_Data.Set_Null;
      Messages.Rewind (False, As.U.Utils.Asu_Dyn_List_Mng.Next);
      Messages.Insert (Input_Data);
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Bus reception of >"
                             & Input_Data.Image & "<");
      end if;
    end if;
  end Receive;

  ----------------------------------------------------
  -- Async stdin callback
  ----------------------------------------------------
  function Stdin_Cb (Str : in String) return Boolean is
  begin
    if Str = "" then
      -- Error or end
      Input_Data := As.U.Tus (Str);
      return True;
    else
      Input_Data := As.U.Tus (Str);
    end if;
    -- Prevent overwritting of Input_Data by freezing Stdin
    Async_Stdin.Activate (False);
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Line_Err ("Flow: Stdin_Cb set >"
                           & Input_Data.Image & "<");
    end if;
    return True;
  end Stdin_Cb;

  ----------------------------------------------------
  -- Socket operations
  ----------------------------------------------------
  Tcp_Active : Boolean;
  function Socket_Connect_Cb (Fd : in Event_Mng.File_Desc; Read : in Boolean)
                          return Boolean is
    pragma Unreferenced (Fd, Read);
  begin
    -- Accept re-connection
    Accepting_Soc.Accept_Connection (Soc);
    -- Detach and close accepting socket
    Event_Mng.Del_Fd_Callback (Accepting_Soc.Get_Fd, True);
    Accepting_Soc.Close;
    -- Activate socket if it was requested by open
    if Tcp_Active then
      Activate_Socket (True);
    end if;
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Err ("Flow: Tcp socket accepted and ");
      if Tcp_Active then
        Async_Stdin.Put_Line_Err ("active");
      else
        Async_Stdin.Put_Line_Err ("inactive");
      end if;
    end if;
    return False;
  end Socket_Connect_Cb;

  procedure Disconnect_Socket is
  begin
    Activate_Socket (False);
    Soc.Close;
    Open_Tcp_Socket (True);
  end Disconnect_Socket;

  function Socket_Rece_Cb (Fd : in Event_Mng.File_Desc; Read : in Boolean)
                          return Boolean is
    pragma Unreferenced (Fd, Read);
    Message : Io_Data.Message_Type;
    Length : Natural;
  begin
    begin
      Soc_Receive (Soc, Message, Length, Io_Mode = Udp);
    exception
      when Socket.Soc_Read_0 =>
        -- Tcp Disconnection: close and accept new
        if Debug.Debug_Level_Array(Debug.Flow) then
          Async_Stdin.Put_Line_Err ("Flow: Socket_Rece_Cb disconnection");
        end if;
        Disconnect_Socket;
        return False;
    end;
    if Length = 0 then
      return False;
    end if;
    -- Cancel Tcp connection when receiveing CtrlD
    if Io_Mode = Tcp
    and then Length = 1
    and then Message(1) = Ada.Characters.Latin_1.Eot then
      -- Tcp Disconnection: close and accept new
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Socket_Rece_Cb disconnecting");
      end if;
      Disconnect_Socket;
      return False;
    end if;
    -- Add this chunk
    Tmp_Data.Append (Message(1 .. Length));
    if      Message(Length) = Ada.Characters.Latin_1.Cr
    or else Message(Length) = Ada.Characters.Latin_1.Lf then
      -- Validate the overall string
      Input_Data := Tmp_Data;
      Tmp_Data.Set_Null;
      -- Freeze fifo to prevent Input_Data to be overwritten
      Activate_Socket (False);
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Socket_Rece_Cb set >"
                             & Input_Data.Image & "<");
      end if;
      return True;
    else
      return False;
    end if;
  end Socket_Rece_Cb;

  function Socket_Is_Active return Boolean is
  begin
    if not Soc.Is_Open and then Io_Mode = Tcp then
      return Tcp_Active;
    else
      return Soc.Is_Open and then Event_Mng.Fd_Callback_Set (Soc.Get_Fd, True);
    end if;
  end Socket_Is_Active;

  procedure Activate_Socket (Active : in Boolean) is
  begin
    if Active = Socket_Is_Active then
      return;
    end if;
    if not Soc.Is_Open and then Io_Mode = Tcp then
      Tcp_Active := Active;
    elsif Active then
      Event_Mng.Add_Fd_Callback (Soc.Get_Fd, True, Socket_Rece_Cb'Access);
    else
      Event_Mng.Del_Fd_Callback (Soc.Get_Fd, True);
    end if;
  end Activate_Socket;

  procedure Open_Tcp_Socket (And_Activate : in Boolean) is
  begin
    -- Create accepting socket
    Accepting_Soc.Open (Socket.Tcp);
    -- Listen and activate
    Socket_Util.Link (Accepting_Soc, Port);
    Event_Mng.Add_Fd_Callback (Accepting_Soc.Get_Fd, True,
                               Socket_Connect_Cb'Access);
    -- Remember activation of the accepted socket
    Tcp_Active := And_Activate;
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Err ("Flow: Tcp socket open and ");
      if And_Activate then
        Async_Stdin.Put_Line_Err ("active");
      else
        Async_Stdin.Put_Line_Err ("inactive");
      end if;
    end if;
  end Open_Tcp_Socket;

  procedure Open_Udp_Sockets (Send_Next : in Boolean) is
    use type Tcp_Util.Remote_Host_List, Tcp_Util.Remote_Port_List,
             Socket.Port_Num;
  begin
    -- Open reception socket
    Soc.Open (Socket.Udp);
    if Host.Kind = Tcp_Util.Host_Id_Spec
    or else not Host.Name.Is_Null then
      -- An address specified => Ipm
      -- Use Set_Dest to indicate Imp address
      Is_Ipm := True;
    else
      -- No address => broadcast
      Host := (Kind => Tcp_Util.Host_Id_Spec,
               Id => Socket.Bcast_Of (Socket.Local_Host_Id));
      Is_Ipm := False;
    end if;
    Socket_Util.Set_Destination (Soc, True, Host, Port);
    Socket_Util.Link (Soc, Port);
    Activate_Socket (True);
    -- Open emission socket
    Send_Soc.Open (Socket.Udp);
    Send_Port.Num := Soc.Get_Linked_To;
    if Send_Next then
      Send_Port.Num := Send_Port.Num + 1;
    else
      Send_Port.Num := Send_Port.Num - 1;
    end if;
    Socket_Util.Set_Destination (Send_Soc, True, Host, Send_Port);
    -- Done
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Err ("Flow: Udp sockets open");
    end if;
  end Open_Udp_Sockets;

end Io_Flow;

