with Aski, As.U.Utils, Argument, Event_Mng, Sys_Calls, Async_Stdin, Text_Line,
     Socket, Socket_Util, Tcp_Util, Ip_Addr, Autobus;
with Io_Data, Debug;
package body Io_Flow is

  -- Io mode
  type Io_Mode_List is (
    Unknown,
    Stdio_Tty,
    Stdio_Not_Tty,
    File,
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
  overriding procedure Receive (
      Unused_Observer : in out Bus_Observer_Type;
      Unused_Subscriber : in Autobus.Subscriber_Access_Type;
      Message : in String);
  Bus_Observer : aliased Bus_Observer_Type;

  -- Async Stdin for input flow
  function Stdin_Cb (Str : in String) return Boolean;

  -- Async Stdin for Get
  Get_Echo : Boolean := True;
  function Get_Stdin_Cb (Str : in String) return Boolean;

  -- File name
  File_Name : As.U.Asu_Us;

  -- Input flow when stdin is not a tty or when file
  Input_Flow : Text_Line.File_Type;

  ----------------------------------------------------
  -- Init fifo, tcp, udp, file or stdin (async or not)
  ----------------------------------------------------
  -- Default init when sync stdin or forced
  procedure Init_Default is
  begin
    Io_Mode := Stdio_Not_Tty;
    Input_Flow.Open_All (Text_Line.In_File);
  end Init_Default;

  function Is_Stdio return Boolean is
    (Io_Mode = Stdio_Tty or else Io_Mode = Stdio_Not_Tty);

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

    -- 0 or 2 arguments
    if Argument.Get_Nbre_Arg = 0 then
      -- Stdin
      Debug.Log (Debug.Flow, "Init on stdio");
      -- Set stdin/out asynchronous if it is a Tty
      if Sys_Calls.File_Desc_Kind (Sys_Calls.Stdin)  = Sys_Calls.Tty
      and then Sys_Calls.File_Desc_Kind (Sys_Calls.Stdout) = Sys_Calls.Tty then
        Io_Mode := Stdio_Tty;
        Async_Stdin.Set_Async (Stdin_Cb'Access, 0);
        Debug.Log (Debug.Flow, "Stdio is a tty");
      else
        Init_Default;
        Debug.Log (Debug.Flow, "Stdio is a not a tty");
      end if;
    elsif Argument.Get_Nbre_Arg = 1 then
      Async_Stdin.Put_Line_Err ("Invalid argument.");
      raise Init_Error;
    elsif Argument.Get_Nbre_Arg /= 2 then
      Async_Stdin.Put_Line_Err ("Too many arguments.");
      raise Init_Error;
    elsif Argument.Get_Parameter (1) = "-a" then
      -- Get bus address argument if set
      -- Parse (auto)bus address
      Bus_Addr := As.U.Tus (Argument.Get_Parameter (Occurence => 2));
      Debug.Log (Debug.Flow, "Init on autobus " & Bus_Addr.Image);
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
    elsif Argument.Get_Parameter (1) = "-t" then
      -- Get tcp port if set
      -- Parse TCP port
      begin
        Port := Ip_Addr.Parse (Argument.Get_Parameter (2));
      exception
        when Ip_Addr.Parse_Error =>
          Async_Stdin.Put_Line_Err ("Invalid tcp port.");
          raise Init_Error;
      end;
      Open_Tcp_Socket (True);
      Debug.Log (Debug.Flow, "Init on tcp port "
                           & Ip_Addr.Image (Accepting_Soc.Get_Linked_To));
      Io_Mode := Tcp;
    elsif Argument.Get_Parameter (1) = "-u"
    or else Argument.Get_Parameter (1) = "-U" then
      -- Get udp/ipm spec if set
      -- Parse udp port or ipm address
      begin
        Ip_Addr.Parse (Argument.Get_Parameter (2), Host, Port);
      exception
        when Ip_Addr.Parse_Error =>
          Async_Stdin.Put_Line_Err ("Invalid udp spec.");
          raise Init_Error;
      end;
      Open_Udp_Sockets (Argument.Get_Parameter (1) = "-U");
      Io_Mode := Udp;
      Debug.Log (Debug.Flow, "Init on "
        & (if Is_Ipm then "ipm LAN " & Ip_Addr.Image (Soc.Get_Destination_Host)
           else "udp"
        & " port " & Ip_Addr.Image (Soc.Get_Linked_To)
        & " sending on port "
        & Ip_Addr.Image (Send_Soc.Get_Destination_Port)));
    elsif Argument.Get_Parameter (1) = "-f" then
      -- Get file name if set
      File_Name := As.U.Tus (Argument.Get_Parameter (Occurence => 2));
      if File_Name.Is_Null then
        Async_Stdin.Put_Line_Err ("Invalid file name.");
        raise Init_Error;
      end if;
      begin
        Input_Flow.Open_All (Text_Line.In_File, File_Name.Image);
      exception
        when others =>
          Async_Stdin.Put_Line_Err ("Cannot open input file "
                                  & File_Name.Image);
          raise Init_Error;
      end;
      Debug.Log (Debug.Flow, "Init on file " & File_Name.Image);
      Io_Mode := File;
    else
      Async_Stdin.Put_Line_Err ("Invalid argument.");
      raise Init_Error;
    end if;

    if not Is_Stdio then
      -- Open stdin asynchronous
      Set_Echo (True);
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
    Async_Stdin.Clear_Pending;
  end Clear_Interactive;

  ----------------------------------------------------
  -- Get data from fifo, tcp, udp or stdin (async or not)
  ----------------------------------------------------
  procedure Next_Line (Str : out As.U.Asu_Us) is
    Evt : Event_Mng.Out_Event_List;
    Len : Natural;
    use type Event_Mng.Out_Event_List;
  begin
    if Io_Mode = Stdio_Not_Tty
    or else Io_Mode = File then
      Input_Data.Set_Null;
      -- Get next non empty line from Stdin (not a tty) or File
      loop
        -- Get next line
        Input_Data := Input_Flow.Get;
        -- End of flow when got an empty line
        Len := Input_Data.Length;
        Debug.Log (Debug.Flow, "Got" & Len'Img & " bytes");
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
          Debug.Log (Debug.Flow, "Got signal");
          exit;
        else
          Debug.Log (Debug.Flow, "Got event " & Evt'Img);
        end if;
      end loop;
    else
      -- Get next data set by callback of async stdin, socket
      loop
        Input_Data.Set_Null;
        Debug.Log (Debug.Flow, "Waiting on bus/socket/tty");
        Evt := Event_Mng.Wait (Event_Mng.Infinite_Ms);

        if Evt = Event_Mng.Fd_Event
        and then not Input_Data.Is_Null then
          -- New string
          exit;
        elsif Evt = Event_Mng.Signal_Event then
          -- Give up on signal
          Input_Data.Set_Null;
          Debug.Log (Debug.Flow, "Got signal");
          exit;
        else
          Debug.Log (Debug.Flow, "Got event " & Evt'Img);
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
    Debug.Log (Debug.Flow, "Next_Line -> " & Str.Image);
  end Next_Line;

  -------------------------------
  -- Send data on bus, tcp or udp
  -------------------------------
  -- Send one message on Autobus, Tcp or Udp socket
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
    when others =>
      raise Communication_Error;
  end Send_Message;

  -- Put or send string
  procedure Put (Str : in String) is
    F, L : Natural;
  begin
    case Io_Mode is
      when Unknown =>
        null;
      when Stdio_Tty | Stdio_Not_Tty | File =>
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
          Debug.Log (Debug.Flow, "Sending -> " & Str(F .. L) & "<");
          Send_Message (Str(F .. L));
          Debug.Log (Debug.Flow, "Sent -> " & Str(F .. L) & "<");
          exit when L = Str'Last;
          F := L + 1;
        end loop;
    end case;
  end Put;

  -------------------------------------------
  -- Input / Output of text in stdin / stdout
  -------------------------------------------
  procedure New_Line is
  begin
    Put_Line ("");
  end New_Line;

  procedure Put_Line (Str : in String) is
  begin
    -- Send/put Lf (the Unix standard)
    Put (Str & Aski.Lf);
  end Put_Line;

  -- Callback to get text on stdin
  Stdin_Data : As.U.Asu_Us;
  function Get_Stdin_Cb (Str : in String) return Boolean is
  begin
    if Str = "" then
      -- Error or end
      Stdin_Data := As.U.Tus (Str);
      return True;
    else
      Stdin_Data := As.U.Tus (Str);
    end if;
    -- Prevent overwritting of Stdin_Data by freezing Stdin
    Async_Stdin.Activate (False);
    Debug.Log (Debug.Flow, "Get_Stdin_Cb set >" & Stdin_Data.Image & "<");
    return True;
  end Get_Stdin_Cb;

  -- Wait for Stdin data
  procedure Wait_Stdin is
    Evt : Event_Mng.Out_Event_List;
    use type Event_Mng.Out_Event_List;
  begin
    Async_Stdin.Activate (True);
    Async_Stdin.Clear_Pending;
    Async_Stdin.Clear_History;
    loop
      Stdin_Data.Set_Null;
      Debug.Log (Debug.Flow, "Waiting on stdin");
      Evt := Event_Mng.Wait (Event_Mng.Infinite_Ms);

      if Evt = Event_Mng.Fd_Event
      and then not Stdin_Data.Is_Null then
        -- New string
        exit;
      elsif Evt = Event_Mng.Signal_Event then
        -- Give up on signal
        Stdin_Data.Set_Null;
        Debug.Log (Debug.Flow, "Got signal");
        exit;
      else
        Debug.Log (Debug.Flow, "Got event " & Evt'Img);
      end if;
    end loop;
    -- Restore default behaviour for stdin
    Async_Stdin.Activate (True);
    if Stdin_Data.Is_Null then
      -- Signal event
      raise End_Error;
    end if;
  end Wait_Stdin;

  procedure Set_Echo (Echo : in Boolean) is
  begin
    if Is_Stdio then
      raise In_Stdin;
    end if;
    Get_Echo := Echo;
    -- Get / store strings (i.o. keys) by default
    Async_Stdin.Set_Async (Get_Stdin_Cb'Access, 0, 1, Get_Echo);
  end Set_Echo;

  function Get_Key return Character is
    Char : Character;
  begin
    if Is_Stdio then
      raise In_Stdin;
    end if;
    Async_Stdin.Set_Async (Get_Stdin_Cb'Access, 1, 1, Get_Echo);
    Wait_Stdin;
    Char := Stdin_Data.Element (1);
    return Char;
  end Get_Key;

  function Get_Str return String is
  begin
    if Is_Stdio then
      raise In_Stdin;
    end if;
    Async_Stdin.Set_Async (Get_Stdin_Cb'Access, 0, 1, Get_Echo);
    Wait_Stdin;
    -- Strip trailing Lf if any
    return Async_Stdin.Strip_Last_Control (Stdin_Data.Image);
  end Get_Str;

  procedure Close is
  begin
    if not Is_Stdio then
      -- Reset stdin to echo
      Debug.Log (Debug.Flow, "Stdin stream closed");
    end if;
    case Io_Mode is
      when Stdio_Tty =>
        -- Reset tty blocking
        Async_Stdin.Set_Async;
        Async_Stdin.Flush_Out;
      when File =>
        -- Close input file
        Input_Flow.Close_All;
      when Stdio_Not_Tty =>
        -- Close input flow
        Input_Flow.Close_All;
        Async_Stdin.Flush_Out;
      when Abus =>
        Bus_Subscriber.Reset;
        Bus.Reset;
      when Udp =>
        Debug.Log (Debug.Flow, "Closing udp");
        if Soc.Is_Open then
          if Socket_Is_Active then
            Activate_Socket (False);
          end if;
          Soc.Close;
        end if;
      when Tcp =>
        Debug.Log (Debug.Flow, " Closing tcp");
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
    if not Is_Stdio then
      -- Restore normal stdin
      Async_Stdin.Set_Async;
    end if;
  end Close;

  ----------------------------------------------------
  -- Bus reception callback
  ----------------------------------------------------
  overriding procedure Receive (
      Unused_Observer : in out Bus_Observer_Type;
      Unused_Subscriber : in Autobus.Subscriber_Access_Type;
      Message : in String) is
  begin
    if Message'Length = 0 then
      return;
    end if;
    -- Add this chunk
    Tmp_Data.Append (Message);
    if      Message(Message'Last) = Aski.Cr
    or else Message(Message'Last) = Aski.Lf then
      -- Validate the overall string and append it to list
      Input_Data := Tmp_Data;
      Tmp_Data.Set_Null;
      Messages.Rewind (As.U.Utils.Asu_Dyn_List_Mng.Next, False);
      Messages.Insert (Input_Data);
      Debug.Log (Debug.Flow, " Bus reception of >" & Input_Data.Image & "<");
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
    Debug.Log (Debug.Flow, "Stdin_Cb set >" & Input_Data.Image & "<");
    return True;
  end Stdin_Cb;

  ----------------------------------------------------
  -- Socket operations
  ----------------------------------------------------
  Tcp_Active : Boolean;
  function Socket_Connect_Cb (Unused_Fd   : in Event_Mng.File_Desc;
                              Unused_Read : in Boolean)
                          return Boolean is
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
    Debug.Log (Debug.Flow, "Tcp socket accepted and "
        & (if Tcp_Active then "active"
           else "inactive"));
    return False;
  end Socket_Connect_Cb;

  procedure Disconnect_Socket is
  begin
    Activate_Socket (False);
    Soc.Close;
    Open_Tcp_Socket (True);
  end Disconnect_Socket;

  function Socket_Rece_Cb (Unused_Fd   : in Event_Mng.File_Desc;
                           Unused_Read : in Boolean)
                          return Boolean is
    Message : Io_Data.Message_Type;
    Length : Natural;
  begin
    begin
      Soc_Receive (Soc, Message, Length, Io_Mode = Udp);
    exception
      when Socket.Soc_Read_0 =>
        -- Tcp Disconnection: close and accept new
        Debug.Log (Debug.Flow, "Socket_Rece_Cb disconnection");
        Disconnect_Socket;
        return False;
    end;
    if Length = 0 then
      return False;
    end if;
    -- Cancel Tcp connection when receiveing CtrlD
    if Io_Mode = Tcp
    and then Length = 1
    and then Message(1) = Aski.Eot then
      -- Tcp Disconnection: close and accept new
      Debug.Log (Debug.Flow, "Socket_Rece_Cb disconnecting");
      Disconnect_Socket;
      return False;
    end if;
    -- Add this chunk
    Tmp_Data.Append (Message(1 .. Length));
    if      Message(Length) = Aski.Cr
    or else Message(Length) = Aski.Lf then
      -- Validate the overall string
      Input_Data := Tmp_Data;
      Tmp_Data.Set_Null;
      -- Freeze fifo to prevent Input_Data to be overwritten
      Activate_Socket (False);
      Debug.Log (Debug.Flow, "Socket_Rece_Cb set >" & Input_Data.Image & "<");
      return True;
    else
      return False;
    end if;
  end Socket_Rece_Cb;

  function Socket_Is_Active return Boolean is
    (if not Soc.Is_Open and then Io_Mode = Tcp then Tcp_Active
    else Soc.Is_Open and then Event_Mng.Fd_Callback_Set (Soc.Get_Fd, True));

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
    Debug.Log (Debug.Flow, "Tcp socket open and "
        & (if And_Activate then "active"
           else "inactive"));
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
    Debug.Log (Debug.Flow, "Udp sockets open");
  end Open_Udp_Sockets;

end Io_Flow;

