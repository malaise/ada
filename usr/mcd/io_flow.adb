with Ada.Characters.Latin_1, Ada.Exceptions;
with Argument, Event_Mng, Sys_Calls, Async_Stdin, Text_Line,
     Socket, Socket_Util,Tcp_Util, Ip_Addr;
with Fifos;
with Debug, Io_Data;
package body Io_Flow is

  -- Io mode
  type Io_Mode_List is (
    Unknown,
    Stdio_Tty,
    Stdio_Not_Tty,
    Udp,
    Tcp,
    Fifo);
  Io_Mode : Io_Mode_List := Unknown;

  -- Data read
  Input_Data : Asu_Us;

  -- Tempo data until Line Feed
  Tmp_Data : Asu_Us;

  -- Udp or tcp socket
  Host : Tcp_Util.Remote_Host;
  Port : Tcp_Util.Remote_Port;
  Is_Ipm : Boolean;
  Soc : Socket.Socket_Dscr;
  Accepting_Soc : Socket.Socket_Dscr;
  function Socket_Is_Active return Boolean;
  procedure Activate_Socket (Active : in Boolean);
  procedure Open_Udp_Socket (And_Activate : in Boolean);
  procedure Open_Tcp_Socket (And_Activate : in Boolean);
  procedure Soc_Send is new Socket.Send (Io_Data.Message_Type);
  procedure Soc_Receive is new Socket.Receive (Io_Data.Message_Type);

  -- Fifo
  Fifo_Name : Asu_Us;
  package Mcd_Fifos is new Fifos.Fifo (Io_Data.Message_Type);
  Acc_Id, Client_Id : Mcd_Fifos.Fifo_Id := Mcd_Fifos.No_Fifo;
  procedure Open_Fifo (Active : in Boolean);

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

    -- Get fifo name argument if set
    if Argument.Is_Set (1, "f") then
      if Argument.Is_Set (2, "f") or else Io_Mode /= Unknown then
        Async_Stdin.Put_Line_Err ("Too many options.");
        raise Init_Error;
      end if;
      Fifo_Name := Asu_Tus (Argument.Get_Parameter (1, "f"));
      if Asu_Is_Null (Fifo_Name) then
        Async_Stdin.Put_Line_Err ("Missing fifo name.");
        raise Init_Error;
      end if;
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Init on fifo " & Asu_Ts (Fifo_Name));
      end if;
      Open_Fifo (True);
      Io_Mode := Fifo;
    end if;

    -- Get tcp port if set
    if Argument.Is_Set (1, "t") then
      if Argument.Is_Set (2, "t") or else Io_Mode /= Unknown then
        Async_Stdin.Put_Line_Err ("Too many options.");
        raise Init_Error;
      end if;
      -- Parse spec
      begin
        Port := Ip_Addr.Parse (Argument.Get_Parameter (1, "t"));
      exception
        when Ip_Addr.Parse_Error =>
          Async_Stdin.Put_Line_Err ("Invalid tcp port");
          raise Init_Error;
      end;
      Open_Tcp_Socket (True);
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Init on tcp port " &
          Ip_Addr.Image (Accepting_Soc.Get_Linked_To));
      end if;
      Io_Mode := Tcp;
      return;
    end if;

    -- Get udp/ipm spec if set
    if Argument.Is_Set (1, "u") then
      if Argument.Is_Set (2, "u") or else Io_Mode /= Unknown then
        Async_Stdin.Put_Line_Err ("Too many options.");
        raise Init_Error;
      end if;
      -- Parse spec
      begin
        Ip_Addr.Parse (Argument.Get_Parameter (1, "u"), Host, Port);
      exception
        when Ip_Addr.Parse_Error =>
          Async_Stdin.Put_Line_Err ("Invalid udp spec");
          raise Init_Error;
      end;
      Open_Udp_Socket (True);
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Err ("Flow: Init on ");
        if Is_Ipm then
          Async_Stdin.Put_Err ("ipm LAN " &
            Ip_Addr.Image (Socket.Id2Addr (Soc.Get_Destination_Host)) & " ");
        else
          Async_Stdin.Put_Err ("udp ");
        end if;
        Async_Stdin.Put_Line_Err ("port " &
          Ip_Addr.Image (Soc.Get_Linked_To));
      end if;
      Io_Mode := Udp;
      return;
    end if;

    -- If no arg => Stdin
    if Argument.Get_Nbre_Arg /= 0 then
      Async_Stdin.Put_Line_Err ("Invalid argument.");
      raise Init_Error;
    end if;
    if Io_Mode /= Unknown then
      return;
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

  ----------------------------------------------------
  -- Get data from fifo, tcp, udp or stdin (async or not)
  ----------------------------------------------------
  procedure Next_Line (Str : out Asu_Us) is
    Evt : Event_Mng.Out_Event_List;
    Len : Natural;
    use type Event_Mng.Out_Event_List;
    use type Mcd_Fifos.Fifo_Id;
  begin
    if Io_Mode = Stdio_Not_Tty then
      Input_Data := Asu_Null;
      -- Get next non empty line from Stdin (not a tty)
      loop
        -- Get next line
        Input_Data := Input_Flow.Get;
        -- End of flow when got an empty line
        Len := Asu.Length (Input_Data);
        exit when Len = 0;
        -- Remove trailing Line_Feed
        if Asu.Element (Input_Data, Len) = Text_Line.Line_Feed_Char then
          Asu.Delete (Input_Data, Len, Len);
          Len := Len - 1;
        end if;
        -- This line is Ok if not empty
        exit when Len /= 0;
      end loop;
    else
      -- Get next data on async stdin, socket or Fifo
      loop
        Input_Data := Asu_Null;
        if Debug.Debug_Level_Array(Debug.Flow) then
          Async_Stdin.Put_Line_Err ("Flow: Waiting on fifo/socket/tty");
        end if;
        Evt := Event_Mng.Wait (Event_Mng.Infinite_Ms);

        if Evt = Event_Mng.Fd_Event
        and then not Asu_Is_Null (Input_Data) then
          -- New string
          exit;
        elsif Evt = Event_Mng.Signal_Event then
          -- Give up on signal
          Input_Data := Asu_Null;
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
      when Fifo =>
        if Client_Id /= Mcd_Fifos.No_Fifo then
          Mcd_Fifos.Activate (Client_Id, True);
        end if;
      when Udp | Tcp =>
        Activate_Socket (True);
      when others =>
        null;
    end case;
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Line_Err ("Flow: Next_Line -> " & Asu_Ts (Str));
    end if;
  end Next_Line;

  ----------------------------------------------------
  -- Pet data on fifo, tcp, udp or stdin (async or not)
  ----------------------------------------------------
  -- Send one message on socket or FIFO
  Message_To_Put : Io_Data.Message_Type;
  procedure Send_Message (Str : in String) is
    Active : Boolean;
    Res : Fifos.Send_Result_List;
    use type Fifos.Send_Result_List;
  begin
    Message_To_Put (1 .. Str'Length) := Str;
    if Io_Mode = Fifo then
      Active := Mcd_Fifos.Is_Active (Client_Id);
      Res := Mcd_Fifos.Send (Client_Id, Message_To_Put, Str'Length);
      if Res /= Fifos.Ok and then Res /= Fifos.Overflow then
        Mcd_Fifos.Close (Client_Id);
        Open_Fifo (Active);
      end if;
    else
      Active := Socket_Is_Active;
      Soc_Send (Soc, Message_To_Put, Str'Length);
    end if;
  exception
    when Fifos.In_Overflow =>
      -- FIFO
      Mcd_Fifos.Close (Client_Id);
      Open_Fifo (Active);
    when Socket.Soc_Would_Block =>
      -- TCP
      Activate_Socket (False);
      Soc.Close;
      Open_Tcp_Socket (Active);
  end Send_Message;

  -- Put or send string
  procedure Put (Str : in String) is
    F, L : Natural;
    use type Mcd_Fifos.Fifo_Id;
  begin
    case Io_Mode is
      when Unknown =>
        null;
      when Stdio_Tty | Stdio_Not_Tty =>
        -- Put on stdout (tty or not)
        Async_Stdin.Put_Out (Str);
      when Udp | Tcp | Fifo =>
        -- Skip if channel is not active
        if Io_Mode = Fifo and then Client_Id /= Mcd_Fifos.No_Fifo then
          return;
        elsif (Io_Mode = Udp or else Io_Mode = Tcp)
        and then not Soc.Is_Open then
          return;
        end if;
        -- Send on Socket/Fifo several messages
        F := Str'First;
        loop
          L := F + Io_Data.Max_Message_Len - 1;
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

  Closing : Boolean := False;
  procedure Close is
    use type Mcd_Fifos.Fifo_Id;
  begin
    Closing := True;
    case Io_Mode is
      when Stdio_Tty =>
        -- Reset tty blocking
        Async_Stdin.Set_Async;
      when Stdio_Not_Tty =>
        -- Close input flow
        Input_Flow.Close;
      when Fifo =>
        if Debug.Debug_Level_Array(Debug.Flow) then
          Async_Stdin.Put_Line_Err ("Flow: Closing fifo");
        end if;
        if Client_Id /= Mcd_Fifos.No_Fifo then
          Mcd_Fifos.Close (Client_Id);
        end if;
        if Acc_Id /= Mcd_Fifos.No_Fifo then
          Mcd_Fifos.Close (Acc_Id);
        end if;
        if Debug.Debug_Level_Array(Debug.Flow) then
          Async_Stdin.Put_Line_Err ("Flow: Closed fifo");
        end if;
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
  -- Fifo operations
  ----------------------------------------------------
  procedure Fifo_Conn_Cb (Fifo_Name : in String;
                          Id        : in Mcd_Fifos.Fifo_Id;
                          Connected : in Boolean) is
    pragma Unreferenced (Fifo_Name);
    Active : Boolean;
  begin
    if Connected then
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Client accepted");
      end if;
      -- Accept one client and stop accepting others
      Mcd_Fifos.Close (Acc_Id);
      Client_Id := Id;
    else
      -- Client disconnected, allow new client (except if we are closing)
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Client has disconnected");
      end if;
      Active := Mcd_Fifos.Is_Active (Client_Id);
      Client_Id := Mcd_Fifos.No_Fifo;
      if not Closing then
        Open_Fifo (Active);
      end if;
    end if;
  end Fifo_Conn_Cb;

  procedure Fifo_Rece_Cb (Id      : in Mcd_Fifos.Fifo_Id;
                          Message : in Io_Data.Message_Type;
                          Length  : in Fifos.Message_Length) is
    pragma Unreferenced (Id);
  begin
    if Length = 0 then
      return;
    end if;
    -- Add this chunk
    Asu.Append (Tmp_Data, Message(1 .. Length));
    if      Message(Length) = Ada.Characters.Latin_1.Cr
    or else Message(Length) = Ada.Characters.Latin_1.Lf then
      -- Validate the overall string
      Input_Data := Tmp_Data;
      Tmp_Data := Asu_Null;
      -- Freeze fifo to prevent Input_Data to be overwritten
      Mcd_Fifos.Activate (Client_Id, False);
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Fifo_Rece_Cb set >"
                             & Asu_Ts (Input_Data) & "<");
      end if;
    end if;
  end Fifo_Rece_Cb;

  procedure Open_Fifo (Active : in Boolean) is
  begin
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Line_Err ("Flow: Opening fifo " & Asu_Ts (Fifo_Name));
    end if;
    Acc_Id := Mcd_Fifos.Open (Asu_Ts (Fifo_Name),
                              False,
                              Fifo_Conn_Cb'Access,
                              Fifo_Rece_Cb'Access,
                              null);
    if not Active then
      Mcd_Fifos.Activate (Acc_Id, False);
    end if;
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Line_Err ("Flow: Fifo open");
    end if;
  exception
    when Error:others =>
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Fifo open error "
                            & Ada.Exceptions.Exception_Name (Error));
      end if;
      raise Fifo_Error;
  end Open_Fifo;

  ----------------------------------------------------
  -- Async stdin callback
  ----------------------------------------------------
  function Stdin_Cb (Str : in String) return Boolean is
    use type Mcd_Fifos.Fifo_Id;
  begin
    if Str = "" then
      -- Error or end
      Input_Data := Asu_Tus (Str);
      return True;
    else
      Input_Data := Asu_Tus (Str);
    end if;
    -- Prevent overwritting of Input_Data by freezing Stdin
    Async_Stdin.Activate (False);
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Line_Err ("Flow: Stdin_Cb set >"
                           & Asu_Ts (Input_Data) & "<");
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
      Soc_Receive (Soc, Message, Length, Io_Mode = Udp, False);
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
    Asu.Append (Tmp_Data, Message(1 .. Length));
    if      Message(Length) = Ada.Characters.Latin_1.Cr
    or else Message(Length) = Ada.Characters.Latin_1.Lf then
      -- Validate the overall string
      Input_Data := Tmp_Data;
      Tmp_Data := Asu_Null;
      -- Freeze fifo to prevent Input_Data to be overwritten
      Activate_Socket (False);
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Socket_Rece_Cb set >"
                             & Asu_Ts (Input_Data) & "<");
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

  procedure Open_Udp_Socket (And_Activate : in Boolean) is
    use type Tcp_Util.Remote_Host_List, Tcp_Util.Remote_Port_List;
  begin
    Soc.Open (Socket.Udp);
    Is_Ipm := False;
    if Host.Kind = Tcp_Util.Host_Id_Spec
    or else Asu_Is_Null (Host.Name) then
      -- An address specified => Ipm
      -- Use Set_Dest to indicate Imp address
      Socket_Util.Set_Destination (Soc, True, Host, Port);
      Is_Ipm := True;
    end if;
    Socket_Util.Link (Soc, Port);
    if And_Activate then
      Activate_Socket (True);
    end if;
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Err ("Flow: Udp socket open and ");
      if And_Activate then
        Async_Stdin.Put_Line_Err ("active");
      else
        Async_Stdin.Put_Line_Err ("inactive");
      end if;
    end if;
  end Open_Udp_Socket;

end Io_Flow;

