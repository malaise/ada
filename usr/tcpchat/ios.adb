with Ada.Calendar;
with Aski, Event_Mng, Ip_Addr, Socket, Socket_Util, Tcp_Util, Input_Buffer,
     Str_Util, Unlimited_Pool, Timers, Async_Stdin;
with Debug, Tree;
package body Ios is

  ------------------------
  -- GLOBAl DEFINITIONS --
  ------------------------

  -- Is communication on stdio or tcp
  Stdio : Boolean;

  -- The port on which we accept
  Port_Def : Socket_Util.Local_Port;
  Port_Num : Socket_Util.Port_Num;

  -- The current connection
  Tcp_Soc : Socket.Socket_Dscr;

  -- The buffer of input flow
  Buffer : Input_Buffer.Buffer;

  -- Events
  Event : Event_Type;
  No_Event : constant Event_Type (Got_Sentence)
           := (Got_Sentence, As.U.Asu_Null);
  -- Event is never set with a sentence. Sentences are stored in a Fifo
  package Sentences_Manager is new Unlimited_Pool (As.U.Asu_Us, Lifo => False);
  package Sentences_Mng renames Sentences_Manager.Upool;
  Sentences : Sentences_Mng.Pool_Type;

  -- Global timer
  Global_Tid : Timers.Timer_Id;

  -- Lf and CrLf
  Lf : String renames Aski.Lf_S;
  Crlf : constant String := Aski.Cr & Aski.Lf;

  ----------------------
  -- LOCAL OPERATIONS --
  ----------------------

  -- Start accepting connections
  procedure Open;

  -- Handle signal
  procedure Signal_Cb is
  begin
    Debug.Logger.Log_Debug ("Signal");
    Event := (Kind => Exit_Requested);
  end Signal_Cb;

  -- Handle global timeout expiration
  function Timer_Cb (Unused_Id   : Timers.Timer_Id;
                     Unused_Data : Timers.Timer_Data) return Boolean is
  begin
    Debug.Logger.Log_Debug ("Global timeout");
    if Event.Kind /= Exit_Requested
    and then Event.Kind /= Disconnection then
      Event := (Kind => Global_Timeout);
    end if;
    return True;
  end Timer_Cb;

  -- Reset event if not exit requested and not disconnection
  procedure Reset_Event is
  begin
    if Event.Kind /= Exit_Requested
    and then Event.Kind /= Disconnection then
      Event := No_Event;
    end if;
  end Reset_Event;

  -- Sentence reception Cb
  procedure Sentence_Cb (Sentence : in String) is
  begin
    -- Store sentence without Lf
    Debug.Logger.Log_Debug ("Got sentence " & Sentence);
    Sentences.Push (As.U.Tus (Str_Util.Substit (Sentence, Lf, "")));
  end Sentence_Cb;

  -- Message reception Cb
  subtype Message_Type is String (1 .. 1024);
  package My_Rece is new Tcp_Util.Reception (Message_Type);
  function Read_Cb (Unused_Dscr : Socket.Socket_Dscr;
                    Msg  : Message_Type;
                    Len  : Natural) return Boolean is
  begin
    Debug.Logger.Log_Debug ("Read_Cb >" & Msg(1 .. Len) & "<");
    Buffer.Push (Str_Util.Substit (Msg(1 .. Len), Crlf, Lf));
    return True;
  end Read_Cb;

 -- When Soc_Read_0
  procedure Discon_Cb (Unused_Dscr : in Socket.Socket_Dscr) is
  begin
    Debug.Logger.Log_Debug ("Disconnected");
    -- Tcp_Util closes the socket
    Tcp_Soc := Socket.No_Socket;
    -- Signal
    if Event.Kind /= Exit_Requested then
      Event := (Kind => Disconnection);
    end if;
  end Discon_Cb;

  -- Connection acception Cb
  procedure Accept_Cb (Unused_Local_Port_Num  : in Socket_Util.Port_Num;
                       Unused_Local_Dscr      : in Socket.Socket_Dscr;
                       Unused_Remote_Host_Id  : in Socket_Util.Host_Id;
                       Unused_Remote_Port_Num : in Socket_Util.Port_Num;
                       New_Dscr               : in Socket.Socket_Dscr) is
  begin
    Tcp_Soc := New_Dscr;
    Tcp_Soc.Set_Blocking (Socket.Blocking_Send);
    My_Rece.Set_Callbacks (Tcp_Soc, Read_Cb'Unrestricted_Access,
                                  Discon_Cb'Unrestricted_Access);
    Debug.Logger.Log_Debug ("Connected");
    -- Only one connection at a time
    Tcp_Util.Abort_Accept (Socket.Tcp, Port_Num);
  end Accept_Cb;

  function Async_Cb (Str : String) return Boolean is
  begin
    if Str = "" then
      -- Async_Stdin error: likely end of input
      Debug.Logger.Log_Debug ("Async_Cb error");
      if Event.Kind /= Exit_Requested then
        Event := (Kind => Input_Error);
      end if;
    else
      Debug.Logger.Log_Debug ("Async_Cb >" & Str & "<");
      Buffer.Push (Str);
    end if;
    return True;
  end Async_Cb;

  -- Start accepting connections
  procedure Open is
    Acc_Soc : Socket.Socket_Dscr;
    Dummy : Boolean;
  begin
    -- Reset caches
    Buffer.Set (Sentence_Cb'Access);
    Event := No_Event;
    Sentences.Clear;

    if Stdio then
      if not Async_Stdin.Is_Set then
        -- First Open
        Async_Stdin.Set_Async (Async_Cb'Access, 256);
      end if;
      return;
    end if;

    -- Accept connections
    loop
      begin
        Tcp_Util.Accept_From (Socket.Tcp,
                              Port_Def,
                              Accept_Cb'Unrestricted_Access,
                              Acc_Soc,
                              Port_Num);
        exit;
      exception
        when Socket.Soc_Addr_In_Use =>
          Debug.Logger.Log_Debug ("Cannot accept, maybe close-wait. Waiting.");
          Event_Mng.Wait (20_000);
      end;
    end loop;
  end Open;

  -- Clear for re-open
  procedure Clear is
  begin
    Stop_Global_Timer;

    if Stdio then
      if Async_Stdin.Is_Set then
        Async_Stdin.Clear_Pending;
      end if;
      return;
    end if;

    begin
      Tcp_Util.Abort_Accept (Socket.Tcp, Port_Num);
    exception
      when Tcp_Util.No_Such =>
        null;
    end;
    if Tcp_Soc.Is_Open then
      begin
        My_Rece.Remove_Callbacks (Tcp_Soc);
      exception
       when Tcp_Util.No_Such =>
         null;
      end;
    end if;
    if Tcp_Soc.Is_Open then
      Tcp_Soc.Close;
    end if;
 end Clear;

  -- Wait during Timeout or up to significant event
  -- Possibly stop waiting if a sentence is ready
  procedure Wait_Event (Timeout_Ms : in Integer;
                       Stop_On_Sentence : in Boolean) is
    Wait_Def : Timers.Delay_Rec;
    Inf : Timers.Delay_Rec(Timers.Delay_Sec);
    Exp : Timers.Delay_Rec(Timers.Delay_Exp);
    Evt : Event_Mng.Out_Event_List;
    use type Ada.Calendar.Time, Event_Mng.Out_Event_List;
  begin
    -- Set infinite timeout sec or compute expiration time
    if Timeout_Ms = Tree.Infinite_Ms then
      Inf.Delay_Seconds := Timers.Infinite_Seconds;
      Wait_Def := Inf;
    else
      Exp.Expiration_Time := Ada.Calendar.Clock
                           + Duration(Timeout_Ms) / 1_000.0;
      Wait_Def := Exp;
    end if;

    loop --## rule line off Loop_While
      -- Wait until timeout or an event
      exit when Event /= No_Event;
      -- On option wait until sentence ready
      -- Event = No_Event so Evnt_Type = Got_Sentence
      exit when Stop_On_Sentence and then not Sentences.Is_Empty;
      Evt := Event_Mng.Wait (Wait_Def);
      exit when Evt = Event_Mng.Timeout;
    end loop;
    if Event /= No_Event then
      Debug.Logger.Log_Debug ("Wait event exiting cause event is "
                            & Event.Kind'Img);
    elsif Stop_On_Sentence and then not Sentences.Is_Empty then
      Debug.Logger.Log_Debug ("Wait event exiting cause sentence got");
    else
      Debug.Logger.Log_Debug ("Wait event exiting cause Evt is " & Evt'Img);
    end if;
  end Wait_Event;

  -----------------------
  -- PUBLIC OPERATIONS --
  -----------------------

  -- Init the accepting of connections on port (name or num)
  procedure Init (Port : in As.U.Asu_Us) is
    Remote_Port_Def : Socket_Util.Remote_Port;

    use type Socket_Util.Remote_Port_List;
  begin

    Stdio := Port.Image = "-";
    if not Stdio then
      -- Parse port name or num
      Remote_Port_Def := Ip_Addr.Parse (Port.Image);
      if Remote_Port_Def.Kind = Socket_Util.Port_Name_Spec then
        Port_Def := (Socket_Util.Port_Name_Spec, Remote_Port_Def.Name);
      else
        Port_Def := (Socket_Util.Port_Num_Spec, Remote_Port_Def.Num);
      end if;
    end if;

    -- Init Cbs
    Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Access);
    Open;

    Debug.Logger.Log_Debug ("Ios open OK");
  exception
    when others =>
      raise Init_Error;
  end Init;

  -- Arm / cancel global timer
  procedure Start_Global_Timer (Timeout_Ms : Integer) is
    Exp : Timers.Delay_Rec(Timers.Delay_Sec);
  begin
    if Timeout_Ms /= Tree.Infinite_Ms then
      Exp.Delay_Seconds := Duration(Timeout_Ms) / 1_000.0;
      Global_Tid.Create (Exp, Timer_Cb'Access);
    end if;
  end Start_Global_Timer;

  procedure Stop_Global_Timer is
  begin
    Global_Tid.Delete_If_Exists;
  end Stop_Global_Timer;

  -- Wait during Timeout (or up to global timeout or disconnection)
  function Wait (Timeout_Ms : Integer) return Event_Type is
    Loc_Event : Event_Type;
  begin
    Debug.Logger.Log_Debug ("Wait " & Timeout_Ms'Img);
    Wait_Event (Timeout_Ms, False);
    if Event /= No_Event then
      -- Something occured
      Loc_Event := Event;
    else
      Loc_Event := (Kind => Local_Timeout);
    end if;
    Reset_Event;
    return Loc_Event;
  end Wait;

  -- Read next sentence. Wait up to Timeout
  -- If Disconnection, other parameters are not significant
  -- Elsif Timeout, other parameters is not significant
  -- Else Text is significant
  function Read (Timeout_Ms : Integer) return Event_Type is
    Loc_Event : Event_Type;
  begin
    Debug.Logger.Log_Debug ("Read " & Timeout_Ms'Img);
    Wait_Event (0, True);
    if Event /= No_Event then
      -- Something occured
      Loc_Event := Event;
      Reset_Event;
      Debug.Logger.Log_Debug ("Read past event " & Loc_Event.Kind'Img);
      return Loc_Event;
    end if;
    Wait_Event (Timeout_Ms, True);
    if Event /= No_Event then
      -- Something occured
      Loc_Event := Event;
    elsif not Sentences.Is_Empty then
      -- A sentence has been received
      Sentences.Pop (Loc_Event.Sentence);
    else
      -- Local timeout
      Loc_Event := (Kind => Local_Timeout);
    end if;
    Reset_Event;
    Debug.Logger.Log_Debug ("Read new event " & Loc_Event.Kind'Img);
    return Loc_Event;
  end Read;

  -- Send a sentence. Disconnection if error, timeout or overflow
  function My_Send is new Tcp_Util.Send (Message_Type);
  procedure Send (Text : in As.U.Asu_Us;
                  Disconnection : out Boolean) is
    Txt : As.U.Asu_Us;
    Len : Natural;
    Msg : Message_Type;
    Dummy : Boolean;
  begin
    Debug.Logger.Log_Debug ("Send "
                          & Str_Util.Substit (Text.Image, Lf, "[LF]"));
    if Stdio then
      Async_Stdin.Put_Out (Text.Image);
      Disconnection := False;
      return;
    end if;
    if not Tcp_Soc.Is_Open then
      Disconnection := True;
      return;
    end if;
    Disconnection := False;
    Txt := Text;
    -- Send slices of Message_Type, blocking with timeout
    loop
      Len := Txt.Length;
      exit when Len = 0;
      if Len > Message_Type'Length then
        Len := Message_Type'Length;
      end if;
      Msg(1 .. Len) := Txt.Slice (1, Len);
      Txt.Delete (1, Len);
      Dummy := My_Send (Tcp_Soc, null, null, 0.1, Msg, Len);
    end loop;
  exception
    when Socket.Soc_Conn_Lost =>
      Debug.Logger.Log_Debug ("Lost connection");
      Disconnection := True;
    when Tcp_Util.Timeout_Error =>
      Debug.Logger.Log_Debug ("Timeout");
      Disconnection := True;
    when Async_Stdin.Io_Error =>
      raise Output_Error;
  end Send;

  -- Clear and re-open
  procedure Reset is
  begin
    Clear;
    Open;
  end Reset;

  -- Final close
  procedure Close is
  begin
    Clear;
    if Stdio then
      Async_Stdin.Set_Async;
    end if;
 end Close;

end Ios;

