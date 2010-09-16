with Ada.Characters.Latin_1, Ada.Calendar;
with Event_Mng, Ip_Addr, Socket, Tcp_Util, Input_Buffer, String_Mng,
     Unlimited_Pool, Timers;
with Debug;
package body Ios is

  ------------------------
  -- GLOBAl DEFINITIONS --
  ------------------------

  -- The port on which we accept
  Port_Def : Tcp_Util.Local_Port;
  Port_Num : Tcp_Util.Port_Num;

  -- The current connection
  Tcp_Soc : Socket.Socket_Dscr;

  -- The buffer of input flow
  Buffer : Input_Buffer.Buffer;

  -- Events
  Event : Event_Type;
  No_Event : constant Event_Type (Got_Sentence) := (Got_Sentence, Asu_Null);
  -- Event is never set with a sentence. Sentences are stored in a Fifo
  package Sentences_Mng is new Unlimited_Pool (Asu_Us, Lifo => False);
  Sentences : Sentences_Mng.Pool_Type;

  -- Global timer
  Global_Tid : Timers.Timer_Id;

  -- Lf and CrLf
  Lf : constant String := Ada.Characters.Latin_1.Lf & "";
  Crlf : constant String
       := Ada.Characters.Latin_1.Cr & Ada.Characters.Latin_1.Lf;


  ----------------------
  -- LOCAL OPERATIONS --
  ----------------------

  -- Start accepting connections
  procedure Open;

  -- Handle signal
  procedure Signal_Cb is
  begin
    Debug.Log ("Signal");
    Event := (Kind => Exit_Requested);
  end Signal_Cb;

  -- Handle global timeout expiration
  function Timer_Cb (Id : in Timers.Timer_Id;
                     Data : in Timers.Timer_Data := Timers.No_Data)
           return Boolean is
    pragma Unreferenced (Id, Data);
  begin
    Debug.Log ("Global timeout");
    if Event.Kind /= Exit_Requested
    and then Event.Kind /= Disconnection then
      Event := (Kind => Global_Timeout);
    end if;
    return True;
  end Timer_Cb;

  -- Reset event if not exit requested and not diconnection
  procedure Reset_Event is
  begin
    if Event.Kind /= Exit_Requested
    and then Event.Kind /= Disconnection then
      Event := No_event;
    end if;
  end Reset_Event;

  -- Sentence reception Cb
  procedure Sentence_Cb (Sentence : in String) is
  begin
    -- Store
    Debug.Log ("Got sentence " & Sentence);
    Sentences.Push (Asu_Tus (Sentence));
  end Sentence_Cb;

  -- Message reception Cb
  subtype Message_Type is String (1 .. 1024);
  package My_Rece is new Tcp_Util.Reception (Message_Type);
  procedure Read_Cb (Dscr : in Socket.Socket_Dscr;
                     Msg : in Message_Type;
                     Len : in Natural) is
    pragma Unreferenced (Dscr);
  begin
    Debug.Log ("Read " & Msg(1 .. Len));
    Buffer.Push (String_Mng.Replace (Msg(1 .. Len), Crlf, Lf));
  end Read_Cb;

 -- When Soc_Read_0
  procedure Discon_Cb (Dscr : in Socket.Socket_Dscr) is
    pragma Unreferenced (Dscr);
  begin
    Debug.Log ("Disconnected");
    -- Tcp_Util closes the socket
    Tcp_Soc := Socket.No_Socket;
    -- Signal
    if Event.Kind /= Exit_Requested then
      Event := (Kind => Disconnection);
    end if;
  end Discon_Cb;

  -- Connection acception Cb
  procedure Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                       Local_Dscr      : in Socket.Socket_Dscr;
                       Remote_Port_Num : in Tcp_Util.Port_Num;
                       Remote_Host_Id  : in Tcp_Util.Host_Id;
                       New_Dscr        : in Socket.Socket_Dscr) is
    pragma Unreferenced (Local_Port_Num, Local_Dscr, Remote_Port_Num,
                         Remote_Host_Id);
    use type Socket.Socket_Dscr;
  begin
    Tcp_Soc := New_Dscr;
    Tcp_Soc.Set_Blocking (False);
    My_Rece.Set_Callbacks (Tcp_Soc, Read_Cb'Unrestricted_Access,
                                  Discon_Cb'Unrestricted_Access);
    Debug.Log ("Connected");
    -- Only one connection at a time
    Tcp_Util.Abort_Accept (Socket.Tcp, Port_Num);
  end Accept_Cb;

  -- Start accepting connections
  procedure Open is
    Acc_Soc : Socket.Socket_Dscr;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    -- Reset caches
    Buffer.Set (Sentence_Cb'Access);
    Event := No_Event;
    Sentences.Clear;
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
          Debug.Log ("Cannot accept, maybe close-wait. Waiting.");
          Event_Mng.Wait (20_000);
      end;
    end loop;
  end Open;

  -- Wait during Timeout or up to significant event
  -- Possibly stop waiting if a sentence is ready
  procedure Wait_Event (Timeout_Ms : in Integer;
                       Stop_On_Sentence : in Boolean) is
    Exp : Timers.Delay_Rec(Timers.Delay_Exp);
    Evt : Event_Mng.Out_Event_List;
    use type Ada.Calendar.Time, Event_Mng.Out_Event_List;
  begin
    Exp.Expiration_Time := Ada.Calendar.Clock + Duration(Timeout_Ms) / 1_000.0;
    loop
      -- Wait until timeout or an event
      exit when Event /= No_Event;
      -- On option wait until sentence ready
      exit when Stop_On_Sentence and then not Sentences.Is_Empty;
      Evt := Event_Mng.Wait (Exp);
      exit when Evt = Event_Mng.No_Event;
    end loop;
    if Event /= No_Event then
      Debug.log ("Wait event exiting cause event is " & Event.Kind'Img);
    elsif Stop_On_Sentence and then not Sentences.Is_Empty then
      Debug.log ("Wait event exiting cause sentence got");
    else
      Debug.log ("Wait event exiting cause Evt is " & Evt'Img);
    end if;
  end Wait_Event;

  -----------------------
  -- PUBLIC OPERATIONS --
  -----------------------

  -- Init the accepting of connections on port (name or num)
  procedure Init (Port : in Asu_Us) is
    Remote_Port_Def : Tcp_Util.Remote_Port;

    use type Tcp_Util.Remote_Port_List;
  begin
    -- Parse port name or num
    Remote_Port_Def := Ip_Addr.Parse (Asu_Ts (Port));
    if Remote_Port_Def.Kind = Tcp_Util.Port_Name_Spec then
      Port_Def := (Tcp_Util.Port_Name_Spec, Remote_Port_Def.Name);
    else
      Port_Def := (Tcp_Util.Port_Num_Spec, Remote_Port_Def.Num);
    end if;

    -- Init Cbs
    Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Access);
    Open;
  exception
    when others =>
      raise Init_Error;
  end Init;

  -- Arm / cancel global timer
  procedure Start_Global_Timer (Timeout_Ms : Integer) is
    Exp : Timers.Delay_Rec(Timers.Delay_Sec);
  begin
    Exp.Delay_Seconds := Duration(Timeout_Ms) / 1_000.0;
    Global_Tid := Timers.Create (Exp, Timer_Cb'Access);
  end Start_Global_Timer;

  procedure Stop_Global_Timer is
    use type Timers.Timer_Id;
  begin
    if Global_Tid /= Timers.No_Timer then
      Timers.Delete (Global_Tid);
      Global_Tid := Timers.No_Timer;
    end if;
  end Stop_Global_Timer;

  -- Wait during Timeout (or up to global timeout or disconnection)
  function Wait (Timeout_Ms : Integer) return Event_Type is
    Loc_Event : Event_Type;
  begin
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
  function Read (Timeout_Ms : in Integer) return Event_Type is
    Loc_Event : Event_Type;
  begin
    Wait_Event (0, False);
    if Event /= No_Event then
      -- Something occured
      Loc_Event := Event;
      Reset_Event;
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
    return Loc_Event;
  end Read;

  -- Send a sentence. Disconnection if error or overflow
  procedure My_Send is new Socket.Send (Message_Type);
  procedure Send (Text : in Asu_Us;
                  Disconnection : out Boolean) is
    Txt : Asu_Us;
    Len : Natural;
    Msg : Message_Type;
  begin
    if not Tcp_Soc.Is_Open then
      Disconnection := True;
      return;
    end if;
    Disconnection := False;
    Txt := Text;
    -- Send slices of Message_Type
    loop
      Len := Asu.Length (Txt);
      exit when Len = 0;
      if Len > Message_Type'Length then
        Len := Message_Type'Length;
      end if;
      Msg(1 .. Len) := Asu.Slice (Txt, 1, Len);
      Asu.Delete (Txt, 1, Len);
      My_Send (Tcp_Soc, Msg, Len);
    end loop;
  exception
    when Socket.Soc_Would_Block =>
      Debug.Log ("Overflow");
      Disconnection := True;
    when Socket.Soc_Conn_Lost =>
      Debug.Log ("Lost connection");
      Disconnection := True;
  end Send;

  -- Close and re-open
  procedure Reset is
  begin
    Close;
    Open;
  end Reset;

  -- Close
  procedure Close is
  begin
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
    Stop_Global_Timer;
 end Close;

end Ios;

