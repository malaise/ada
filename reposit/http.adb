with Ada.Characters.Latin_1, Ada.Calendar;
with Trace.Loggers, Environ, Str_Util, Parser,
     Event_Mng, Timers, Ip_Addr, Socket, Tcp_Util, Mutex_Manager;
package body Http is

  -- The Mutex of exclusive execution
  Mut : Mutex_Manager.Mutex (Mutex_Manager.Simple, True);

  -- Debug
  Logger : Trace.Loggers.Logger;

  -- Timeout definition
  Timeout_Var : constant String := "HTTP_TIMEOUT_MS";
  The_Timeout_Ms : Integer;
  The_Delay : Timers.Delay_Rec (Timers.Delay_Exp);

  -- Each connection try timeout definition
  Default_Connect_Timeout_Ms : constant Integer := 3000;
  Connect_Timeout_Var : constant String := "HTTP_CONNECT_TIMEOUT_MS";

  -- End of processing (Error, disconnection or timeout)
  Done : Boolean;

  -- Result to return
  Result : Result_Type;

  -- The request
  Host : Tcp_Util.Remote_Host;
  Port : Tcp_Util.Remote_Port;
  Request : As.U.Asu_Us;

  -- The socket
  Soc : Socket.Socket_Dscr;

  -- Buffer of incoming data
  Buffer : As.U.Asu_Us;

  -- Line terminators
  Lf : constant String := Ada.Characters.Latin_1.Lf & "";
  Cr : constant String := Ada.Characters.Latin_1.Cr & "";
  Crlf : constant String := Cr & Lf;

  -- The timeout for sending request
  Send_Timeout : Duration;

  -- Send/receive Msg
  -- Shall be large enough to receive the whole reply header
  subtype Message_Type is String (1 .. 1024 * 1024);
  function My_Send is new Tcp_Util.Send (Message_Type);
  package My_Rece is new Tcp_Util.Reception (Message_Type);

  -- Timer Cb
  Timer_Id : Timers.Timer_Id;
  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Id, Data);
  begin
    Logger.Log_Debug ("HTTP: Timeout");
    Result := (Client_Error, Timeout);
    Done := True;
    return True;
  end Timer_Cb;

  -- For parsing status line of reply
  function Is_Space (C : Character) return Boolean is
  begin
    return C = ' ';
  end Is_Space;

  -- Message reception Cb, until disconnection
  function Read_Cb (Dscr : Socket.Socket_Dscr;
                    Msg : Message_Type;
                    Len : Natural) return Boolean is
    pragma Unreferenced (Dscr);
  begin
    Logger.Log_Debug ("HTTP: Reading");
    Buffer.Append (Msg(1 .. Len));
    return False;
  end Read_Cb;

  -- Check received Data, set Result
  Expected_Length : Natural;
  procedure Check is
    New_Line : As.U.Asu_Us;
    Ind : Natural;
    Header : As.U.Asu_Us;
  begin
    Logger.Log_Debug ("HTTP: Checking");

    -- Parse buffer, init default result
    Result := (Client_Error, Invalid_Answer);

    if Buffer.Is_Null then
      Logger.Log_Debug ("HTTP: No anwser at all");
      return;
    end if;

    -- See if line break is Lf or Cr+Lf, set New_Line
    Ind := Str_Util.Locate (Buffer.Image, Lf);
    if Ind = 0 then
      Logger.Log_Debug ("HTTP: No line feed at all");
      return;
    end if;
    if Ind = 1 or else Buffer.Element (Ind - 1) & "" /= Cr then
      New_Line := As.U.Tus (Lf);
    else
      New_Line := As.U.Tus (Crlf);
    end if;

    -- Locate end of header: 2 consecutive New_Lines, Isolate header
    Ind := Str_Util.Locate (Buffer.Image, New_Line.Image & New_Line.Image);
    if Ind /= 0 then
      Header := Buffer.Uslice (1, Ind - 1);
      Ind := Ind + 2 * New_Line.Length;
      Buffer.Delete (1, Ind - 1);
    else
      -- Empty Buffer
      Header := Buffer;
      Buffer.Set_Null;
      Logger.Log_Debug ("HTTP: No header delimiter: " & Header.Image);
    end if;
    -- CrLf -> Lf in Header
    Header := As.U.Tus (Str_Util.Substit (Header.Image, New_Line.Image, Lf));

    -- First line of header reply: status
    Ind := Str_Util.Locate (Header.Image, Lf);
    if Ind = 0 then
      Logger.Log_Debug ("HTTP: No header line feed: " & Header.Image);
      return;
    end if;
    declare
      Iter : Parser.Iterator;
      Word : As.U.Asu_Us;
      Http_Header : constant String := "HTTP/";
    begin
      Iter.Set (Header.Slice (1, Ind - 1), Is_Space'Access);
      -- First word: HTTP/<vers>
      Word := As.U.Tus (Iter.Next_Word);
      if Word.Length < Http_Header'Length
      or else Word.Slice (1, Http_Header'Length) /= Http_Header
      or else Iter.Prev_Separators /= "" then
        Logger.Log_Debug ("HTTP: Invalid reply (http/): " & Iter.Image);
        return;
      end if;
      Result := (Server_Error, 400, As.U.Asu_Null);
      -- Code
      Result.Code := Server_Code_Range'Value (Iter.Next_Word);
      if Iter.Prev_Separators /= " " then
        Logger.Log_Debug ("HTTP: Invalid reply (code): " & Iter.Image);
        Result := (Client_Error, Invalid_Answer);
        return;
      end if;
      -- Message: tail
      Word := As.U.Tus (Iter.Next_Word);
      Word := As.U.Tus (Iter.Image);
      Result.Message := Word.Uslice (Iter.First_Index, Word.Length);
      Iter.Del;
    exception
      when others =>
       Result := (Client_Error, Invalid_Answer);
       return;
    end;

    -- Check status
    Logger.Log_Debug ("HTTP: Status: " & Result.Code'Img & " " & Result.Message.Image);
    if Result.Code = 200 and then Result.Message.Image = "OK" then
      -- Ok, continue
      Result := (Ok, As.U.Asu_Null);
    else
      -- Done on server error
      Logger.Log_Debug ("HTTP: Server error:" & Result.Code'Img
           & " " & Result.Message.Image);
      return;
    end if;

    -- Locate content-length header
    Ind := Str_Util.Locate (Header.Image, "Content-Length:");
    if Ind = 0 then
      Result := (Client_Error, Missing_Length);
      Logger.Log_Debug ("HTTP: Invalid reply (no length): " & Header.Image);
      return;
    end if;
    declare
      Iter : Parser.Iterator;
      Word : As.U.Asu_Us;
      Ind1 : Natural;
    begin
      Ind1 := Str_Util.Locate (Header.Image, Lf, Ind);
      Iter.Set (Header.Slice (Ind, Ind1 - 1), Is_Space'Access);
      -- Skip "Content-Length:", get value
      Word := As.U.Tus (Iter.Next_Word);
      Word := As.U.Tus (Iter.Next_Word);
      Expected_Length := Natural'Value (Word.Image);
      Logger.Log_Debug ("HTTP: Expected length: " & Expected_Length'Img);
    exception
      when others =>
        Logger.Log_Debug ("HTTP: Invalid length: " & Iter.Image);
        Result := (Client_Error, Invalid_Answer);
        return;
    end;
    if Expected_Length > Max_Msg_Len then
      Logger.Log_Debug ("HTTP: Message too long:" & Expected_Length'Img);
      Result := (Client_Error, Msg_Too_Long);
      return;
    end if;

    -- OK
    Result := (Ok, Buffer);
  end Check;

  procedure Close is
  begin
    Logger.Log_Debug ("HTTP: Closing");
    -- Cancel timer
    Timer_Id.Delete_If_Exists;
    begin
      Tcp_Util.Abort_Connect (Host, Port);
    exception
      when Tcp_Util.No_Such =>
        null;
    end;
    if Soc.Is_Open then
      begin
        My_Rece.Remove_Callbacks (Soc);
      exception
       when Tcp_Util.No_Such =>
         null;
      end;
      begin
        Tcp_Util.Abort_Send_And_Close (Soc);
      exception
       when Tcp_Util.No_Such =>
         null;
      end;
    end if;
    if Soc.Is_Open then
      Soc.Close;
    end if;
  end Close;

  -- When Soc_Read_0
  procedure Disconnection_Cb (Dscr : in Socket.Socket_Dscr) is
    pragma Unreferenced (Dscr);
  begin
    Logger.Log_Debug ("HTTP: Disconnection");
    -- Tcp_Util closes the socket
    Soc := Socket.No_Socket;
    Done := True;
  end Disconnection_Cb;

  ---------------------------------------------------------------------------
  -- For parsing URL
  function Is_Slash (C : Character) return Boolean is
  begin
    return C = '/';
  end Is_Slash;

  function Get (Url : String) return Result_Type is
    use type Ada.Calendar.Time;
  begin

    Logger.Init ("Http");

    -- Sanity check on request: Parse Url and set host and port
    declare
      Iter : Parser.Iterator;
      Addr : As.U.Asu_Us;
      Port_Start : Natural;
      use type Tcp_Util.Remote_Host_List, Tcp_Util.Remote_Port_List;
    begin
      -- Parse protocol: "http://<addr>"
      -- Where addr is <host>[:<port>]/<path>
      Iter.Set (Url, Is_Slash'Access);
      if Iter.Next_Word /= "http:" or else Iter.Prev_Separators /= "" then
        return (Client_Error, Invalid_Url);
      end if;
      Addr := As.U.Tus (Iter.Next_Word);
      if Iter.Prev_Separators /= "//" then
        return (Client_Error, Invalid_Url);
      end if;
      -- Reject URL with "username:password@"
      if Str_Util.Locate (Addr.Image, "@") /= 0 then
        return (Client_Error, Invalid_Url);
      end if;

      Port_Start := Str_Util.Locate (Addr.Image, ":");
      if Port_Start = 0 then
        -- No port, <host> and "http"
        Host := Ip_Addr.Parse (Addr.Image);
        Port := Ip_Addr.Parse ("http");
      else
        -- A port "<host>:<port>"
        Host := Ip_Addr.Parse (Addr.Slice (1, Port_Start - 1));
        Port := Ip_Addr.Parse (Addr.Slice (Port_Start + 1, Addr.Length));
      end if;
      Iter.Del;
      if Host.Kind = Tcp_Util.Host_Name_Spec then
        Addr := Host.Name;
      else
        Addr := As.U.Tus (Ip_Addr.Image (Host.Id));
      end if;
      Logger.Log_Debug ("Server address is " & Addr.Image);
      if Port.Kind = Tcp_Util.Port_Name_Spec then
        Addr := Port.Name;
      else
        Addr := As.U.Tus (Ip_Addr.Image (Port.Num));
      end if;
      Logger.Log_Debug ("Server port is " & Addr.Image);
    exception
      when others =>
        return (Client_Error, Invalid_Url);
    end;

    -- Here we go, init result
    Mut.Get;
    Done := False;
    Result := (Kind => Ok, Content => As.U.Asu_Null);

    -- Getenv Timeout and arm timeout if set
    -- Set send timeout
    The_Timeout_Ms := Environ.Get_Int (Timeout_Var, 0);
    if The_Timeout_Ms > 0 then
      -- Arm timer
      The_Delay.Expiration_Time := Ada.Calendar.Clock
                                 + Duration(The_Timeout_Ms) / 1000.0;
      Timer_Id.Create (The_Delay, Timer_Cb'Access);
    end if;

    -- Store request and connect
    declare
      Connect_Timeout_Ms : Integer;
    begin
      -- Get connection timeout
      Connect_Timeout_Ms := Environ.Get_Int (Connect_Timeout_Var,
                                             Default_Connect_Timeout_Ms);
      if The_Timeout_Ms /= 0 and then Connect_Timeout_Ms > The_Timeout_Ms then
        Connect_Timeout_Ms := The_Timeout_Ms;
      end if;
      -- Init request and result
      Request := As.U.Tus ("GET " & Url & " HTTP/1.0" & Crlf & Crlf);
      -- Connect: one synchronous try
      Logger.Log_Debug ("HTTP: Connecting during" & Connect_Timeout_Ms'Img);
      Soc := Tcp_Util.Connect_To (Socket.Tcp, Host, Port,
                                  Duration(Connect_Timeout_Ms) / 1000.0);
      if not Soc.Is_Open then
        -- Connection failure
        Close;
        Mut.Release;
        return (Client_Error, No_Server);
      end if;
    exception
      when Tcp_Util.Name_Error =>
        -- Host or "http" not found
        Close;
        Mut.Release;
        return (Client_Error, Name_Error);
    end;

    -- Send request (socket is in mode Blocking_Send): slices of Msg'Length
    Logger.Log_Debug ("HTTP: Sending " & Request.Image);
    declare
      Len : Natural;
      Dummy : Boolean;
      Msg : Message_Type;
      pragma Unreferenced (Dummy);
    begin
      loop
        if The_Timeout_Ms /= 0 then
          -- Set timeout to (positive) value of time remaining
          Send_Timeout := The_Delay.Expiration_Time - Ada.Calendar.Clock;
          if Send_Timeout < 0.0 then
            Send_Timeout := 0.001;
          end if;
        else
          -- Inifinite
          Send_Timeout := 0.0;
        end if;
        Len := Request.Length;
        exit when Len = 0 or else Done;
        if Len > Msg'Length then
          Len := Msg'Length;
        end if;
        Msg (1 .. Len) := Request.Slice (1, Len);
        Request.Delete (1, Len);
        Dummy := My_Send (Soc, null, null, Send_Timeout, Msg, Len);
        Event_Mng.Wait (0);
      end loop;
    end;

    -- Ready to receive
    Buffer.Set_Null;
    My_Rece.Set_Callbacks (Soc,
                           Read_Cb'Unrestricted_Access,
                           Disconnection_Cb'Unrestricted_Access);

    -- Loop and wait until Done
    Logger.Log_Debug ("HTTP: Waiting");
    loop
      Event_Mng.Wait (Event_Mng.Infinite_Ms);
      exit when Done;
    end loop;

    -- Close Timer and Tcp
    Close;

    -- Check data received
    if Result.Kind = Ok then
      Check;
    end if;

    -- Done
    Mut.Release;
    Logger.Log_Debug ("HTTP: Done");
    return Result;
  exception
    when others =>
      if Mut.Is_Owner then
        Mut.Release;
      end if;
      raise;
  end Get;

end Http;

