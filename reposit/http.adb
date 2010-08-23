with Ada.Characters.Latin_1;
with Basic_Proc, Environ, String_Mng, Parser,
     Event_Mng, Timers, Ip_Addr, Socket, Tcp_Util;
package body Http is

  -- Debug
  Debug_Var : constant String := "HTTP_DEBUG";
  type Debug_Status_List is (Unknown, Set, Notset);
  Debug_Status : Debug_Status_List := Unknown;
  procedure Debug (Msg : in String) is
  begin
    if Debug_Status = Unknown then
      if Environ.Is_Yes (Debug_Var) then
        Debug_Status := Set;
      else
        Debug_Status := Notset;
      end if;
    end if;
    if Debug_Status = Set then
      Basic_Proc.Put_Line_Error (Msg);
    end if;
  end Debug;

  -- Timeout definition
  Timeout_Var : constant String := "HTTP_TIMEOUT_MS";

  -- Each connection try timeout definition
  Default_Connect_Timeout : constant Integer := 3000;
  Connect_Timeout_Var : constant String := "HTTP_CONNECT_TIMEOUT_MS";

  -- End of processing (Error, disconnection or timeout)
  Done : Boolean;

  -- Result to return
  Result : Result_Type;

  -- The request
  Host : Tcp_Util.Remote_Host;
  Port : Tcp_Util.Remote_Port;
  Request : Asu_Us;

  -- The socket
  Soc : Socket.Socket_Dscr;

  -- Send/receive Msg
  -- Shall be large enough to receive the whole reply header
  subtype Message_Type is String (1 .. 1024 * 1024);
  function My_Send is new Tcp_Util.Send (Message_Type);
  package My_Rece is new Tcp_Util.Reception (Message_Type);

  -- Timer Cb
  Timer_Id : Timers.Timer_Id;
  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data := Timers.No_Data)
           return Boolean is
    pragma Unreferenced (Id, Data);
  begin
    Debug ("HTTP: Timeout");
    -- Cancel connect if connecting
    begin
      Tcp_Util.Abort_Connect (Host, Port);
    exception
      when Tcp_Util.No_Such =>
        null;
    end;
    -- Cancel reception if receiving
    if Soc.Is_Open then
      begin
        My_Rece.Remove_Callbacks (Soc);
      exception
        when Tcp_Util.No_Such =>
          null;
      end;
    end if;
    -- Close if open
    if Soc.Is_Open then
      Soc.Close;
    end if;
    Result := (Client_Error, Timeout);
    Done := True;
    return True;
  end Timer_Cb;

  -- For parsing status line of reply
  function Is_Space (C : Character) return Boolean is
  begin
    return C = ' ';
  end Is_Space;

  -- Message reception Cb
  Line_Feed : constant String := Ada.Characters.Latin_1.Lf & "";
  In_Content : Boolean;
  Expected_Length : Integer;
  procedure Read_Cb (Dscr : in Socket.Socket_Dscr;
                     Msg : in Message_Type;
                     Len : in Natural) is
    pragma Unreferenced (Dscr);
    Ind : Natural;
    Text : Asu_Us;
  begin
    if Result.Kind /= Ok then
      -- Already on error => Drop
      return;
    end if;
    -- CrLf -> Lf
    Text := Asu_Tus (String_Mng.Replace (Msg(1 .. Len),
                            Ada.Characters.Latin_1.Cr & Line_Feed,
                            Line_Feed));
    if In_Content then
      -- Not first Msg, append content to result
      Asu.Append (Result.Content, Text);
      Debug ("HTTP: Appended content len:" & Integer'Image (Asu.Length (Text)));
      return;
    end if;

    -- Parse header, init default result
    Result := (Client_Error, Invalid_Answer);
    Done := True;

    -- First line of header reply: status
    Ind := String_Mng.Locate (Asu_Ts (Text), Line_Feed);
    if Ind = 0 then
      return;
    end if;
    declare
      Iter : Parser.Iterator;
      Word : Asu_Us;
      Http_Header : constant String := "HTTP/";
    begin
      Iter.Set (Msg (1 .. Ind - 1), Is_Space'Access);
      -- First word: HTTP/<vers>
      Word := Asu_Tus (Iter.Next_Word);
      if Asu.Length (Word) < Http_Header'Length
      or else Asu.Slice (Word, 1, Http_Header'Length) /= Http_Header
      or else Iter.Prev_Separators /= "" then
        Debug ("HTTP: Invalid reply (http/): " & Iter.Image);
        return;
      end if;
      Result := (Server_Error, 400, Asu_Null);
      -- Code
      Result.Code := Server_Code_Range'Value (Iter.Next_Word);
      if Iter.Prev_Separators /= " " then
        Debug ("HTTP: Invalid reply (code): " & Iter.Image);
        Result := (Client_Error, Invalid_Answer);
        return;
      end if;
      -- Message: tail
      Word := Asu_Tus (Iter.Next_Word);
      Word := Asu_Tus (Iter.Image);
      Result.Message := Asu.Unbounded_Slice (Word,
                  Iter.First_Index, Asu.Length (Word));
      Iter.Del;
    exception
      when others =>
       Result := (Client_Error, Invalid_Answer);
       return;
    end;

    -- Check status
    Debug ("HTTP: Status: " & Result.Code'Img & " " & Asu_Ts (Result.Message));
    if Result.Code = 200 and then Asu_Ts (Result.Message) = "OK" then
      -- Ok, continue
      Result := (Ok, Asu_Null);
    else
      -- Done on server error
      Debug ("HTTP: Server error:" & Result.Code'Img
           & " " & Asu_Ts (Result.Message));
      return;
    end if;

    -- Locate content length header
    Ind := String_Mng.Locate (Asu_Ts (Text), "Content-Length:");
    if Ind = 0 then
      Result := (Client_Error, Missing_Length);
      Debug ("HTTP: Invalid reply (no length): " & Asu_Ts (Text));
      return;
    end if;
    declare
      Iter : Parser.Iterator;
      Word : Asu_Us;
      Ind1 : Natural;
    begin
      Ind1 := String_Mng.Locate (Asu_Ts (Text), Line_Feed, Ind);
      Iter.Set (Asu.Slice (Text, Ind, Ind1 - 1), Is_Space'Access);
      -- Skip "Content-Length:", get value
      Word := Asu_Tus (Iter.Next_Word);
      Word := Asu_Tus (Iter.Next_Word);
      Expected_Length := Natural'Value (Asu_Ts (Word));
      Debug ("HTTP: Expected length: " & Expected_Length'Img);
    exception
      when others =>
        Debug ("HTTP: Invalid length: " & Iter.Image);
        Result := (Client_Error, Invalid_Answer);
        return;
    end;

    -- Locate end of header: Empty line
    Ind := String_Mng.Locate (Asu_Ts (Text), Line_Feed & Line_Feed);
    if Ind = 0 then
      Debug ("HTTP: Missing break");
      Result := (Client_Error, Invalid_Answer);
      return;
    end if;
    In_Content := True;
    Result := (Ok, Asu_Null);
    if Ind /= Asu.Length (Text) - 1 then
      -- There is content
      Asu.Unbounded_Slice (Text, Result.Content, Ind + 2, Asu.Length (Text));
      Done := False;
      Debug ("HTTP: Got content len:"
           & Integer'Image (Asu.Length (Result.Content)));
    end if;
  end Read_Cb;

  -- When Soc_Read_0
  procedure Disconnection_Cb (Dscr : in Socket.Socket_Dscr) is
    pragma Unreferenced (Dscr);
    use type Timers.Timer_Id;
  begin
    Debug ("HTTP: Disconnection");
    -- Tcp_Util closes the socket
    Soc := Socket.No_Socket;
    -- Cancel timer
    if Timer_Id /= Timers.No_Timer then
      Timers.Delete (Timer_Id);
      Timer_Id := Timers.No_Timer;
    end if;
    -- Check length and set result
    if Result.Kind = Ok then
      if not In_Content then
        Debug ("HTTP: Disconnection with no content");
        Result := (Client_Error, Invalid_Answer);
      elsif Asu.Length (Result.Content) /= Expected_Length then
        Debug ("HTTP: Disconnection with length:"
             & Natural'Image (Asu.Length (Result.Content)));
        Result := (Client_Error, Wrong_Length);
      end if;
    end if;
    Done := True;
  end Disconnection_Cb;

  -- Connection_Cb
  procedure Connection_Cb (Remote_Port_Num : in Tcp_Util.Port_Num;
                           Remote_Host_Id  : in Tcp_Util.Host_Id;
                           Connected       : in Boolean;
                           Dscr            : in Socket.Socket_Dscr) is
    pragma Unreferenced (Remote_Port_Num, Remote_Host_Id, Connected);
    Msg : Message_Type;
    Len : Natural;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    Debug ("HTTP: Connection");
    -- Save Dscr & Send request
    Soc := Dscr;
    -- Send request: slices of Msg'Length
    Debug ("HTTP: Sending " & Asu_Ts (Request));
    loop
      Len := Asu.Length (Request);
      exit when Len = 0;
      if Len > Msg'Length then
        Len := Msg'Length;
      end if;
      Msg (1 .. Len) := Asu.Slice (Request, 1, Len);
      Asu.Delete (Request, 1, Len);
      Dummy := My_Send (Soc, null, Msg, Len);
    end loop;
    -- Set not blocking and hook receptions
    Soc.Set_Blocking (False);
    In_Content := False;
    My_Rece.Set_Callbacks (Soc,
                           Read_Cb'Unrestricted_Access,
                           Disconnection_Cb'Unrestricted_Access);

  end Connection_Cb;

  ---------------------------------------------------------------------------
  -- For parsing URL
  function Is_Slash (C : Character) return Boolean is
  begin
    return C = '/';
  end Is_Slash;

  function Get (Url : String) return Result_Type is
  begin
    -- Init result
    Done := False;
    Timer_Id := Timers.No_Timer;
    Result := (Kind => Ok, Content => Asu_Null);

    -- Parse Url and set host and port
    declare
      Iter : Parser.Iterator;
    begin
      Iter.Set (Url, Is_Slash'Access);
      if Iter.Next_Word /= "http:" or else Iter.Prev_Separators /= "" then
        return (Client_Error, Invalid_Url);
      end if;
      Host := Ip_Addr.Parse (Iter.Next_Word);
      if Iter.Prev_Separators /= "//" then
        return (Client_Error, Invalid_Url);
      end if;
      Port := Ip_Addr.Parse ("http");
      Iter.Del;
    exception
      when others =>
        return (Client_Error, Invalid_Url);
    end;

    -- Getenv Timeout and arm timeout if set
    declare
      Timeout : Integer;
      The_Delay : Timers.Delay_Rec;
    begin
      Timeout := Environ.Get_Int (Timeout_Var, 0);
      if Timeout > 0 then
        -- Arm timer
        The_Delay.Delay_Seconds := Duration(Timeout) / 1000.0;
        Timer_Id := Timers.Create (The_Delay, Timer_Cb'Access);
      end if;
    end;

    -- Store request and connect
    declare
      Connect_Timeout : Integer;
      Lf : constant Character := Ada.Characters.Latin_1.Lf;
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
    begin
      -- Get connection timeout
      Connect_Timeout := Environ.Get_Int (Connect_Timeout_Var,
                                          Default_Connect_Timeout);
      -- Init request and result
      Request := Asu_Tus ("GET " & Url & " HTTP/1.0" & Lf & Lf);
      -- Connect: infinite retries each Connect_Timeout sec
      Soc := Socket.No_Socket;
      Debug ("HTTP: Connecting each" & Connect_Timeout'Img);
      Dummy := Tcp_Util.Connect_To (Socket.Tcp, Host, Port,
                                    Duration(Connect_Timeout) / 1000.0, 0,
                                    Connection_Cb'Access);
    end;

    -- Loop and wait until Done
    Debug ("HTTP: Waiting");
    loop
      Event_Mng.Wait (Event_Mng.Infinite_Ms);
      exit when Done;
    end loop;

    -- Done
    Debug ("HTTP: Done");
    return Result;
  end Get;

end Http;

