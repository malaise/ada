with Ada.Text_Io, Ada.Calendar, Ada.Exceptions;

with Text_Handler, Argument, Sys_Calls, Ip_Addr,
     Normal, My_Math, Timers, Socket, Tcp_Util, Event_Mng;

procedure Tcping is


  -- Usage and error message, put on stderr
  procedure Usage is
  begin
    Sys_Calls.Put_Line_Error (
      "Usage: " & Argument.Get_Program_Name &
      " <host> <port> [ -t<timeout> ] [ -d<delta> ] [ -n<tries> ] [ -s ]");
    Sys_Calls.Put_Line_Error (
          "  <host>    ::= host name or ip address");
    Sys_Calls.Put_Line_Error (
          "  <port>    ::= port name or num");
    Sys_Calls.Put_Line_Error (
          "  -t<timeout> ::= connect timeout in s (1.0s)");
    Sys_Calls.Put_Line_Error (
          "  -d<delta>   ::= delta in s between tries (t + 1.0s)");
    Sys_Calls.Put_Line_Error (
          "  -n<tries>   ::= number of try attempts, 0 = infinite (1)");
    Sys_Calls.Put_Line_Error (
          "  -s          ::= silent, result in exit code only");
  end Usage;

  Arg_Error : exception;

  procedure Put_Arg_Error (Msg : in String := "") is
  begin
    Sys_Calls.Put_Error ("Error, invalid argument");
    if Msg /= "" then
      Sys_Calls.Put_Line_Error (": " & Msg);
    else
      Sys_Calls.Put_Line_Error (".");
    end if;
    Usage;
    Sys_Calls.Set_Error_Exit_Code;
    raise Arg_Error;
  end Put_Arg_Error;

  -- Argument parsed
  Txt : Text_Handler.Text (Tcp_Util.Max_Host_Name_Len);
  Host : Tcp_Util.Remote_Host;
  Port : Tcp_Util.Remote_Port;

  Timeout     : Timers.Period_Range := 1.0;
  Delta_Tries : Timers.Period_Range := 0.0;
  Nb_Tries    : Natural := 1;
  Silent      : Boolean := False;

  -- Timer for retries, count
  Tid         : Timers.Timer_Id := Timers.No_Timer;
  Curr_Try    : Natural := 0;

  -- Connect in progess
  Connecting : Boolean := False;

  -- Time when connection started
  Start_Time : Ada.Calendar.Time;

  -- Event received
  Event : Event_Mng.Out_Event_List;

  -- End of game and result
  Game_Over : Boolean := False;
  Success   : Boolean := False;

  -- Various dignostics, put on stdout if not silent
  procedure Put (Str : in String) is
  begin
    if Silent then
      return;
    end if;
    Ada.Text_Io.Put (Str);
  end Put;

  procedure Put_Line (Str : in String) is
  begin
    if Silent then
      return;
    end if;
    Ada.Text_Io.Put_Line (Str);
  end Put_Line;
  -- 1 to 3 digits of a byte
  function Image (B : Socket.Byte) return String is
    Str : constant String := Socket.Byte'Image(B);
  begin
    return Str (2 .. Str'Last);
  end Image;

  -- "xxx.yyy.zzz.ttt" from Host_Id
  function Image (Id : Socket.Host_Id) return String is
    Addr : constant Socket.Ip_Address := Socket.Id2Addr (Id);
  begin
    return Image (Addr.A) & "." & Image (Addr.B) & "." &
           Image (Addr.C) & "." & Image (Addr.D);
  end Image;

  -- Immediate timer to cancel retries
  function Cancel_Cb (Id : in Timers.Timer_Id;
                    Data : in Timers.Timer_Data := Timers.No_Data)
           return Boolean is
  begin
    Game_Over := True;
    return True;
  end Cancel_Cb;

  procedure Cancel is
    Cid : Timers.Timer_Id;
  begin
    Cid := Timers.Create ( (Delay_Kind => Timers.Delay_Sec,
                            Period => Timers.No_Period,
                            Delay_Seconds => 0.0),
                           Cancel_Cb'Unrestricted_Access);
  end Cancel;
 
  procedure Connect_Cb (Remote_Port_Num : in Tcp_Util.Port_Num;
                        Remote_Host_Id  : in Tcp_Util.Host_Id;
                        Connected       : in Boolean;
                        Dscr            : in Socket.Socket_Dscr) is
    Loc_Dscr : Socket.Socket_Dscr := Dscr;
    Dur : Duration;
    R : My_Math.Real;
    Int : My_Math.Inte;
    Frac : Integer;
    use type Ada.Calendar.Time, My_Math.Real;
  begin
    if Connected then
      -- Done, clean
      Dur := Ada.Calendar.Clock - Start_Time;
      R := My_Math.Real(Dur);
      R := My_Math.Real (My_Math.Round (R * 1000.0)) / 1000.0;
      Int :=  My_Math.Trunc (R);
      Frac := Integer(My_Math.Trunc (My_Math.Frac (R) * 1000.0));

      Socket.Close (Loc_Dscr);
      Put_Line (
         "Connected to " & Image (Remote_Host_Id) &
         " port" & Socket.Port_Num'Image(Remote_Port_Num) &
         " in" & Int'Img & "." & Normal (Frac, 3, Gap => '0') & "s.");
      Success := True;
    else
       Put_Line ("Failed.");
    end if;
    Connecting := False;
    if Nb_Tries /= 0 and then Curr_Try = Nb_Tries then
      -- Finished
      Cancel;
    end if;
  end Connect_Cb;

  function Timer_Cb (Id : in Timers.Timer_Id;
                     Data : in Timers.Timer_Data := Timers.No_Data)
           return Boolean is
    Dummy :  Boolean;
  begin
    -- Cancel pending connection
    -- This occures if blocked looking for host/port in dns/yp...
    if Connecting then
      Put_Line ("Blocked!!!.");
      Tcp_Util.Abort_Connect (Host, Port);
      Connecting := False;
    end if;

    -- Check max tries
    if Nb_Tries /= 0 then
      Curr_Try := Curr_Try + 1;
      if Curr_Try > Nb_Tries then
        -- This should not occure cause timeout < delta
        Put_Line ("Overflow!!!.");
        Cancel;
      end if;
    end if;

    -- Retry
    Put ("Connecting... ");
    Connecting := True;
    begin
      Dummy := Tcp_Util.Connect_To (Socket.Tcp, Host, Port, Timeout, 1,
               Connect_Cb'Unrestricted_Access);
    exception
      when Error:others =>
        Put_Line ("Connect exception "
            & Ada.Exceptions.Exception_Name (Error));
    end;
    Start_Time := Ada.Calendar.Clock;
    return False;
  end Timer_Cb;
 
begin
  -- 2 and only 2 no_key, no dup options
  if Argument.Get_Nbre_Arg < 2 then
    Put_Arg_Error;
  end if;
  begin
    Argument.Get_Parameter (Txt, 3, Argument.Not_Key);
    Put_Arg_Error (Text_Handler.Value (Txt));
    Argument.Get_Parameter (Txt, 2, "t");
    Put_Arg_Error (Text_Handler.Value (Txt));
    Argument.Get_Parameter (Txt, 2, "d");
    Put_Arg_Error (Text_Handler.Value (Txt));
    Argument.Get_Parameter (Txt, 2, "n");
    Put_Arg_Error (Text_Handler.Value (Txt));
    Argument.Get_Parameter (Txt, 2, "s");
    Put_Arg_Error (Text_Handler.Value (Txt));
  exception
    when Argument.Argument_Not_Found =>
      null;
  end;

  -- Host and port
  begin
    Host := Ip_Addr.Parse (Argument.Get_Parameter (1, Argument.Not_Key));
  exception
    when others =>
      Put_Arg_Error ("host" & Argument.Get_Parameter (1, Argument.Not_Key));
  end;
  begin
    Port := Ip_Addr.Parse (Argument.Get_Parameter (2, Argument.Not_Key));
  exception
    when others =>
      Put_Arg_Error ("port" & Argument.Get_Parameter (2, Argument.Not_Key));
  end;

  -- Options
  begin
    Timeout := Timers.Period_Range'Value (
       Argument.Get_Parameter (Param_Key => "t"));
  exception
    when Argument.Argument_Not_Found =>
      null;
    when others =>
      Put_Arg_Error (Argument.Get_Parameter (Param_Key => "t"));
  end;
  Delta_Tries := Timeout + 1.0;
  begin
    Delta_Tries := Timers.Period_Range'Value (
       Argument.Get_Parameter (Param_Key => "d"));
    if Delta_Tries <= 0.0 then
      raise Constraint_Error;
    end if;
  exception
    when Argument.Argument_Not_Found =>
      null;
    when others =>
      Put_Arg_Error (Argument.Get_Parameter (Param_Key => "d"));
  end;
  begin
    Nb_Tries := Natural'Value (
       Argument.Get_Parameter (Param_Key => "n"));
  exception
    when Argument.Argument_Not_Found =>
      null;
    when others =>
      Put_Arg_Error (Argument.Get_Parameter (Param_Key => "n"));
  end;
  begin
    Silent := Argument.Get_Parameter (Param_Key => "s") = "";
    if not Silent then
      -- Something was set after "-s"
      Put_Arg_Error (Argument.Get_Parameter (Param_Key => "s"));
    end if;
  exception
    when Argument.Argument_Not_Found =>
      Silent := False;
    when others =>
      Put_Arg_Error (Argument.Get_Parameter (Param_Key => "s"));
  end;

  -- Check timeout < delta
  if Timeout >= Delta_Tries then
    Put_Arg_Error ("timeout must be less than delta");
  end if;

  -- Arm periodical timer
  Tid := Timers.Create ( (Delay_Kind => Timers.Delay_Sec,
                          Period => Delta_Tries,
                          Delay_Seconds => 0.0),
                         Timer_Cb'Unrestricted_Access);

  -- Main loop
  loop
    Event := Event_Mng.Wait (-1);
    case Event is
      when Event_Mng.Timer_Event =>
        if Game_Over then
          Put_Line ("Done.");
          exit;
        end if;
      when Event_Mng.Signal_Event =>
        Put_Line ("Aborted.");
        exit;
      when others =>
        null;
    end case;
  end loop;

  -- Cancel timer
  Timers.Delete (Tid);

  -- Cancel pending connection
  if Connecting then
    Tcp_Util.Abort_Connect (Host, Port);
    Connecting := False;
  end if;

  if not Success then
    Sys_Calls.Set_Error_Exit_Code;
  end if;

exception
  when Socket.Soc_Name_Not_Found =>
    Sys_Calls.Put_Line_Error ("Unknown host or port");
    Sys_Calls.Set_Error_Exit_Code;
  when Arg_Error =>
    null;
end Tcping;

