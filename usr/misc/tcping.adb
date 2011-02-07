-- Open a TCP connection to the provided host and port
with Ada.Text_Io, Ada.Calendar, Ada.Exceptions;

with As.U, Argument, Basic_Proc, Ip_Addr,
     Normal, My_Math, Round_At, Timers, Socket, Tcp_Util, Event_Mng;

procedure Tcping is


  -- Usage and error message, put on stderr
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error (
      Argument.Get_Program_Name & " tries to establish a tcp connection to a"
      & " host on a port.");
    Basic_Proc.Put_Line_Error (
      "Usage: " & Argument.Get_Program_Name &
      " <host> <port> [ -t<timeout> ] [ -d<delta> ] [ -n<tries> ] [ -c ]"
      & " [ -s ]");
    Basic_Proc.Put_Line_Error (
          "  <host>      ::= host name or ip address");
    Basic_Proc.Put_Line_Error (
          "  <port>      ::= port name or number");
    Basic_Proc.Put_Line_Error (
          "  -t<timeout> ::= connect timeout in s (default 1.0s)");
    Basic_Proc.Put_Line_Error (
          "  -d<delta>   ::= delta in s between retries (default t + 1.0s)");
    Basic_Proc.Put_Line_Error (
          "  -n<tries>   ::= number of connect tries, 0 = infinite (default 1)");
    Basic_Proc.Put_Line_Error (
          "  -c          ::= go on trying even after a success (default false)");
    Basic_Proc.Put_Line_Error (
          "  -s          ::= silent, result in exit code only (default verbose)");
    Basic_Proc.Put_Line_Error (
          "Exits with 0 if at least one connect succeeds, 1 otherwise.");
  end Usage;

  Arg_Error : exception;

  procedure Put_Arg_Error (Msg : in String := "") is
  begin
    Basic_Proc.Put_Error ("Error, invalid argument(s)");
    if Msg /= "" then
      Basic_Proc.Put_Line_Error (": " & Msg);
    else
      Basic_Proc.Put_Line_Error (".");
    end if;
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    raise Arg_Error;
  end Put_Arg_Error;

  -- Argument parsed
  Txt : As.U.Asu_Us;
  Host : Tcp_Util.Remote_Host;
  Port : Tcp_Util.Remote_Port;

  Timeout     : Timers.Period_Range := 1.0;
  Delta_Tries : Timers.Period_Range := 0.0;
  Nb_Tries    : Natural := 1;
  Silent      : Boolean := False;
  Go_Success  : Boolean := False;

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

  -- "xxx.yyy.zzz.ttt" from Host_Id
  function Image (Id : Socket.Host_Id) return String is
    Addr : constant Socket.Ip_Address := Socket.Id2Addr (Id);
  begin
    return Ip_Addr.Image (Addr);
  end Image;

  -- Cancel immediate timer Cb
  function Cancel_Cb (Id : in Timers.Timer_Id;
                    Data : in Timers.Timer_Data := Timers.No_Data)
           return Boolean is
    pragma Unreferenced (Id, Data);
  begin
    Game_Over := True;
    return True;
  end Cancel_Cb;

  -- Immediate timer to stop
  procedure Cancel is
    Cid : Timers.Timer_Id;
    pragma Unreferenced (Cid);
  begin
    -- Need a timer so that the main loop Wait returns
    Cid := Timers.Create ( (Delay_Kind    => Timers.Delay_Sec,
                            Clock         => null,
                            Period        => Timers.No_Period,
                            Delay_Seconds => 0.0),
                           Cancel_Cb'Unrestricted_Access);
  end Cancel;

  procedure Connect_Cb (Remote_Host_Id  : in Tcp_Util.Host_Id;
                        Remote_Port_Num : in Tcp_Util.Port_Num;
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
      R := Round_At (R, -3);
      Int :=  My_Math.Trunc (R);
      Frac := Integer(My_Math.Trunc (My_Math.Frac (R) * 1000.0));

      Loc_Dscr.Close;
      Put_Line (
         "Connected to " & Image (Remote_Host_Id) &
         " port" & Socket.Port_Num'Image(Remote_Port_Num) &
         " in" & Int'Img & "." & Normal (Frac, 3, Gap => '0') & "s.");
      Success := True;
    else
       Put_Line ("Failed.");
    end if;
    Connecting := False;
    -- Stop if connect and not explicitly requested to go on on success
    -- Or stop when nb tries set and reached
    if (Connected and then not Go_Success)
    or else (Nb_Tries /= 0 and then Curr_Try = Nb_Tries) then
      -- Finished
      Cancel;
    end if;
  end Connect_Cb;

  function Timer_Cb (Id : in Timers.Timer_Id;
                     Data : in Timers.Timer_Data := Timers.No_Data)
           return Boolean is
    pragma Unreferenced (Id, Data);
    Dummy :  Boolean;
    pragma Unreferenced (Dummy);
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
        Connecting := False;
        if Nb_Tries /= 0 and then Curr_Try = Nb_Tries then
          Cancel;
        end if;
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
    Put_Arg_Error (Txt.Image);
    Argument.Get_Parameter (Txt, 2, "t");
    Put_Arg_Error (Txt.Image);
    Argument.Get_Parameter (Txt, 2, "d");
    Put_Arg_Error (Txt.Image);
    Argument.Get_Parameter (Txt, 2, "n");
    Put_Arg_Error (Txt.Image);
    Argument.Get_Parameter (Txt, 2, "s");
    Put_Arg_Error (Txt.Image);
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
  begin
    Delta_Tries := Timers.Period_Range'Value (
       Argument.Get_Parameter (Param_Key => "d"));
    if Delta_Tries <= 0.0 then
      raise Constraint_Error;
    end if;
  exception
    when Argument.Argument_Not_Found =>
      Delta_Tries := Timeout + 1.0;
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
    Go_Success := Argument.Get_Parameter (Param_Key => "c") = "";
    if not Go_Success then
      -- Something was set after "-c"
      Put_Arg_Error (Argument.Get_Parameter (Param_Key => "c"));
    end if;
  exception
    when Argument.Argument_Not_Found =>
      Go_Success := False;
    when others =>
      Put_Arg_Error (Argument.Get_Parameter (Param_Key => "c"));
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
  Tid := Timers.Create ( (Delay_Kind    => Timers.Delay_Sec,
                          Clock         => null,
                          Period        => Delta_Tries,
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
    Basic_Proc.Set_Error_Exit_Code;
  end if;

exception
  when Socket.Soc_Name_Not_Found =>
    Basic_Proc.Put_Line_Error ("Error, unknown host or port");
    Basic_Proc.Set_Error_Exit_Code;
  when Arg_Error =>
    Basic_Proc.Set_Exit_Code(2);
  when Argument.Argument_Not_Found =>
    Basic_Proc.Put_Error ("Error, invalid argument(s)");
    Basic_Proc.Set_Exit_Code(2);
end Tcping;

