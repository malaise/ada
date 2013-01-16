with Ada.Exceptions, Ada.Characters.Latin_1;
with Argument, Basic_Proc, Async_Stdin, Event_Mng, Socket, Socket_Util,
     Tcp_Util, Ip_Addr, Sys_Calls, Autobus, As.U;
procedure T_Async is

  procedure Usage is
  begin
    Async_Stdin.Put_Line_Err ("Usage: "
       & Argument.Get_Program_Name & " [ <no_stdin> ] <mode>");
    Async_Stdin.Put_Line_Err ("  <no_stdin>   ::= -n");
    Async_Stdin.Put_Line_Err ("  <mode>       ::= <autobus> | <tcp_client> | <tcp_server> | <udp>");
    Async_Stdin.Put_Line_Err ("  <autobus>    ::= -a <lan>:<port>");
    Async_Stdin.Put_Line_Err ("  <tcp_client> ::= -c <host>:<port>");
    Async_Stdin.Put_Line_Err ("  <tcp_server> ::= -s <port>");
    Async_Stdin.Put_Line_Err ("  <udp>        ::= -u [<lan>]:<port> | -U [<lan>]:<port>");
    Async_Stdin.Put_Line_Err ("  <lan>        ::= <lan_name>  | <lan_address>");
    Async_Stdin.Put_Line_Err ("  <host>       ::= <host_name> | <host_address>");
    Async_Stdin.Put_Line_Err ("  <port>       ::= <port_name> | <port_num>");
    Async_Stdin.Put_Line_Err ("-U => send on port+1, -u => send on port-1");
  end Usage;
  Arg_Error : exception;
  Use_Stdin : Boolean;
  Mode_Index : Positive;

  -- Client or server or udp mode
  type Mode_List is (Abus, Client, Server, Udp);
  Mode : Mode_List;

  -- Give_up on connection failure, signal, Async_Stdin error
  Give_Up : Boolean := False;

  -- Common message type
  subtype Message_Type is String (1 .. Autobus.Message_Max_Length);

  -- Set asynchronous stdin
  procedure Set_Async;

  -- Autobus / TCP / UDP stuff
  Bus : aliased Autobus.Bus_Type;
  Bus_Addr : As.U.Asu_Us;
  Subs : aliased Autobus.Subscriber_Type;
  Local_Port_Def : Tcp_Util.Local_Port;
  Remote_Host_Def : Tcp_Util.Remote_Host;
  Remote_Port_Def : Tcp_Util.Remote_Port;
  Send_Port_Def : Tcp_Util.Remote_Port;
  Port_Num : Tcp_Util.Port_Num;
  Soc : Socket.Socket_Dscr;
  Send_Soc : Socket.Socket_Dscr;
  In_Overflow : Boolean;
  procedure Open;
  procedure Close;

  -- Autobus receiver
  type Observer_Type is new Autobus.Observer_Type with null record;
  Receiver : aliased Observer_Type;
  procedure Receive (Observer : in out Observer_Type;
                     Subscriber : in Autobus.Subscriber_Access_Type;
                     Message : in String) is
    pragma Unreferenced (Observer, Subscriber);
  begin
    if Message'Length = 1
    and then Message(Message'First) = Ada.Characters.Latin_1.Eot then
      -- Single Ctrl D => Close connection
      Async_Stdin.Put_Line_Err ("closing");
      Give_Up := True;
    else
      Async_Stdin.Put_Out (Message);
    end if;
  end Receive;

  -- End of sending overflow
  procedure End_Ovf_Cb (Dscr : in  Socket.Socket_Dscr) is
    pragma Unreferenced (Dscr);
    use type Socket.Socket_Dscr;
  begin
    In_Overflow := False;
  end End_Ovf_Cb;

  -- Sending fatal error
  procedure Send_Err_Cb (Dscr : in  Socket.Socket_Dscr;
                         Conn_Lost : in Boolean) is
    pragma Unreferenced (Dscr);
  begin
    if Conn_Lost then
      Async_Stdin.Put_Line_Err ("Sending msg failed cause connection lost");
    else
      Async_Stdin.Put_Line_Err ("Sending msg failed cause timeout");
    end if;
  end Send_Err_Cb;


  -- For sending on Tcp
  function Tcp_Send is new Tcp_Util.Send (Message_Type);
  procedure Udp_Send is new Socket.Send (Message_Type);
  procedure Send (Msg : in Message_Type; Len : in Positive) is
  begin
    if Mode = Abus then
      Bus.Send (Msg(Msg'First .. Msg'First + Len - 1));
    elsif not Soc.Is_Open then
      return;
    elsif Mode = Udp then
       Udp_Send (Send_Soc, Msg, Len);
    elsif not In_Overflow then
      if not Tcp_Send (Soc, End_Ovf_Cb'Unrestricted_Access,
                       Send_Err_Cb'Unrestricted_Access, 1.0, Msg, Len) then
        In_Overflow := True;
      end if;
    end if;
  exception
    when Error : others =>
      Async_Stdin.Put_Line_Err ("Sending " & Msg(1 .. Len) & " failed cause "
        & Ada.Exceptions.Exception_Name (Error) & ".");
  end Send;

  -- When a message to read
  function Read_Cb (Dscr : Socket.Socket_Dscr;
                    Msg  :  Message_Type;
                    Len  :  Natural) return Boolean is
    pragma Unreferenced (Dscr);
  begin
    if Len = 1 and then Msg(1) = Ada.Characters.Latin_1.Eot then
      -- Single Ctrl D => Close connection
      Async_Stdin.Put_Line_Err ("closing");
      Close;
      Give_Up := True;
    else
      Async_Stdin.Put_Out (Msg(1 .. Len));
    end if;
    return True;
  end Read_Cb;
  package My_Rece is new Tcp_Util.Reception (Message_Type);

  -- When Soc_Read_0
  procedure Discon_Cb (Dscr : in Socket.Socket_Dscr) is
    pragma Unreferenced (Dscr);
  begin
    Async_Stdin.Put_Line_Err ("disconnected");
    -- Tcp_Util closes the socket
    Soc := Socket.No_Socket;
    if Mode = Server then
      -- Accept connections again
      Open;
    else
      Give_Up := True;
    end if;
  end Discon_Cb;

  procedure Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                       Local_Dscr      : in Socket.Socket_Dscr;
                       Remote_Host_Id  : in Tcp_Util.Host_Id;
                       Remote_Port_Num : in Tcp_Util.Port_Num;
                       New_Dscr        : in Socket.Socket_Dscr) is
    pragma Unreferenced (Local_Port_Num, Local_Dscr, Remote_Port_Num,
                         Remote_Host_Id);
    use type Socket.Socket_Dscr;
  begin
    Soc := New_Dscr;
    Soc.Set_Blocking (Socket.Non_Blocking);
    In_Overflow := False;
    My_Rece.Set_Callbacks (Soc, Read_Cb'Unrestricted_Access,
                                  Discon_Cb'Unrestricted_Access);
    Async_Stdin.Put_Line_Err ("connected");
    -- Only one connection at a time
    Tcp_Util.Abort_Accept (Socket.Tcp, Port_Num);
    -- Ready to process stdin
    Set_Async;
  end Accept_Cb;

  -- Connection report Cb
  procedure Conn_Cb (Remote_Host_Id  : in Tcp_Util.Host_Id;
                     Remote_Port_Num : in Tcp_Util.Port_Num;
                     Connected       : in Boolean;
                     Dscr            : in Socket.Socket_Dscr) is
    pragma Unreferenced (Remote_Port_Num, Remote_Host_Id);
  begin
    if Connected then
      Soc := Dscr;
      Soc.Set_Blocking (Socket.Non_Blocking);
      In_Overflow := False;
      My_Rece.Set_Callbacks (Soc, Read_Cb'Unrestricted_Access,
                                  Discon_Cb'Unrestricted_Access);
      Async_Stdin.Put_Line_Err ("connected");
      -- Ready to process stdin
      Set_Async;
    else
      -- Connection failure => Abort
      Give_Up := True;
    end if;
  end Conn_Cb;

  -- Open (accept) Tcp connection
  procedure Open is
    Acc_Soc : Socket.Socket_Dscr;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    case Mode is
      when Abus =>
        Bus.Init (Bus_Addr.Image);
        Subs.Init (Bus'Unrestricted_Access, Receiver'Unrestricted_Access);
        Set_Async;
      when Server =>
        loop
          begin
            Tcp_Util.Accept_From (Socket.Tcp,
                                  Local_Port_Def,
                                  Accept_Cb'Unrestricted_Access,
                                  Acc_Soc,
                                  Port_Num);
            exit;
          exception
            when Socket.Soc_Addr_In_Use =>
              Async_Stdin.Put_Line_Err ("Cannot accept. Maybe Close-wait. Waiting");
              Event_Mng.Wait (20_000);
              exit when Give_Up;
          end;
        end loop;
      when Client =>
        Dummy := Tcp_Util.Connect_To (Socket.Tcp,
                                      Remote_Host_Def,
                                      Remote_Port_Def,
                                      Conn_Cb'Unrestricted_Access,
                                      1.0,
                                      1);
      when Udp =>
        Soc.Open (Socket.Udp);
        Soc.Set_Blocking (Socket.Non_Blocking);
        Socket_Util.Set_Destination (Soc, True, Remote_Host_Def,
                                     Remote_Port_Def);
        Socket_Util.Link (Soc, Remote_Port_Def);
        Send_Soc.Open (Socket.Udp);
        Send_Soc.Set_Blocking (Socket.Non_Blocking);
        Socket_Util.Set_Destination (Send_Soc, True, Remote_Host_Def,
                                     Send_Port_Def);

        My_Rece.Set_Callbacks (Soc, Read_Cb'Unrestricted_Access, null);
        Set_Async;
    end case;
  exception
    when Autobus.Invalid_Address | Autobus.Name_Error =>
      raise Arg_Error;
  end Open;

  procedure Close is
  begin
    -- Reset Bus
    if Mode = Abus then
      Subs.Reset;
      Bus.Reset;
      return;
    end if;
    -- Cancel TCP connect or accept
    if Mode = Server then
      begin
        Tcp_Util.Abort_Accept (Socket.Tcp, Port_Num);
      exception
        when Tcp_Util.No_Such =>
          null;
      end;
    elsif Mode = Client then
      begin
        Tcp_Util.Abort_Connect (Remote_Host_Def, Remote_Port_Def);
      exception
        when Tcp_Util.No_Such =>
          null;
      end;
    end if;
    -- Cancel TCP or UDP reception and TCP overflow
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
    -- Close TCP or UDP socket
    if Soc.Is_Open then
      Soc.Close;
    end if;
  end Close;

  -- Async stdin stuff
  function Async_Cb (Str : String) return Boolean is
    Msg : Message_Type;
  begin
    if Str = "" then
      -- Async stdin error
      Give_Up := True;
      return True;
    else
      Msg(1 .. Str'Length) := Str;
      Send (Msg, Str'Length);
      return False;
    end if;
  end Async_Cb;

  -- Set asynchronous stdin
  procedure Set_Async is
  begin
    if Use_Stdin then
      Async_Stdin.Set_Async (Async_Cb'Unrestricted_Access, Message_Type'Length);
      if Sys_Calls.Is_Blocking (Sys_Calls.Stdin) then
        Async_Stdin.Put_Line_Err ("Stdin is still blocking");
        raise Program_Error;
      end if;
    end if;
  end Set_Async;

  -- Signal callback
  procedure Signal_Cb is
  begin
    Give_Up := True;
  end Signal_Cb;

begin
  if Argument.Get_Nbre_Arg = 3
  and then Argument.Get_Parameter (1) = "-n" then
    Use_Stdin := False;
    Mode_Index := 2;
  else
    Use_Stdin := True;
    Mode_Index := 1;
  end if;

  -- Parse Arg, port name or num
  if Argument.Get_Nbre_Arg /=  Mode_Index + 1 then
    raise Arg_Error;
  end if;

  if Argument.Get_Parameter (Mode_Index) = "-a" then
    Mode := Abus;
    Argument.Get_Parameter (Bus_Addr, Mode_Index + 1);
  elsif Argument.Get_Parameter (Mode_Index) = "-s" then
    Mode := Server;
    declare
      use type Tcp_Util.Remote_Port_List;
    begin
      Remote_Port_Def := Ip_Addr.Parse (Argument.Get_Parameter (
                                                       Mode_Index + 1));
      if Remote_Port_Def.Kind = Tcp_Util.Port_Name_Spec then
        Local_Port_Def := (Tcp_Util.Port_Name_Spec, Remote_Port_Def.Name);
      else
        Local_Port_Def := (Tcp_Util.Port_Num_Spec, Remote_Port_Def.Num);
      end if;
    exception
      when others =>
        raise Arg_Error;
    end;
  elsif Argument.Get_Parameter (Mode_Index) = "-c" then
    Mode := Client;
    begin
      Ip_Addr.Parse (Argument.Get_Parameter (Mode_Index + 1),
                     Remote_Host_Def, Remote_Port_Def);
    exception
      when others =>
        raise Arg_Error;
    end;
  elsif Argument.Get_Parameter (Mode_Index) = "-u"
  or else Argument.Get_Parameter (Mode_Index) = "-U" then
    Mode := Udp;
    declare
      use type Tcp_Util.Local_Port_List, Tcp_Util.Remote_Host_List,
               Socket.Port_Num;
    begin
      Ip_Addr.Parse (Argument.Get_Parameter (Mode_Index + 1),
                     Remote_Host_Def, Remote_Port_Def);
      if Remote_Host_Def.Kind = Tcp_Util.Host_Name_Spec
      and then Remote_Host_Def.Name.Is_Null then
        -- Only a port => broadcast
        Remote_Host_Def := (Kind => Tcp_Util.Host_Id_Spec,
                            Id => Socket.Bcast_Of (Socket.Local_Host_Id));
      end if;
      Send_Port_Def := (Tcp_Util.Port_Num_Spec, 0);
      if Remote_Port_Def.Kind = Tcp_Util.Port_Name_Spec then
        Send_Port_Def.Num := Socket.Port_Num_Of (
                  Send_Port_Def.Name.Image, Socket.Udp);
      else
        Send_Port_Def.Num := Remote_Port_Def.Num;
      end if;
      if Argument.Get_Parameter (Mode_Index) = "-u" then
        Send_Port_Def.Num := Send_Port_Def.Num - 1;
      else
        Send_Port_Def.Num := Send_Port_Def.Num + 1;
      end if;
    exception
      when others =>
        raise Arg_Error;
    end;
  else
    raise Arg_Error;
  end if;

  -- Init
  Give_Up := False;
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);
  Open;

  -- Main loop
  pragma Warnings (Off, "variable ""*"" is not modified in loop body");
  while not Give_Up loop
    pragma Warnings (On, "variable ""*"" is not modified in loop body");
    Event_Mng.Wait (Event_Mng.Infinite_Ms);
  end loop;

  -- Close
  Close;
  Async_Stdin.Set_Async;

exception
  when Arg_Error =>
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
  when Error : others =>
    Async_Stdin.Put_Line_Err ("Exception: "
                   & Ada.Exceptions.Exception_Name (Error) & " raised.");
    Basic_Proc.Set_Error_Exit_Code;
end T_Async;

