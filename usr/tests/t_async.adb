with Ada.Exceptions;
with Argument, Basic_Proc, Async_Stdin, Event_Mng, Socket, Tcp_Util, Ip_Addr;
procedure T_Async is

  Arg_Error : exception;

  -- Common message type
  subtype Message_Type is String (1 .. 1024);

  -- TCP stuff
  Port_Def : Tcp_Util.Local_Port;
  Port_Num : Tcp_Util.Port_Num;
  Tcp_Soc : Socket.Socket_Dscr;
  In_Overflow : Boolean;
  procedure Open;

  -- End of sending overflow
  procedure End_Ovf_Cb (Dscr : in  Socket.Socket_Dscr) is
    pragma Unreferenced (Dscr);
    use type Socket.Socket_Dscr;
  begin
    In_Overflow := False;
  end End_Ovf_Cb;

  -- For sending on Tcp
  function My_Send is new Tcp_Util.Send (Message_Type);
  procedure Send (Msg : in Message_Type; Len : in Positive) is
  begin
    if not Tcp_Soc.Is_Open then
      return;
    end if;
    if not In_Overflow then
      if not My_Send (Tcp_Soc, End_Ovf_Cb'Unrestricted_Access, Msg, Len) then
        In_Overflow := True;
      end if;
    end if;
  exception
    when Error : others =>
      Async_Stdin.Put_Line_Err ("Sending " & Msg(1 .. Len) & " failed cause "
        & Ada.Exceptions.Exception_Name (Error) & ".");
  end Send;

  -- When a message to read
  procedure Read_Cb (Dscr : in Socket.Socket_Dscr;
                     Msg : in Message_Type;
                     Len : in Natural) is
    pragma Unreferenced (Dscr);
  begin
    Async_Stdin.Put_Out (Msg(1 .. Len));
  end Read_Cb;
  package My_Rece is new Tcp_Util.Reception (Message_Type);

  -- When Soc_Read_0
  procedure Discon_Cb (Dscr : in Socket.Socket_Dscr) is
    pragma Unreferenced (Dscr);
  begin
    -- Accept connections again
    Open;
  end Discon_Cb;

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
    In_Overflow := False;
    My_Rece.Set_Callbacks (Tcp_Soc, Read_Cb'Unrestricted_Access,
                                  Discon_Cb'Unrestricted_Access);
    -- Only one connection at a time
    Tcp_Util.Abort_Accept (Socket.Tcp, Port_Num);
  end Accept_Cb;

  -- Open (accept) Tcp connection
  procedure Open is
    Acc_Soc : Socket.Socket_Dscr;
  begin
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
          Async_Stdin.Put_Line_Err ("Cannot accept. Maybe Close-wait. Waiting");
          Event_Mng.Wait (20_000);
      end;
    end loop;
  end Open;

  -- Async stdin stuff
  Give_Up : Boolean := False;
  function Async_Cb (Str : String) return Boolean is
    Msg : Message_Type;
  begin
    if Str = "" then
      Give_Up := True;
      return True;
    else
      Msg(1 .. Str'Length) := Str;
      Send (Msg, Str'Length);
      return False;
    end if;
  end Async_Cb;

  -- Signal callback
  procedure Signal_Cb is
  begin
    Async_Stdin.Put_Line_Err ("Aborted.");
    Give_Up := True;
  end Signal_Cb;

begin

  -- Parse Arg, port name or num
  if Argument.Get_Nbre_Arg /= 1 then
    raise Arg_Error;
  end if;
  declare
    Port : Tcp_Util.Remote_Port;
    use type Tcp_Util.Remote_Port_List;
  begin
    Port := Ip_Addr.Parse (Argument.Get_Parameter (1));
    if Port.Kind = Tcp_Util.Port_Name_Spec then
      Port_Def := (Tcp_Util.Port_Name_Spec, Port.Name);
    else
      Port_Def := (Tcp_Util.Port_Num_Spec, Port.Num);
    end if;
  exception
    when others =>
      raise Arg_Error;
  end;

  -- Init
  Give_Up := False;
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);
  Open;
  Async_Stdin.Set_Async (Async_Cb'Unrestricted_Access, Message_Type'Length);

  -- Main loop
  loop
    exit when Event_Mng.Wait (Event_Mng.Infinite_Ms) and then Give_Up;
  end loop;

  -- Close
  begin
    Tcp_Util.Abort_Accept (Socket.Tcp, Port_Num);
  exception
    when Tcp_Util.No_Such =>
      null;
  end;
  begin
    My_Rece.Remove_Callbacks (Tcp_Soc);
  exception
   when Tcp_Util.No_Such =>
     null;
  end;
  begin
    Tcp_Util.Abort_Send_And_Close (Tcp_Soc);
  exception
   when Tcp_Util.No_Such =>
     null;
  end;
  if Tcp_Soc.Is_Open then
    Tcp_Soc.Close;
  end if;
  Async_Stdin.Set_Async (null);

exception
  when Arg_Error =>
    Async_Stdin.Put_Line_Err ("Usage: "
            & Argument.Get_Program_Name & " <port_name_or_num>");
    Basic_Proc.Set_Error_Exit_Code;
  when Error : others =>
    Async_Stdin.Put_Line_Err ("Exception: "
                   & Ada.Exceptions.Exception_Name (Error) & "raised.");
    Basic_Proc.Set_Error_Exit_Code;
end T_Async;

