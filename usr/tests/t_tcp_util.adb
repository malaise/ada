with Ada.Exceptions, Ada.Text_Io;
with Text_Handler, Argument, Lower_Str, X_Mng, Socket, Tcp_Util;
procedure T_Tcp_Util is
  Arg_Error : exception;

  Server : Boolean;
  Server_Name : Text_Handler.Text (80);
  Server_Port_Name : constant String := "test_tcp";

  Give_Up : Boolean;
  In_Ovf : Boolean := False;
  Server_Nb_Overflow : Natural := 0;

  Delay_Try : constant Duration := 10.0;
  Nb_Try : constant := 9;

  Accept_Dscr, The_Dscr : Socket.Socket_Dscr;

  type Message_Type is record
    Len : Positive;
    Num : Positive;
    Str : String (1 .. 80);
    Dummy : String (1 .. 11_000);
  end record;
  Message, Received_Message : Message_Type;
  Str : Constant String := "Ah que coucou!";
  procedure My_Read is new Socket.Receive (Message_Type);
  function My_Send is new Tcp_Util.Send (Message_Type);

  function Read_Cb (Fd : in X_Mng.File_Desc; Read : in Boolean) return Boolean;
  procedure End_Ovf_Cb (Dscr : in  Socket.Socket_Dscr);

  function Send (Msg : in String) return Boolean is
  begin
    if not Socket.Is_Open (The_Dscr) then
      Ada.Text_Io.Put_Line (Msg & " not sending cause not open");
      return False;
    end if;
    if Server then
      if not In_Ovf then
        if My_Send (The_Dscr, End_Ovf_Cb'Unrestricted_Access, Message) then
          Ada.Text_Io.Put_Line (Msg & " sent num " & Integer'Image(Message.Num));
          return True;
        else
          In_Ovf := True;
          Ada.Text_Io.Put_Line (Msg & "                     sending overflow");
        end if;
      else
         Ada.Text_Io.Put_Line (Msg & " not sending cause in overflow");
      end if;
      return False;
    else
      if In_Ovf then
        Ada.Text_Io.Put_Line (Msg & " not sending cause in overflow");
        return False;
      end if;
      while My_Send (The_Dscr, End_Ovf_Cb'Unrestricted_Access, Message) loop
        Ada.Text_Io.Put_Line (Msg & " sent num " & Integer'Image(Message.Num));
        Message.Num := Message.Num + 1;
      end loop;
      In_Ovf := True;
      Ada.Text_Io.Put_Line (Msg & "                     sending overflow");
    end if;
    return False;
  exception
    when Error : others =>
      Ada.Text_Io.Put_Line ("Sending " & Msg & " failed cause "
        & Lower_Str (Ada.Exceptions.Exception_Name (Error)) & ".");
      return False;
  end Send;

  procedure End_Ovf_Cb (Dscr : in  Socket.Socket_Dscr) is
    use type Socket.Socket_Dscr;
  begin
    if Dscr /= The_Dscr then
      Ada.Text_Io.Put_Line ("End of overflow on invalid dscr");
      return;
    end if;
    Ada.Text_Io.Put_Line ("End of overflow Cb ->                    End of overflow");
    In_Ovf := False;
  end End_Ovf_Cb;

  procedure Connect_Cb (Remote_Port_Num : in Tcp_Util.Port_Num;
                        Remote_Host_Id  : in Tcp_Util.Host_Id;
                        Connected       : in Boolean;
                        Dscr            : in Socket.Socket_Dscr) is
    dummy : Boolean;
  begin
    if Connected then
      Ada.Text_Io.Put_Line ("Connected");
      The_Dscr := Dscr;
      X_Mng.X_Add_CallBack (Socket.Fd_Of(Dscr),
                            True,
                            Read_Cb'Unrestricted_Access);
      Socket.Set_Blocking (The_Dscr, False);
    else
      Ada.Text_Io.Put_Line ("Not connected");
      Give_Up := True;
      return;
    end if;
    if not Server then
      dummy := Send ("First request");
    end if;
  end Connect_Cb;

  procedure Connect is
    Host : Tcp_Util.Remote_Host(Tcp_Util.Host_Name_Spec);
    Port : Tcp_Util.Remote_Port(Tcp_Util.Port_Name_Spec);
    Result : Boolean;
  begin
    Host.Name (1 .. Text_Handler.Length(Server_Name))
         := Text_Handler.Value(Server_Name);
    Port.Name (1 .. Server_Port_Name'Length) := Server_Port_Name;
    Result := Tcp_Util.Connect_To (Socket.Tcp_Header,
                                   Host, Port,
                                   Delay_Try, Nb_Try,
                                   Connect_Cb'Unrestricted_Access);
  end Connect;

  procedure Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                       Local_Dscr      : in Socket.Socket_Dscr;
                       Remote_Port_Num : in Tcp_Util.Port_Num;
                       Remote_Host_Id  : in Tcp_Util.Host_Id;
                       New_Dscr        : in Socket.Socket_Dscr) is
    use type Socket.Socket_Dscr;
    Tmp_Dscr : Socket.Socket_Dscr;
  begin
    if The_Dscr /= Socket.No_Socket then
      Ada.Text_Io.Put_Line ("Rejected");
      Tmp_Dscr := New_Dscr;
      Socket.Close (Tmp_Dscr);
    else
      The_Dscr := New_Dscr;
      X_Mng.X_Add_CallBack (Socket.Fd_Of(New_Dscr),
                            True,
                            Read_Cb'Unrestricted_Access);
      Socket.Set_Blocking (The_Dscr, False);
      Ada.Text_Io.Put_Line ("Accepted");
    end if;
  end Accept_Cb;

  procedure Wait (Dur : in Duration) is
    Event : Boolean;
  begin
    Event := X_Mng.Select_No_X (Integer (Dur) * 1_000);
    if Event then
      Ada.Text_Io.Put_Line ("Timer/Event");
    else
      Ada.Text_Io.Put_Line ("Timeout");
    end if;
  end Wait;

  function Read_Cb (Fd : in X_Mng.File_Desc; Read : in Boolean)
  return Boolean is
    Len : Natural;
    Res : Boolean;
    use type X_Mng.File_Desc;
  begin
    if Server then
      Ada.Text_Io.Put ("Server: ");
    else
      Ada.Text_Io.Put ("Client: ");
    end if;
    if not Socket.Is_Open (The_Dscr) or else Fd /= Socket.Fd_Of (The_Dscr) then
      Ada.Text_Io.Put_Line ("Read Cb -> Unknown fd");
      return False;
    end if;

    begin
      My_Read (The_Dscr, Received_Message, Len, False);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
        Ada.Text_Io.Put ("Read Cb -> disconnected: Closing");
        X_Mng.X_Del_CallBack (Fd, True);
        if In_Ovf then
          Tcp_Util.Abort_Send_and_Close (The_Dscr);
          In_Ovf := False;
        else
          Socket.Close (The_Dscr);
        end if;
        if not Server then
          Ada.Text_Io.Put_Line (" - Waiting");
          delay 3.0;
          Connect;
        else
          Ada.Text_Io.New_Line;
        end if;
        return True;
      when Socket.Soc_Would_Block =>
        Ada.Text_Io.Put_Line ("Read Cb ->                     underflow");
        return False;
    end;

    Message := Received_Message;
    Ada.Text_Io.Put_Line ("receives: >"
                   & Message.Str(1 .. Message.Len)
                   & "< num "
                   & Positive'Image(Message.Num));

    if not Server then
      Ada.Text_Io.Put_Line ("      Working");
      delay 0.3;
      Res := Send ("Request");
    else
      Ada.Text_Io.Put_Line ("      Working");
      delay 0.1;
      Ada.Text_Io.Put_Line ("      Replying");
      Message.Num := Message.Num + 10;
      if not Send ("Reply") and then In_Ovf then
        if Server_Nb_Overflow < 50 then
          Ada.Text_Io.Put_Line ("      Not responding to client in overflow");
          Server_Nb_Overflow := Server_Nb_Overflow + 1;
        else
          -- Cancel with this bad client
          Ada.Text_Io.Put_Line ("      Closing with client in overflow");
          X_Mng.X_Del_CallBack (Fd, True);
          Tcp_Util.Abort_Send_and_Close (The_Dscr);
          In_Ovf := False;
          Server_Nb_Overflow := 0;
        end if;
      end if;
    end if;
    return False;
  end Read_Cb;

begin
  -- Server or client
  if Argument.Get_Nbre_Arg = 1 and then Argument.Get_Parameter = "-s" then
    Server := True;
  elsif Argument.Get_Nbre_Arg = 2 and then Argument.Get_Parameter = "-c" then
    Server := False;
    Argument.Get_Parameter (Server_Name, 2);
  else
    raise Arg_Error;
  end if;

  Give_Up := False;
  if Server then
    declare
      Port : Tcp_Util.Local_Port;
      Port_num : Tcp_Util.Port_num;
    begin
      Port.Name (1 .. Server_Port_Name'Length) := Server_Port_Name;
      loop
        begin
          Tcp_Util.Accept_From (Socket.Tcp_Header,
                                Port,
                                Accept_Cb'Unrestricted_Access,
                                Accept_Dscr,
                                Port_Num);
          exit;
        exception
          when Socket.Soc_addr_In_Use =>
            Ada.Text_Io.Put_Line ("Cannot accept. Maybe Close-wait. Waiting");
            Wait (20.0);
        end;
      end loop;
    end;
  else
    Message.Num := 1;
    Message.Str (1 .. Str'Length) := Str;
    Message.Len := Str'Length;
    Connect;
  end if;

  loop
    Wait (1.0);
    exit when Give_Up;
  end loop;

exception
  when Arg_Error =>
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                   & " -c <server_host> | -s");
  when Error : others =>
    Ada.Text_Io.Put_Line ("Exception: "
                   & Ada.Exceptions.Exception_Name (Error));
end T_Tcp_Util;


