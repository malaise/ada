with Ada.Exceptions, Ada.Text_Io;
with Text_Handler, Argument, X_Mng, Socket, Tcp_Util;
procedure T_Tcp_Util is
  Arg_Error : exception;

  Server : Boolean;
  Server_Name : Text_Handler.Text (80);
  Server_Port_Name : constant String := "test_tcp";

  Give_Up : Boolean;

  Delay_Try : constant Duration := 10.0;
  Nb_Try : constant := 9;

  Accept_Dscr, The_Dscr : Socket.Socket_Dscr;

  type Message_Type is record
    Len : Positive;
    Num : Positive;
    Str : String (1 .. 80);
  end record;
  Message : Message_Type;
  Str : Constant String := "Ah que coucou!";
  procedure My_Read is new Socket.Receive (Message_Type);
  procedure My_Send is new Socket.Send (Message_Type);

  procedure Read_Cb (Fd : in X_Mng.File_Desc);

  procedure Send (Msg : in String) is
  begin
    My_Send (The_Dscr, Message);
    Ada.Text_Io.Put_Line (Msg & " sent");
  exception
    when others =>
      Ada.Text_Io.Put_Line ("Sending " & Msg & " failed.");
  end Send;


  procedure Connect_Cb (Remote_Port_Num : in Tcp_Util.Port_Num;
                        Remote_Host_Id  : in Tcp_Util.Host_Id;
                        Connected       : in Boolean;
                        Dscr            : in Socket.Socket_Dscr) is
  begin
    if Connected then
      Ada.Text_Io.Put_Line ("Connected");
      The_Dscr := Dscr;
      X_Mng.X_Add_CallBack (Socket.Fd_Of(Dscr),
                            True,
                            Read_Cb'Unrestricted_Access);
    else
      Ada.Text_Io.Put_Line ("Not connected");
      Give_Up := True;
      return;
    end if;
    if not Server then
      Send ("First request");
    end if;
  end Connect_Cb;

  procedure Connect is
    Host : Tcp_Util.Remote_Host(Tcp_Util.Host_Name_Spec);
    Port : Tcp_Util.Remote_Port(Tcp_Util.Port_Name_Spec);
  begin
    Host.Name (1 .. Text_Handler.Length(Server_Name))
         := Text_Handler.Value(Server_Name);
    Port.Name (1 .. Server_Port_Name'Length) := Server_Port_Name;
    Tcp_Util.Connect_To (Socket.Tcp_Header,
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
      Ada.Text_Io.Put_Line ("Accepted");
    end if;
  end Accept_Cb;

  procedure Read_Cb (Fd : in X_Mng.File_Desc) is
    Len : Natural;
    use type X_Mng.File_Desc;
  begin
    if Server then
      Ada.Text_Io.Put ("Server: ");
    else
      Ada.Text_Io.Put ("Client: ");
    end if;
    if not Socket.Is_Open (The_Dscr) or else Fd /= Socket.Fd_Of (The_Dscr) then
      Ada.Text_Io.Put_Line ("Read Cb -> Unknown fd");
      return;
    end if;

    begin
      My_Read (The_Dscr, Message, Len, False);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
        Ada.Text_Io.Put ("Read Cb -> disconnected: Closing");
        X_Mng.X_Del_CallBack (Fd, True);
        Socket.Close (The_Dscr);
        if not Server then
          Ada.Text_Io.Put_Line (" - Waiting");
          delay 3.0;
          Connect;
        else
          Ada.Text_Io.New_Line;
        end if;
        return;
    end;

    Ada.Text_Io.Put_Line ("receives: >"
                   & Message.Str(1 .. Message.Len)
                   & "< num "
                   & Positive'Image(Message.Num));

    if not Server then
      Ada.Text_Io.Put_Line ("      Waiting");
      delay 3.0;
      Send ("Request");
    else
      Ada.Text_Io.Put_Line ("      Working");
      delay 5.0;
      Ada.Text_Io.Put_Line ("      Replying");
      Message.Num := Message.Num + 1;
      Send ("Reply");
    end if;
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
            delay 20.0;
        end;
      end loop;
    end;
  else
    Message.Num := 1;
    Message.Str (1 .. Str'Length) := Str;
    Message.Len := Str'Length;
    Connect;
  end if;
  Give_Up := False;

  declare
    Timeout : Boolean;
  begin
    loop
      Timeout := X_Mng.Select_No_X (1_000);
      exit when Give_Up;
    end loop;
  end;

exception
  when Arg_Error =>
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                   & " -c <server_host> | -s");
  when Error : others =>
    Ada.Text_Io.Put_Line ("Exception: "
                   & Ada.Exceptions.Exception_Name (Error));
end T_Tcp_Util;


