with Ada.Text_Io;
with Text_Handler, Argument, X_Mng, Socket, Tcp_Util;
procedure T_Tcp_Util is
  Arg_Error : exception;

  Server : Boolean;
  Server_Name : Text_Handler.Text (80);
  Server_Port_Name : constant String := "test_tcp";

  Give_Up : Boolean;

  The_Dscr : Socket.Socket_Dscr;

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


  procedure Connect_Cb (Remote_Port_Num : in Tcp_Util.Port_Num;
                        Remote_Host_Id  : in Tcp_Util.Host_Id;
                        Connected       : in Boolean;
                        Dscr            : in Socket.Socket_Dscr) is
  begin
    if Connected then
      Ada.Text_Io.Put_Line ("Connected");
      The_Dscr := Dscr;
      X_Mng.X_Add_CallBack (Socket.Fd_Of(Dscr),
                            Read_Cb'Unrestricted_Access);
    else
      Ada.Text_Io.Put_Line ("Not connected");
      Give_Up := True;
      return;
    end if;
    if not Server then
      begin
        My_Send (The_Dscr, Message);
        Ada.Text_Io.Put_Line ("First request sent");
      exception
        when Socket.Socket_Error =>
          Ada.Text_Io.Put_Line ("Sending first request failed.");
      end;
    end if;
  end Connect_Cb;

  procedure Connect is
    Host : Tcp_Util.Remote_Host(Tcp_Util.Host_Name_Spec);
    Port : Tcp_Util.Remote_Port(Tcp_Util.Port_Name_Spec);
  begin
    Host.Name (1 .. Text_Handler.Length(Server_Name))
         := Text_Handler.Value(Server_Name);
    Port.Name (1 .. Server_Port_Name'Length) := Server_Port_Name;
    Tcp_Util.Connect_To (Host, Port, 5.0, 3, Connect_Cb'Unrestricted_Access);
  end Connect;

  procedure Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                       Local_Dscr      : in Socket.Socket_Dscr;
                       Remote_Port_Num : in Tcp_Util.Port_Num;
                       Remote_Host_Id  : in Tcp_Util.Host_Id;
                       New_Dscr        : in Socket.Socket_Dscr) is
  begin
    The_Dscr := New_Dscr;
    X_Mng.X_Add_CallBack (Socket.Fd_Of(New_Dscr),
                          Read_Cb'Unrestricted_Access);
    Ada.Text_Io.Put_Line ("Accepted");
  end Accept_Cb;

  procedure Read_Cb (Fd : in X_Mng.File_Desc) is
    Len : Natural;
    Rec : Boolean;
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

    My_Read (The_Dscr, Message, Len, Rec, False);

    if not Rec  or else Len = 0 then
      if not Rec then
        Ada.Text_Io.Put_Line ("Read Cb -> no message");
      else
        Ada.Text_Io.Put_Line ("Read Cb -> empty message");
      end if;
      X_Mng.X_Del_CallBack (Fd);
      Socket.Close (The_Dscr);
      if not Server then
        Connect;
      end if;
      return;
    end if;

    Ada.Text_Io.Put_Line ("receives: >"
                   & Message.Str(1 .. Message.Len)
                   & "< num "
                   & Positive'Image(Message.Num));

    if not Server then
      Ada.Text_Io.Put_Line ("      Waiting");
      delay 3.0;
      begin
        My_Send (The_Dscr, Message);
        Ada.Text_Io.Put_Line ("      Request sent");
      exception
        when Socket.Socket_Error =>
          Ada.Text_Io.Put_Line ("     Sending request failed.");
      end;
    else
      Ada.Text_Io.Put_Line ("      Working");
      delay 5.0;
      Ada.Text_Io.Put_Line ("      Replying");
      Message.Num := Message.Num + 1;
      begin
        My_Send (The_Dscr, Message);
        Ada.Text_Io.Put_Line ("      Reply sent");
      exception
        when Socket.Socket_Error =>
          Ada.Text_Io.Put_Line ("     Sending reply failed.");
      end;
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
      Tcp_Util.Accept_From (Port, Accept_Cb'Unrestricted_Access, The_Dscr,
                            Port_Num);
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

end T_Tcp_Util;


