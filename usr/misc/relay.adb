-- Usage: pipe <mode> <channel>
-- <mode> ::= -s | -p
-- -p : reads text from stdin, each line is published on the channel
-- -s : subscribes to the channel and dump messages on stdout
--       only one publisher is accepted at a time
-- The channel destinations file is <channel>.chn

with Ada.Text_Io;

with Text_Handler, Argument, Sys_Calls, X_Mng,
     Socket, Tcp_Util, Channels, Async_Stdin;
procedure Pipe is

  -- Message type
  Max_Data_Size : constant := 1024;
  subtype Message_Type is String (1 .. Max_Data_Size);
  Message : Message_Type;
  Len : Natural;

  -- Name of the Channel
  Channel_Name : Text_Handler.Text(Tcp_Util.Max_Port_Name_Len);
  -- Suffix to build destinations file name
  File_Name_Suffix : constant String := ".chn";

  -- Mode
  type Mode_List is (Subscribe, Publish);
  Mode : Mode_List;

  -- End of processing
  Done : Boolean := False;

  -- Event occured
  Event : Boolean;

  -- Is stdin a tty
  Is_A_Tty : Boolean;

  procedure Usage is
  begin
    Sys_Calls.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                            & " <mode> <channel>");
    Sys_Calls.Put_Line_Error ("<mode> ::= -s | -p");
    Sys_Calls.Set_Error_Exit_Code;
  end Usage;

  -- Not used, needed for sender channel
  procedure Channel_Read_Cb (Message  : in Message_Type;
                             Length   : in Channels.Message_Length;
                             Diffused : in Boolean) is
  begin
    null;
  end Channel_Read_Cb;
  package My_Channel is new Channels.Channel ("dummy", Message_Type,
                                              Channel_Read_Cb);

  -- Sender may read stdin with this
  function Stdin_Cb (Str : in String) return Boolean is
    Len : Natural := Str'Length;
  begin
    if Len >= 1 and then Str(Str'Length) = Ascii.Eot then
      Len := Len - 1;
      Done := True;
    end if;
    if Len > 1 and then (Str(Len) = Ascii.Lf
                 or else Str(Len) = Ascii.Cr) then
      Len := Len - 1;
    end if;
    if Len = 1 and then Str(Len) = Ascii.Lf then
      Message(1) := Ascii.Cr;
    else
      Message (1 .. Len) := Str (1 .. Len);
    end if;
    if Len > 0 then
      My_Channel.Write (Message, Len);
    end if;
    return True;
  end Stdin_Cb;

  -- Read data from not a tty (file?)
  procedure Get_No_Tty is
  begin
    begin
      Ada.Text_Io.Get_Line (Message, Len);
    exception
      when Ada.Text_Io.End_Error =>
        Done := True;
        return;
    end;

    if Len = 0 then
      Message(1) := Ascii.Cr;
      Len := 1;
    end if;
    My_Channel.Write (Message, Len);
  end Get_No_Tty;
    

  -- Subscriber reads with this
  Read_Dscr   : Socket.Socket_Dscr;
  Channel_Message : My_Channel.Channel_Message_Type;
  procedure My_Read is new Socket.Receive (My_Channel.Channel_Message_Type);
  function Read_Cb (Fd : in X_Mng.File_Desc;
                    Read : in Boolean) return Boolean is
    use type X_Mng.File_Desc;

  begin
    if not Socket.Is_Open (Read_Dscr)
    or else Fd /= Socket.Fd_Of (Read_Dscr) then
      return False;
    end if;

    begin
      My_Read (Read_Dscr, Channel_Message, Len, False);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
        X_Mng.X_Del_CallBack (Fd, True);
        Socket.Close (Read_Dscr);
        Done := True;
        return True;
      when Socket.Soc_Would_Block =>
        return False;
      when others =>
        Sys_Calls.Put_Line_Error ("Read error");
        Sys_Calls.Set_Error_Exit_Code;
        X_Mng.X_Del_CallBack (Fd, True);
        Socket.Close (Read_Dscr);
        Done := True;
        return True;
    end;

    Len := Len - (Channel_Message.Diff'Size / 8);
    if Len = 1 and then Channel_Message.Data(1) = Ascii.Cr then
      Len := 0;
    end if;
    Ada.Text_Io.Put_Line (Channel_Message.Data(1 .. Len));
    Ada.Text_Io.Flush;

    return True;
  end Read_Cb;

  -- Subscriber accepts with this
  Accept_Dscr : Socket.Socket_Dscr;
  procedure Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                       Local_Dscr      : in Socket.Socket_Dscr;
                       Remote_Port_Num : in Tcp_Util.Port_Num;
                       Remote_Host_Id  : in Tcp_Util.Host_Id;
                       New_Dscr        : in Socket.Socket_Dscr) is
    Tmp_Dscr : Socket.Socket_Dscr;
    use type Socket.Socket_Dscr;
  begin
    -- Reject extra connection
    if Read_Dscr /= Socket.No_Socket then
      Tmp_Dscr := New_Dscr;
      Socket.Close (Tmp_Dscr);
      return;
    end if;
    Read_Dscr := New_Dscr;
    X_Mng.X_Add_CallBack (Socket.Fd_Of(New_Dscr),
                          True,
                          Read_Cb'Unrestricted_Access);
    Socket.Set_Blocking (Read_Dscr, False);
  end Accept_Cb;


begin

  -- 2 arguments
  if Argument.Get_Nbre_Arg /= 2 then
    Usage;
    return;
  end if;

  -- Store mode
  if Argument.Get_Parameter (Occurence => 1) = "-s" then
    Mode := Subscribe;
  elsif Argument.Get_Parameter (Occurence => 1) = "-p" then
    Mode := Publish;
  else
    Usage;
    return;
  end if;

  -- Store channel_name
  begin
    Argument.Get_Parameter (Channel_Name, Occurence => 2);
  exception
   when others =>
     Sys_Calls.Put_Line_Error ("Invalid channel_name "
                             & Argument.Get_Parameter (Occurence => 2));
     Usage;
     return;
  end;

  -- Init
  if Mode = Subscribe then
    declare
      Local_Port : Tcp_Util.Local_Port( Tcp_Util.Port_Name_Spec);
      Local_Port_Num : Tcp_Util.Port_Num;
    begin
      Local_Port.Name(1 .. Text_Handler.Length(Channel_Name))
                     := Text_Handler.Value(Channel_Name);
      Tcp_Util.Accept_From (Socket.Tcp_Header,
                            Local_Port,
                            Accept_Cb'Unrestricted_Access,
                            Accept_Dscr,
                            Local_Port_Num);
    exception
      when Socket.Soc_addr_In_Use =>
        Sys_Calls.Put_Line_Error ("Address of "
                                & Text_Handler.Value(Channel_Name)
                                & " already in use");
        Sys_Calls.Set_Error_Exit_Code;
        return;
      when Socket.Soc_Name_Not_Found =>
        Sys_Calls.Put_Line_Error ("Unknown channel "
                                & Text_Handler.Value(Channel_Name));
        Sys_Calls.Set_Error_Exit_Code;
        return;
    end;
  else
    My_Channel.Change_Channel_Name (Text_Handler.Value(Channel_Name));
    begin
      My_Channel.Add_Destinations (Text_Handler.Value(Channel_Name) &
                                   File_Name_Suffix);
    exception
      when Channels.Unknown_Channel =>
        Sys_Calls.Put_Line_Error ("Unknown channel "
                                & Text_Handler.Value(Channel_Name));
        Sys_Calls.Set_Error_Exit_Code;
        return;
      when Channels.File_Error =>
        Sys_Calls.Put_Line_Error ("Error processing destinations file "
                                & Text_Handler.Value(Channel_Name)
                                & File_Name_Suffix);
        Sys_Calls.Set_Error_Exit_Code;
        return;
    end;
    begin
      Async_Stdin.Set_Async (Stdin_Cb'Unrestricted_Access, Max_Data_Size);
      Is_A_Tty := True;
    exception
      when Async_Stdin.Not_A_Tty =>
        Event := X_Mng.Select_No_X (500);
        Is_A_Tty := False;
    end;
  end if;
  


  -- Main loop
  loop
    if Mode = Subscribe then

       Event := X_Mng.Select_No_X (-1);

     else

       if not Is_A_Tty then
         Event := X_Mng.Select_No_X (0);
         Get_No_Tty;
       else
         Event := X_Mng.Select_No_X (-1);
       end if;

     end if;
     exit when Done;

  end loop;

  if Mode = Publish then
    My_Channel.Del_All_Destinations;
    if Is_A_Tty then
      Async_Stdin.Set_Async;
    end if;
  end if;

end Pipe;

