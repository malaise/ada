with System, Ada.Calendar;
with Text_Handler, Sys_Calls, Socket, Tcp_Util, Dynamic_List,
     Event_Mng, Assertion;
package body Channels is

  Byte_Size : constant := System.Storage_Unit;

  package Host_List_Mng is new Dynamic_List (Tcp_Util.Remote_Host);

  -- Destination
  type Dest_Rec is record
    Host_Name : Tcp_Util.Remote_Host (Tcp_Util.Host_Name_Spec);
    Host_Id : Socket.Host_Id;
    Dscr : Socket.Socket_Dscr := Socket.No_Socket;
    Fd : Event_Mng.File_Desc;
  end record;
  package Dest_List_Mng is new Dynamic_List(Dest_Rec);

  function Fd_Match (D1, D2 : Dest_Rec) return Boolean is
    use type Event_Mng.File_Desc;
  begin
    return D1.Fd = D2.Fd;
  end Fd_Match;
  procedure Fd_Search is new Dest_List_Mng.Search (Fd_Match);

  function Host_Id_Match (D1, D2 : Dest_Rec) return Boolean is
    use type Socket.Host_Id;
  begin
    return D1.Host_Id = D2.Host_Id;
  end Host_Id_Match;
  procedure Host_Id_Search is new Dest_List_Mng.Search (Host_Id_Match);

  function Dscr_Match (D1, D2 : Dest_Rec) return Boolean is
    use type Socket.Socket_Dscr;
  begin
    return D1.Dscr = D2.Dscr;
  end Dscr_Match;
  procedure Dscr_Search is new Dest_List_Mng.Search (Dscr_Match);

  function Host_Name_Match (D1, D2 : Dest_Rec) return Boolean is
    use type Tcp_Util.Remote_Host;
  begin
    return D1.Host_Name = D2.Host_Name;
  end Host_Name_Match;
  procedure Host_Name_Search is new Dest_List_Mng.Search (Host_Name_Match);

  -- Sender
  type Send_Rec is record
    Dscr : Socket.Socket_Dscr := Socket.No_Socket;
    Fd : Event_Mng.File_Desc;
  end record;
  package Send_List_Mng is new Dynamic_List(Send_Rec);

  function Fd_Match (D1, D2 : Send_Rec) return Boolean is
    use type Event_Mng.File_Desc;
  begin
    return D1.Fd = D2.Fd;
  end Fd_Match;
  procedure Fd_Search is new Send_List_Mng.Search (Fd_Match);

  function Dscr_Match (D1, D2 : Send_Rec) return Boolean is
    use type Socket.Socket_Dscr;
  begin
    return D1.Dscr = D2.Dscr;
  end Dscr_Match;
  procedure Dscr_Search is new Send_List_Mng.Search (Dscr_Match);

  -- Reply
  package Reply_List_Mng is new Dynamic_List (Socket.Socket_Dscr);

  -- The channel
  type Read_Kind is (None, Send, Dest);
  type Channel_Rec is record
    Init : Boolean := False;
    Name : Text_Handler.Text (Tcp_Util.Max_Port_Name_Len);
    Period : Ada.Calendar.Day_Duration;
    Accept_Num : Tcp_Util.Port_Num := 0;
    Dests : Dest_List_Mng.List_Type;
    Sends : Send_List_Mng.List_Type;
    Replies : Reply_List_Mng.List_Type;
  end record;

  -- Misc toolkits
  -- Close a (overflow) connection
  procedure Close (Dscr : in out Socket.Socket_Dscr) is
  begin
    Tcp_Util.Abort_Send_And_Close (Dscr);
  exception
    when Tcp_Util.No_Such =>
      Socket.Close (Dscr);
  end Close;

  -- Remove tailing spaces of a string
  function Parse (Str : String) return String is
  begin
    for I in reverse Str'Range loop
      if Str(I) /= ' ' then
        return Str (Str'First .. I);
      end if;
    end loop;
    return Str;
  end Parse;

  function Get_Period (Channel : String) return Ada.Calendar.Day_Duration is
    Set : Boolean;
    Tru : Boolean;
    Val : String (1 .. 6);
    Len : Natural;
    Default_Period : constant Ada.Calendar.Day_Duration := 1.0;
  begin
    Sys_Calls.Getenv ("Channel_" & Channel & "_period", Set, Tru, Val, Len);
    if not Set or else Tru then
      return Default_Period;
    end if;
    return Ada.Calendar.Day_Duration'Value (Val(1 .. Len));
  exception
    when others =>
      return Default_Period;
  end Get_Period;

  package File is
    -- All may raise File_Error

    -- Open a file and look for a channel
    procedure Open (File_Name : in String; Channel_Name : in String);

    procedure Close;

    -- May raise End_Error
    End_Error : exception;
    function Next_Host return Tcp_Util.Remote_Host;

  end File;
  package body File is separate;


  package body Channel is

    -- Current channel state
    Channel_Dscr : Channel_Rec;

    procedure Channel_Read is new Socket.Receive (Channel_Message_Type);
    function Channel_Send is new Tcp_Util.Send (Channel_Message_Type);

    procedure Delete_Current_Dest is
    begin
      Dest_List_Mng.Delete (Channel_Dscr.Dests);
    exception
      when Dest_List_Mng.Not_In_List =>
        Dest_List_Mng.Delete (Channel_Dscr.Dests, Dest_List_Mng.Prev);
    end Delete_Current_Dest;

    procedure Delete_Current_Send is
    begin
      Send_List_Mng.Delete (Channel_Dscr.Sends);
    exception
      when Send_List_Mng.Not_In_List =>
        Send_List_Mng.Delete (Channel_Dscr.Sends, Send_List_Mng.Prev);
    end Delete_Current_Send;

    procedure Init is
    begin
      -- Save Channel_Name from instantiation 
      if Channel_Dscr.Init then
        return;
      end if;
      Text_Handler.Set (Channel_Dscr.Name, Channel_Name);
      Channel_Dscr.Period := Get_Period (Channel_Name);
      Channel_Dscr.Init := True;
    exception
      when Constraint_Error =>
        raise Name_Too_Long;
    end Init;

    procedure Change_Channel_Name (New_Channel_Name : in String) is
      use type Socket.Port_Num;
    begin
      -- No subscribe
      if Channel_Dscr.Accept_Num /= 0 then
        raise Channel_Active;
      end if;
      -- No destination
      if not Dest_List_Mng.Is_Empty (Channel_Dscr.Dests) then
        raise Channel_Active;
      end if;
      -- Store new name
      Text_Handler.Set (Channel_Dscr.Name, New_Channel_Name);
      Channel_Dscr.Period := Get_Period (New_Channel_Name);
      Channel_Dscr.Init := True;
    exception
      when Constraint_Error =>
        raise Name_Too_Long;
    end Change_Channel_Name;

    -- Connection callback (used in read callback on destination
    --  disconnection
    procedure Connect_Cb (Remote_Port_Num : in Tcp_Util.Port_Num;
                          Remote_Host_Id  : in Tcp_Util.Host_Id;
                          Connected       : in Boolean;
                          Dscr            : in Socket.Socket_Dscr);

    -- General reception callback
    function Read_Cb (Sender : in Boolean; Fd : in Event_Mng.File_Desc)
                     return Boolean is 
      S_Rec : Send_Rec;
      D_Rec : Dest_Rec;
      Dscr : Socket.Socket_Dscr;
      Msg : Channel_Message_Type;
      Len : Natural;
    begin
      if Sender then
        -- Look for sender. Unhook Fd if not found (bug).
        S_Rec.Fd := Fd;
        begin
          Fd_Search (Channel_Dscr.Sends, S_Rec, From_Current => False);
        exception
          when Send_List_Mng.Not_In_List =>
            Event_Mng.Del_Fd_Callback (Fd, True);
            return False;
        end;
        Send_List_Mng.Read (Channel_Dscr.Sends, S_Rec, Send_List_Mng.Current);
        Dscr := S_Rec.Dscr;
      else
        -- Look for destination. Unhook Fd if not found (bug).
        D_Rec.Fd := Fd;
        begin
          Fd_Search (Channel_Dscr.Dests, D_Rec, From_Current => False);
        exception
          when Dest_List_Mng.Not_In_List =>
            Event_Mng.Del_Fd_Callback (Fd, True);
            return False;
        end;
        Dest_List_Mng.Read (Channel_Dscr.Dests, D_Rec, Dest_List_Mng.Current);
        Dscr := D_Rec.Dscr;
      end if;
      -- Read message
      begin
        Channel_Read (Dscr, Msg, Len);
      exception
        when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
          -- Sender / Dest has disconnected
          Event_Mng.Del_Fd_Callback (Fd, True);
          Close (Dscr);
          if Sender then
            Delete_Current_Send;
          else
            declare
              Res : Boolean;
              Port : Tcp_Util.Remote_Port (Tcp_Util.Port_Name_Spec);
            begin
              -- Update record
              D_Rec.Dscr := Socket.No_Socket;
              D_Rec.Fd := 0;
              Dest_List_Mng.Modify (Channel_Dscr.Dests, D_Rec,
                                    Dest_List_Mng.Current);
              -- Retry to connect
              Port.Name (1 .. Text_Handler.Length (Channel_Dscr.Name))
                    := Text_Handler.Value (Channel_Dscr.Name);
              Res := Tcp_Util.Connect_To (Socket.Tcp_Header,
                                          D_Rec.Host_Name, Port,
                                          Channel_Dscr.Period, 0,
                                          Connect_Cb'Unrestricted_Access);
            exception
              when others =>
                -- Failure
                Delete_Current_Dest;
            end;
          end if;
          return False;
        when Socket.Soc_Len_Err =>
          -- Invalid length
          Event_Mng.Del_Fd_Callback (Fd, True);
          Close (Dscr);
          if Sender then
            Delete_Current_Send;
          else
            Delete_Current_Dest;
          end if;
          return False;
        when Socket.Soc_Would_Block =>
          return False;
      end;
      -- Call callback
      if not Reply_List_Mng.Is_Empty (Channel_Dscr.Replies) then
        Reply_List_Mng.Move_To (Channel_Dscr.Replies,
           Reply_List_Mng.Prev, 0, False);
      end if;
      Reply_List_Mng.Insert (Channel_Dscr.Replies, Dscr);
      Read_Cb (Msg.Data, Len - (Msg.Diff'Size / Byte_Size), Msg.Diff);
      Reply_List_Mng.Delete (Channel_Dscr.Replies, Reply_List_Mng.Prev);
      return True;
    end Read_Cb;

    function Rec_Read_Cb (Fd : in Event_Mng.File_Desc; Read : in Boolean)
                     return Boolean is 
    begin
      return Read_Cb (True, Fd);
    end Rec_Read_Cb;

    procedure Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                         Local_Dscr      : in Socket.Socket_Dscr;
                         Remote_Port_Num : in Tcp_Util.Port_Num;
                         Remote_Host_Id  : in Tcp_Util.Host_Id;
                         New_Dscr        : in Socket.Socket_Dscr) is
      use type Socket.Port_Num;
    begin
      -- Discard and close if channel is closed
      if Channel_Dscr.Accept_Num = 0 then
        declare
          Tmp_Socket : Socket.Socket_Dscr := New_Dscr;
        begin
          Socket.Close (Tmp_Socket);
        exception
          when others => null;
        end;
        return;
      else
        -- Insert new sender
        Send_List_Mng.Insert (Channel_Dscr.Sends,
                              (Dscr => New_Dscr,
                               Fd   => Socket.Fd_Of (New_Dscr)));

        -- Hook fd to receive data
        Event_Mng.Add_Fd_Callback (Socket.Fd_Of (New_Dscr), True,
                              Rec_Read_Cb'Unrestricted_Access);
        Socket.Set_Blocking (New_Dscr, False);
      end if;
    end Accept_Cb;

    -- Subscription
    -- Allow connections to local channel
    procedure Subscribe is
      Port : Tcp_Util.Local_Port (Tcp_Util.Port_Name_Spec);
      Accept_Dscr : Socket.Socket_Dscr;
      use type Socket.Port_Num;
    begin
      Init;
      -- Error if already subscribed to channel
      if Channel_Dscr.Accept_Num /= 0 then
        raise Already_Subscribed;
      end if;
      -- Build port record
      Port.Name (1 .. Text_Handler.Length (Channel_Dscr.Name))
          := Text_Handler.Value (Channel_Dscr.Name); 
      
      -- Accept
      begin
        Tcp_Util.Accept_From (Socket.Tcp_Header, Port,
                              Accept_Cb'Unrestricted_Access,
                              Accept_Dscr,  Channel_Dscr.Accept_Num);
      exception
        when Socket.Soc_Name_Not_Found =>
          raise Unknown_Channel;
      end;
    end Subscribe;

    -- Close all connections and forbid new connections
    procedure Unsubscribe is
      Rec : Send_Rec;
      use type Socket.Port_Num;
    begin
      if Channel_Dscr.Accept_Num = 0 then
        raise Not_Subscribed;
      end if;

      -- Abort accept
      Tcp_Util.Abort_Accept (Channel_Dscr.Accept_Num);
      Channel_Dscr.Accept_Num := 0;

      -- Close all connections
      if not Send_List_Mng.Is_Empty (Channel_Dscr.Sends) then
        -- Rewind and close all connections
        Send_List_Mng.Move_To (Channel_Dscr.Sends,
                               Send_List_Mng.Next, 0, False);
        loop
          Send_List_Mng.Read (Channel_Dscr.Sends, Rec, Send_List_Mng.Current);
          -- Unhook fd receiving data
          Event_Mng.Del_Fd_Callback (Socket.Fd_Of (Rec.Dscr), True);
          Close (Rec.Dscr);
          exit when Send_List_Mng.Get_Position (Channel_Dscr.Sends)
                  = Send_List_Mng.List_Length (Channel_Dscr.Sends);
          Send_List_Mng.Move_To (Channel_Dscr.Sends);
        end loop;
        -- Delete list
        Send_List_Mng.Delete_List (Channel_Dscr.Sends, False);
      end if;
    end Unsubscribe;

    ----------------------

    function Snd_Read_Cb (Fd : in Event_Mng.File_Desc; Read : in Boolean)
                     return Boolean is 
    begin
      return Read_Cb (False, Fd);
    end Snd_Read_Cb;

    procedure Connect_Cb (Remote_Port_Num : in Tcp_Util.Port_Num;
                          Remote_Host_Id  : in Tcp_Util.Host_Id;
                          Connected       : in Boolean;
                          Dscr            : in Socket.Socket_Dscr) is
      Dest : Dest_Rec;
    begin
      -- Find record
      Dest.Host_Id := Remote_Host_Id;
      begin
        Host_Id_Search (Channel_Dscr.Dests, Dest, From_Current => False);
      exception
        when Dest_List_Mng.Not_In_List =>
          -- Bug?
          Socket.Close (Dest.Dscr);
          return;
      end;

      -- Update Dscr and Fd
      Dest_List_Mng.Read (Channel_Dscr.Dests, Dest, Dest_List_Mng.Current);
      Dest.Dscr := Dscr;
      Dest.Fd := Socket.Fd_Of (Dscr);
      Dest_List_Mng.Modify (Channel_Dscr.Dests, Dest, Dest_List_Mng.Current);

      -- Hook fd to receive data (replies)
      Event_Mng.Add_Fd_Callback (Socket.Fd_Of (Dscr), True,
                            Snd_Read_Cb'Unrestricted_Access);
      Socket.Set_Blocking (Dscr, False);
    end Connect_Cb;


    procedure Build_Host_Port (Host_Name : in String;
                               Host : out Tcp_Util.Remote_Host;
                               Port : out Tcp_Util.Remote_Port) is
    begin
      -- Build host and port records
      if Host_Name'Length > Tcp_Util.Max_Host_Name_Len then
        raise Name_Too_Long;
      end if; 
      Host := (Kind => Tcp_Util.Host_Name_Spec, Name => (others => ' '));
      Host.Name (1 .. Host_Name'Length) := Host_Name; 
      Port := (Kind => Tcp_Util.Port_Name_Spec, Name => (others => ' '));
      Port.Name (1 .. Text_Handler.Length (Channel_Dscr.Name))
          := Text_Handler.Value (Channel_Dscr.Name); 
    end Build_Host_Port;

    -- Add destinations from file
    procedure Add_Destinations (File_Name : in String) is
      Host : Tcp_Util.Remote_Host;
      List : Host_List_Mng.List_Type;
    begin
      -- Store hosts (fully parse file)
      File.Open (File_Name, Text_Handler.Value (Channel_Dscr.Name) );
      loop
        begin
          Host := File.Next_Host;
        exception
          when File.End_Error =>
            File.Close;
            exit;
          when File_Error =>
            File.Close;
            raise;
        end;
        Host_List_Mng.Insert (List, Host);
      end loop;

      if Host_List_Mng.Is_Empty (List) then
        return;
      end if;

      Host_List_Mng.Move_To (List, Host_List_Mng.Next, 0 , False);
      loop
        Host_List_Mng.Read (List, Host, Host_List_Mng.Current);
        begin
          Add_Destination (Parse (Host.Name));
        exception
          when Destination_Already | Unknown_Destination =>
            null;
          when others =>
            raise;
        end;
        begin
          Host_List_Mng.Move_To (List);
        exception
          when Host_List_Mng.Not_In_List =>
            Host_List_Mng.Delete_List (List, Deallocate => True);
            exit;
        end;
      end loop;

    exception
      when others =>
        Host_List_Mng.Delete_List (List, Deallocate => True);
        raise;      
    end Add_Destinations;

    -- Add a new recipient
    procedure Add_Destination (Host_Name : in String) is
      Dest : Dest_Rec;
      Host : Tcp_Util.Remote_Host;
      Port : Tcp_Util.Remote_Port;
      Result : Boolean;
    begin
      Init;
      -- Build host and port records
      Build_Host_Port (Host_Name, Host, Port);

      -- Check this host not in dest list
      begin
        Dest.Host_Id := Socket.Host_Id_Of (Host_Name);
      exception
        when Socket.Soc_Name_Not_Found =>
          raise Unknown_Destination;
      end;
      begin
        Host_Id_Search (Channel_Dscr.Dests, Dest,  From_Current => False);
        raise Destination_Already;
      exception
        when Dest_List_Mng.Not_In_List =>
          null;
      end;

      -- Insert host, dscr and fd in dest list
      Dest.Host_Name := Host;
      Dest.Dscr := Socket.No_Socket;
      Dest.Fd := 0;
      Dest_List_Mng.Insert (Channel_Dscr.Dests, Dest);

      -- Try to connect each sec indefinitely
      begin
        Result := Tcp_Util.Connect_To (Socket.Tcp_Header, Host, Port,
                                       Channel_Dscr.Period, 0,
                                       Connect_Cb'Unrestricted_Access);
      exception
        when Socket.Soc_Name_Not_Found =>
          -- Host/port name is not fount in hosts/services
          -- Check host name
          declare
            Id : Socket.Host_Id;
          begin
            Id := Socket.Host_Id_Of (Host_Name);
            -- Host is ok
            raise Unknown_Channel;
          exception
            when Socket.Soc_Name_Not_Found =>
              -- Host unknown
              raise Unknown_Destination;
          end;
      end;
    end Add_Destination;

    -- Close current connection in Dest list (may be pending)
    procedure Close_Current_Connection is
      Dest : Dest_Rec;
      Host : Tcp_Util.Remote_Host;
      Port : Tcp_Util.Remote_Port;
      use type Socket.Socket_Dscr;
    begin
      Dest_List_Mng.Read (Channel_Dscr.Dests, Dest, Dest_List_Mng.Current);
      if Dest.Dscr = Socket.No_Socket then
        -- Pending connection
        Build_Host_Port ("", Host, Port);
        Tcp_Util.Abort_Connect (Dest.Host_Name, Port);
      else
        -- Unhook fd and close
        Event_Mng.Del_Fd_Callback (Dest.Fd, True);
        Close (Dest.Dscr);
      end if;
      -- Delete rec
      Delete_Current_Dest;
    end Close_Current_Connection;

    -- Delete a recipient
    procedure Del_Destination (Host_Name : in String) is
      Dest : Dest_Rec;
      Host : Tcp_Util.Remote_Host;
      Port : Tcp_Util.Remote_Port;
    begin
      Init;
      -- Build host and port records
      Build_Host_Port (Host_Name, Host, Port);

      -- Check this host not in dest list
      begin
        Dest.Host_Id := Socket.Host_Id_Of (Host_Name);
      exception
        when Socket.Soc_Name_Not_Found =>
          raise Unknown_Destination;
      end;
      begin
        Host_Id_Search (Channel_Dscr.Dests, Dest,  From_Current => False);
      exception
        when Dest_List_Mng.Not_In_List =>
          raise Unknown_Destination;
      end;

      -- Close (pending) connection
      Close_Current_Connection;

    end Del_Destination;

    -- Delete all recipients
    procedure Del_All_Destinations is
      Dest : Dest_Rec;
    begin
      -- Empty list?
      if Dest_List_Mng.Is_Empty (Channel_Dscr.Dests) then
        return;
      end if;

      -- Delete all connections
      Dest_List_Mng.Move_To (Channel_Dscr.Dests, Dest_List_Mng.Next, 0, False); 
      loop
        Dest_List_Mng.Read (Channel_Dscr.Dests, Dest, Dest_List_Mng.Current);
        Close_Current_Connection;
        exit when Dest_List_Mng.Is_Empty (Channel_Dscr.Dests);
      end loop;

    end Del_All_Destinations;

    ----------------------

    -- Send a message to all recipients
    procedure Write (Message : in Message_Type;
                     Length : in Message_Length := 0;
                     Send_Cb : in Send_Callback_Access := null) is
      Dest : Dest_Rec;
      Msg : Channel_Message_Type;
      Len : Message_Length;
      Res : Boolean;
      use type Socket.Socket_Dscr;
    begin
      -- Empty list?
      if Dest_List_Mng.Is_Empty (Channel_Dscr.Dests) then
        return;
      end if;

      -- Build message and len
      Msg.Diff := True;
      Msg.Data := Message;
      if Length = 0 then
        Len := Message'Size / Byte_Size + Msg.Diff'Size / Byte_Size;
      else
        Len := Length + Msg.Diff'Size / Byte_Size;
      end if;

      -- Send to all connected destinations
      Dest_List_Mng.Move_To (Channel_Dscr.Dests, Dest_List_Mng.Next, 0, False); 
      loop
        Dest_List_Mng.Read (Channel_Dscr.Dests, Dest, Dest_List_Mng.Current);
        if Dest.Dscr /= Socket.No_Socket then
          begin
            Res := Channel_Send (Dest.Dscr, null, Msg, Len);
            Res := True;
          exception
            when Socket.Soc_Tail_Err =>
              -- Already in overflow
              Res := False;
            when others =>
              -- Other error
              Res := False;
          end;
        else
          Res := False;
        end if;
        if Send_Cb /= null then
          Send_Cb (Parse (Dest.Host_Name.Name), Res);
        end if;
        exit when Dest_List_Mng.Get_Position (Channel_Dscr.Dests)
                = Dest_List_Mng.List_Length (Channel_Dscr.Dests);
        Dest_List_Mng.Move_To (Channel_Dscr.Dests);
      end loop;

    end Write;

    -- Send a message on a dscr
    procedure Send (Dscr    : in Socket.Socket_Dscr;
                    Message : in Message_Type;
                    Length  : in Message_Length) is
      Msg : Channel_Message_Type;
      Len : Message_Length;
      Res : Boolean;
    begin
      -- Build message and len
      Msg.Diff := False;
      Msg.Data := Message;
      if Length = 0 then
        Len := Message'Size / Byte_Size + Msg.Diff'Size / Byte_Size;
      else
        Len := Length + Msg.Diff'Size / Byte_Size;
      end if;

      -- Send on Dscr
      begin
        Res := Channel_Send (Dscr, null, Msg, Len);       
      exception
        when Socket.Soc_Tail_Err =>
          raise Send_Overflow;
        when others =>
          raise Send_Failed;
      end;
    end Send;

    -- Reply to sender of last message received
    procedure Reply (Message : in Message_Type;
                     Length : in Message_Length := 0) is
      Dscr : Socket.Socket_Dscr;
      D_Rec : Dest_Rec;
      S_Rec : Send_Rec;
      use type Socket.Socket_Dscr;
    begin
      -- Get current socket
      if Reply_List_Mng.Is_Empty (Channel_Dscr.Replies) then
        raise Not_In_Read;
      end if;
      Reply_List_Mng.Read (Channel_Dscr.Replies, Dscr, Reply_List_Mng.Current);

      -- Check it is still known (not closed)
      -- More probably in senders, but maybe in dests (if reply of a reply)
      begin
        S_Rec.Dscr := Dscr;
        Dscr_Search (Channel_Dscr.Sends, S_Rec, From_Current => False);
      exception
        when Send_List_Mng.Not_In_List =>
          begin
            D_Rec.Dscr := Dscr;
            Dscr_Search (Channel_Dscr.Dests, D_Rec, From_Current => False);
          exception
            when Dest_List_Mng.Not_In_List =>
              raise Reply_Failed;
          end;
      end;

      -- Reply on current
      begin
        Send (Dscr, Message, Length);
      exception
        when Send_Overflow =>
          raise Reply_Overflow;
        when Send_Failed =>
          raise Reply_Failed;
      end;
    end Reply;

    -- Send a message to one destination
    -- May raise Unknown_Destination if Host_Name is not known
    -- May raise Send_Overflow if message cannot be sent due to overflow
    -- May raise Send_Failed if message cannot be sent due to other error
    procedure Send (Host_Name : in String;
                    Message   : in Message_Type;
                    Length    : in Message_Length := 0) is
      D_Rec : Dest_Rec;
    begin
      -- Find destination from host name
      D_Rec.Host_Name.Name := (others => ' ');
      D_Rec.Host_Name.Name(1 .. Host_Name'Length) := Host_Name; 
      begin
        Host_Name_Search (Channel_Dscr.Dests, D_Rec, From_Current => False);
      exception
        when Dest_List_Mng.Not_In_List =>
          raise Unknown_Destination;
      end;
      Dest_List_Mng.Read (Channel_Dscr.Dests, D_Rec, Dest_List_Mng.Current);      
      Send (D_Rec.Dscr, Message, Length);
    end Send;

  begin -- Channel
    Init;
  end Channel;

  ----------------------------------------------------------------------------

  package Bus_Reply_List_Mng is new Dynamic_List (Socket.Host_Id);
  package body Bus is

    type Bus_Dscr_Rec is record
      Init : Boolean := False;
      Active : Boolean := False;
      Subscribed : Boolean := False;
      Joined : Boolean := False;
      Bus_Name : Text_Handler.Text (Tcp_Util.Max_Port_Name_Len);
      Dest_Name : Text_Handler.Text (Tcp_Util.Max_Host_Name_Len);
      Send_Dscr : Socket.Socket_Dscr;
      Rece_Dscr : Socket.Socket_Dscr;
      Bus_Id : Socket.Host_Id;
      Replies : Bus_Reply_List_Mng.List_Type;
    end record;
    Bus_Dscr : Bus_Dscr_Rec;

    -- Socket instance
    procedure Bus_Read is new Socket.Receive (Bus_Message_Type);
    procedure Bus_Write is new Socket.Send (Bus_Message_Type);

    -- Store instanciation names
    procedure Init is
    begin
      if Bus_Dscr.Init then
        return;
      end if;
      Text_Handler.Set (Bus_Dscr.Bus_Name, Bus_Name);
      Text_Handler.Set (Bus_Dscr.Dest_Name, Destination_Name);
      Bus_Dscr.Init := True;
    exception
      when Constraint_Error =>
        raise Name_Too_Long;
    end Init;

    -- Change bus and Lan names 
    -- May raise Name_Too_Long if a Name is too long
    -- May raise Bus_Active if subscribed or joined
    procedure Change_Names (New_Bus_Name, New_Destination_Name : in String) is
    begin
      if Bus_Dscr.Active then
        raise Bus_Active;
      end if;
      Text_Handler.Set (Bus_Dscr.Bus_Name, New_Bus_Name);
      Text_Handler.Set (Bus_Dscr.Dest_Name, New_Destination_Name);
      Bus_Dscr.Init := True;
    exception
      when Constraint_Error =>
        raise Name_Too_Long;
    end Change_Names;

    function Loc_Read_Cb (Fd : in Event_Mng.File_Desc; Read : in Boolean)
                     return Boolean is 
      Dscr : Socket.Socket_Dscr;
      Msg : Bus_Message_Type;
      Len  : Natural;
      use type Event_Mng.File_Desc;
    begin
      if Fd = Socket.Fd_Of (Bus_Dscr.Rece_Dscr) then
        if Bus_Dscr.Subscribed then
          Dscr := Bus_Dscr.Rece_Dscr;
        else
          Assertion.Assert (False, "Channel.Bus receiving on rece but not subscribed");
        end if;
      elsif Fd = Socket.Fd_Of (Bus_Dscr.Send_Dscr) then
        if Bus_Dscr.Joined then
          Dscr := Bus_Dscr.Send_Dscr;
        else
          Assertion.Assert (False, "Channel.Bus receiving on send but not joined");
        end if;
      else
        Assertion.Assert (False, "Channel.Bus receiving but no bus");
      end if;
       
      begin
        Bus_Read (Dscr, Msg, Len, True);
      exception
        when others =>
          Assertion.Assert (False, "Channel.Bus reading error");
          return False;
      end;
      Bus_Reply_List_Mng.Insert (Bus_Dscr.Replies,
           Socket.Get_Destination_Host (Dscr));
      Read_Cb (Msg.Data, Len - (Msg.Diff'Size / Byte_Size), Msg.Diff);
      Bus_Reply_List_Mng.Delete (Bus_Dscr.Replies, Bus_Reply_List_Mng.Prev);
      return True;
    exception
      when others =>
        Assertion.Assert (False, "Channel.Bus handler error");
        return False;
    end Loc_Read_Cb;

    procedure Set_Dest_Bus (Dscr : in out Socket.Socket_Dscr) is
    begin
      Socket.Set_Destination_Name_And_Service (
         Dscr,
         True,
         Text_Handler.Value (Bus_Dscr.Dest_Name),
         Text_Handler.Value (Bus_Dscr.Bus_Name));
      Bus_Dscr.Bus_Id := Socket.Get_Destination_Host (Dscr);
    exception
      when Socket.Soc_Name_Not_Found =>
        declare
          Num : Socket.Port_Num;
        begin
          Num := Socket.Port_Num_Of (Text_Handler.Value (Bus_Dscr.Bus_Name),
                                     Socket.Udp);
        exception
           when Socket.Soc_Name_Not_Found =>
             raise Unknown_Bus;
        end;
        raise Unknown_Destination;
    end Set_Dest_Bus;
          
    -- Subscription
    -- Allow reception from bus
    -- May raise Already_Subscribed if already subscribed to this bus
    -- May raise Name_Too_Long if Bus or Destination Name is too long
    -- May raise Unknown_Bus if Bus_Name is not known
    -- May raise Unknown_Destination if Destination_Name is not known
    procedure Subscribe is
    begin
      if Bus_Dscr.Subscribed then
        raise Already_Subscribed;
      end if;
      Init;
      Bus_Dscr.Active := True;
      Bus_Dscr.Subscribed := True;
      Socket.Open (Bus_Dscr.Rece_Dscr, Socket.Udp);
      Set_Dest_Bus (Bus_Dscr.Rece_Dscr);
      Socket.Link_Service (Bus_Dscr.Rece_Dscr,
            Text_Handler.Value (Bus_Dscr.Bus_Name));
      Event_Mng.Add_Fd_Callback (Socket.Fd_Of (Bus_Dscr.Rece_Dscr),
                                 True, Loc_Read_Cb'Unrestricted_Access);
    end Subscribe;

    -- Close reception from bus
    -- May raise Not_Subscribed if not subscribed to this channel
    procedure Unsubscribe is
    begin
      if not Bus_Dscr.Subscribed then
        raise Not_Subscribed;
      end if;
      Event_Mng.Del_Fd_Callback (Socket.Fd_Of (Bus_Dscr.Rece_Dscr), True);
      Socket.Close (Bus_Dscr.Rece_Dscr);
      Bus_Dscr.Subscribed := False;
    end Unsubscribe;


    -- Join a bus for publishing
    -- May raise Already_Joined if already joined
    -- May raise Unknown_Bus if Bus_Name is not known
    -- May raise Unknown_Destination if Destination_Name is not known
    procedure Join is
    begin
      if Bus_Dscr.Joined then
        raise Already_Joined;
      end if;
      Init;
      Bus_Dscr.Active := True;
      Bus_Dscr.Joined := True;
      Socket.Open (Bus_Dscr.Send_Dscr, Socket.Udp);
      Set_Dest_Bus (Bus_Dscr.Send_Dscr);
      -- This is for receiving replies
      Socket.Link_Dynamic (Bus_Dscr.Send_Dscr);
      Event_Mng.Add_Fd_Callback (Socket.Fd_Of (Bus_Dscr.Send_Dscr),
                                 True, Loc_Read_Cb'Unrestricted_Access);
    end Join;

    -- Leave a bus
    -- May raise Not_Joined if not joined
    procedure Leave is
    begin
      if not Bus_Dscr.Joined then
        raise Not_Joined;
      end if;
      Event_Mng.Del_Fd_Callback (Socket.Fd_Of (Bus_Dscr.Send_Dscr), True);
      Socket.Close (Bus_Dscr.Send_Dscr);
      Bus_Dscr.Joined := False;
    end Leave;
    

    procedure Send  (Dscr    : in Socket.Socket_Dscr;
                     Diff    : in Boolean;
                     Message : in Message_Type;
                     Length  : in Message_Length) is
      Msg : Bus_Message_Type;
      Len  : Natural;
    begin
      -- Build message and len
      Msg.Diff := Diff;
      Msg.Data := Message;
      if Length = 0 then
        Len := Message'Size / Byte_Size + Msg.Diff'Size / Byte_Size;
      else
        Len := Length + Msg.Diff'Size / Byte_Size;
      end if;
      Bus_Write (Dscr, Msg, Len);
    end Send;

    -- Send a message on the bus
    -- May raise Not_Joined if not joined
    procedure Write (Message : in Message_Type;
                     Length  : in Message_Length := 0) is
    begin
      if not Bus_Dscr.Joined then
        raise Not_Joined;
      end if;
      -- Dest may have been changed by a send or by reading a reply
      Socket.Change_Destination_Host (Bus_Dscr.Send_Dscr, Bus_Dscr.Bus_Id);
      Send (Bus_Dscr.Send_Dscr, True, Message, Length);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Would_Block =>
        null;
    end Write;

    -- Reply to sender of last message received
    -- Should only be called in Read_Cb.
    -- May raise Not_In_Read if not called by Read_Cb
    -- May raise Reply_Failed if reply cannot be sent
    procedure Reply (Message : in Message_Type;
                     Length : in Message_Length := 0) is
      Id : Socket.Host_Id;
    begin
      if not Bus_Dscr.Subscribed then
        raise Not_Subscribed;
      end if;
      -- Get current socket of reception
      if Bus_Reply_List_Mng.Is_Empty (Bus_Dscr.Replies) then
        raise Not_In_Read;
      end if;
      Bus_Reply_List_Mng.Read (Bus_Dscr.Replies, Id,
                               Bus_Reply_List_Mng.Current);
      -- Reply to it
      Socket.Change_Destination_Host (Bus_Dscr.Rece_Dscr, Id);
      Send (Bus_Dscr.Rece_Dscr, False, Message, Length);
    exception
      when others =>
        raise Reply_Failed;
    end Reply;

    -- Send a message to one destination
    -- May raise Unknown_Destination if Host_Name is not known
    -- May raise Send_Failed if message cannot be sent
    procedure Send (Host_Name : in String;
                    Message   : in Message_Type;
                    Length    : in Message_Length := 0) is

    begin
      if not Bus_Dscr.Joined then
        raise Not_Joined;
      end if;

      -- Use send port
      begin
        Socket.Change_Destination_Name (Bus_Dscr.Send_Dscr, False, Host_Name);
      exception
        when Socket.Soc_Name_Not_Found =>
          raise Unknown_Destination;
      end;

      begin
        Send (Bus_Dscr.Send_Dscr, False, Message, Length);
      exception
        when others =>
          raise Send_Failed;
      end;
    end Send;


  begin -- Bus
    Init;
  end Bus;
    
end Channels;

