with System, Ada.Calendar;
with As.U, Environ, Socket, Tcp_Util, Dynamic_List, Event_Mng, Assertion;
pragma Elaborate (Tcp_Util);
package body Channels is

  Byte_Size : constant := System.Storage_Unit;

  package Host_Dyn_List_Mng is new Dynamic_List (Tcp_Util.Remote_Host);
  package Host_List_Mng renames Host_Dyn_List_Mng.Dyn_List;

  -- Destination
  type Dest_Rec is record
    Host_Name : Tcp_Util.Remote_Host (Tcp_Util.Host_Name_Spec);
    Host_Id : Socket.Host_Id;
    Dscr : Socket.Socket_Dscr := Socket.No_Socket;
    Fd : Event_Mng.File_Desc;
  end record;
  package Dest_Dyn_List_Mng is new Dynamic_List(Dest_Rec);
  package Dest_List_Mng renames Dest_Dyn_List_Mng.Dyn_List;

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
  package Send_Dyn_List_Mng is new Dynamic_List(Send_Rec);
  package Send_List_Mng renames Send_Dyn_List_Mng.Dyn_List;

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
  package Reply_Dyn_List_Mng is new Dynamic_List (Socket.Socket_Dscr);
  package Reply_List_Mng renames Reply_Dyn_List_Mng.Dyn_List;

  -- The channel
  type Channel_Rec is record
    Init : Boolean := False;
    Name : Tcp_Util.Host_Name;
    Period : Ada.Calendar.Day_Duration;
    Accept_Num : Tcp_Util.Port_Num := 0;
    Dests : Dest_List_Mng.List_Type;
    Sends : Send_List_Mng.List_Type;
    Replies : Reply_List_Mng.List_Type;
    Active : Boolean;
  end record;

  -- Misc toolkits
  -- Close a (overflow) connection
  procedure Close (Dscr : in out Socket.Socket_Dscr) is
  begin
    Tcp_Util.Abort_Send_And_Close (Dscr);
  exception
    when Tcp_Util.No_Such =>
      Dscr.Close;
  end Close;

  function Get_Period (Channel : String) return Ada.Calendar.Day_Duration is
    Default_Period : constant Ada.Calendar.Day_Duration := 1.0;
  begin
    return Environ.Get_Dur ("Channel_" & Channel & "_period", Default_Period);
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
      Moved : Boolean;
    begin
      Channel_Dscr.Dests.Delete (Moved => Moved);
    end Delete_Current_Dest;

    procedure Delete_Current_Send is
      Moved : Boolean;
    begin
      Channel_Dscr.Sends.Delete (Moved => Moved);
    end Delete_Current_Send;

    procedure Init is
    begin
      -- Save Channel_Name from instantiation
      if Channel_Dscr.Init then
        return;
      end if;
      Channel_Dscr.Name := As.U.Tus (Channel_Name);
      Channel_Dscr.Period := Get_Period (Channel_Name);
      Channel_Dscr.Init := True;
      Channel_Dscr.Active := True;
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
      if not Channel_Dscr.Dests.Is_Empty then
        raise Channel_Active;
      end if;
      -- Store new name
      Channel_Dscr.Name := As.U.Tus (New_Channel_Name);
      Channel_Dscr.Period := Get_Period (New_Channel_Name);
      Channel_Dscr.Init := True;
    exception
      when Constraint_Error =>
        raise Name_Too_Long;
    end Change_Channel_Name;

    -- Connection callback (used in read callback on destination
    --  disconnection
    procedure Connect_Cb (Remote_Host_Id  : in Tcp_Util.Host_Id;
                          Remote_Port_Num : in Tcp_Util.Port_Num;
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
      Found : Boolean;
    begin
      if Sender then
        -- Look for sender. Unhook Fd if not found (bug).
        S_Rec.Fd := Fd;
        Fd_Search (Channel_Dscr.Sends, Found, S_Rec,
                   From => Send_List_Mng.Absolute);
        if not Found then
          Event_Mng.Del_Fd_Callback (Fd, True);
          return False;
        end if;
        Channel_Dscr.Sends.Read (S_Rec, Send_List_Mng.Current);
        Dscr := S_Rec.Dscr;
      else
        -- Look for destination. Unhook Fd if not found (bug).
        D_Rec.Fd := Fd;
        Fd_Search (Channel_Dscr.Dests, Found, D_Rec,
                   From => Dest_List_Mng.Absolute);
        if not Found then
          Event_Mng.Del_Fd_Callback (Fd, True);
          return False;
        end if;
        Channel_Dscr.Dests.Read (D_Rec, Dest_List_Mng.Current);
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
              pragma Unreferenced (Res);
              Port : Tcp_Util.Remote_Port (Tcp_Util.Port_Name_Spec);
            begin
              -- Update record
              D_Rec.Dscr := Socket.No_Socket;
              D_Rec.Fd := 0;
              Channel_Dscr.Dests.Modify (D_Rec, Dest_List_Mng.Current);
              -- Retry to connect
              Port.Name := Channel_Dscr.Name;
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
      -- Append, Call callback
      Channel_Dscr.Replies.Rewind (False, Reply_List_Mng.Prev);
      Channel_Dscr.Replies.Insert (Dscr);
      Read_Cb (Msg.Data, Len - (Msg.Diff'Size / Byte_Size), Msg.Diff);
      Channel_Dscr.Replies.Delete (Reply_List_Mng.Prev);
      return True;
    end Read_Cb;

    function Rec_Read_Cb (Fd : in Event_Mng.File_Desc; Read : in Boolean)
                     return Boolean is
      pragma Unreferenced (Read);
    begin
      return Read_Cb (True, Fd);
    end Rec_Read_Cb;

    procedure Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                         Local_Dscr      : in Socket.Socket_Dscr;
                         Remote_Host_Id  : in Tcp_Util.Host_Id;
                         Remote_Port_Num : in Tcp_Util.Port_Num;
                         New_Dscr        : in Socket.Socket_Dscr) is
      pragma Unreferenced (Local_Port_Num, Local_Dscr,
                           Remote_Port_Num, Remote_Host_Id);
      use type Socket.Port_Num;
    begin
      -- Discard and close if channel is closed
      if Channel_Dscr.Accept_Num = 0 then
        declare
          Tmp_Socket : Socket.Socket_Dscr := New_Dscr;
        begin
          Tmp_Socket.Close;
        exception
          when others => null;
        end;
        return;
      else
        -- Insert new sender
        Channel_Dscr.Sends.Insert ( (Dscr => New_Dscr,
                                     Fd   => New_Dscr.Get_Fd));

        -- Hook fd to receive data
        if Channel_Dscr.Active then
          Event_Mng.Add_Fd_Callback (New_Dscr.Get_Fd, True,
                                Rec_Read_Cb'Unrestricted_Access);
        end if;
        New_Dscr.Set_Blocking (False);
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
      Port.Name := Channel_Dscr.Name;

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
      Tcp_Util.Abort_Accept (Socket.Tcp_Header, Channel_Dscr.Accept_Num);
      Channel_Dscr.Accept_Num := 0;

      -- Close all connections
      if not Channel_Dscr.Sends.Is_Empty then
        -- Rewind and close all connections
        Channel_Dscr.Sends.Rewind;
        loop
          Channel_Dscr.Sends.Read (Rec, Send_List_Mng.Current);
          -- Unhook fd receiving data
          Event_Mng.Del_Fd_Callback (Rec.Dscr.Get_Fd, True);
          Close (Rec.Dscr);
          exit when not Channel_Dscr.Sends.Check_Move;
          Channel_Dscr.Sends.Move_To;
        end loop;
        -- Delete list
        Channel_Dscr.Sends.Delete_List (False);
      end if;
    end Unsubscribe;

    ----------------------

    function Snd_Read_Cb (Fd : in Event_Mng.File_Desc; Read : in Boolean)
                     return Boolean is
      pragma Unreferenced (Read);
    begin
      return Read_Cb (False, Fd);
    end Snd_Read_Cb;

    procedure Connect_Cb (Remote_Host_Id  : in Tcp_Util.Host_Id;
                          Remote_Port_Num : in Tcp_Util.Port_Num;
                          Connected       : in Boolean;
                          Dscr            : in Socket.Socket_Dscr) is
      pragma Unreferenced (Remote_Port_Num, Connected);
      Dest : Dest_Rec;
      Found : Boolean;
    begin
      -- Find record
      Dest.Host_Id := Remote_Host_Id;
      Host_Id_Search (Channel_Dscr.Dests, Found, Dest,
                      From => Dest_List_Mng.Absolute);
      if not Found then
        -- Bug?
        Dest.Dscr.Close;
        return;
      end if;

      -- Update Dscr and Fd
      Channel_Dscr.Dests.Read (Dest, Dest_List_Mng.Current);
      Dest.Dscr := Dscr;
      Dest.Fd := Dscr.Get_Fd;
      Channel_Dscr.Dests.Modify (Dest, Dest_List_Mng.Current);

      -- Hook fd to receive data (replies)
      if Channel_Dscr.Active then
        Event_Mng.Add_Fd_Callback (Dscr.Get_Fd, True,
                              Snd_Read_Cb'Unrestricted_Access);
      end if;
      Dscr.Set_Blocking (False);
    end Connect_Cb;


    procedure Build_Host_Port (Host_Name : in String;
                               Host : out Tcp_Util.Remote_Host;
                               Port : out Tcp_Util.Remote_Port) is
    begin
      -- Build host and port records
      Host := (Kind => Tcp_Util.Host_Name_Spec, Name => As.U.Tus (Host_Name));
      Port := (Kind => Tcp_Util.Port_Name_Spec, Name => Channel_Dscr.Name);
    end Build_Host_Port;

    -- Add destinations from file
    procedure Add_Destinations (File_Name : in String) is
      Host : Tcp_Util.Remote_Host;
      List : Host_List_Mng.List_Type;
    begin
      -- Store hosts (fully parse file)
      File.Open (File_Name, Channel_Dscr.Name.Image);
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
        List.Insert (Host);
      end loop;

      if List.Is_Empty then
        return;
      end if;

      -- Add destinations of list
      List.Rewind;
      loop
        List.Read (Host, Host_List_Mng.Current);
        begin
          Add_Destination (Host.Name.Image);
        exception
          when Destination_Already | Unknown_Destination =>
            null;
        end;
        -- Next host or the end
        if List.Check_Move then
          List.Move_To;
        else
          -- Done
          List.Delete_List (Deallocate => True);
          exit;
        end if;
      end loop;

    exception
      when others =>
        List.Delete_List (Deallocate => True);
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

      Host_Id_Search (Channel_Dscr.Dests, Result, Dest,
                      From => Dest_List_Mng.Absolute);
      if Result then
        raise Destination_Already;
      end if;

      -- Insert host, dscr and fd in dest list
      Dest.Host_Name := Host;
      Dest.Dscr := Socket.No_Socket;
      Dest.Fd := 0;
      Channel_Dscr.Dests.Insert (Dest);

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
            pragma Unreferenced (Id);
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
      Channel_Dscr.Dests.Read (Dest, Dest_List_Mng.Current);
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
      Found : Boolean;
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
      Host_Id_Search (Channel_Dscr.Dests, Found, Dest,
                      From => Dest_List_Mng.Absolute);
      if not Found then
        raise Unknown_Destination;
      end if;

      -- Close (pending) connection
      Close_Current_Connection;

    end Del_Destination;

    -- Delete all recipients
    procedure Del_All_Destinations is
      Dest : Dest_Rec;
    begin
      -- Empty list?
      if Channel_Dscr.Dests.Is_Empty then
        return;
      end if;

      -- Delete all connections
      Channel_Dscr.Dests.Rewind;
      loop
        Channel_Dscr.Dests.Read (Dest, Dest_List_Mng.Current);
        Close_Current_Connection;
        exit when Channel_Dscr.Dests.Is_Empty;
      end loop;

    end Del_All_Destinations;

    ----------------------

    -- Activate or not the reception of messages on the channel
    procedure Activate (Allow_Reception : in Boolean) is
      Send : Send_Rec;
      Dest : Dest_Rec;
    begin
      -- Check if this is a change
      if Allow_Reception = Channel_Dscr.Active then
        return;
      end if;

      -- Subscribed connections
      if not Channel_Dscr.Sends.Is_Empty then
        -- Rewind and close all connections
        Channel_Dscr.Sends.Rewind;
        loop
          Channel_Dscr.Sends.Read (Send, Send_List_Mng.Current);
          -- (Un)hook fd receiving data
          if Allow_Reception then
            Event_Mng.Add_Fd_Callback (Send.Dscr.Get_Fd, True,
                                  Rec_Read_Cb'Unrestricted_Access);
          else
            Event_Mng.Del_Fd_Callback (Send.Dscr.Get_Fd, True);
          end if;
          exit when not Channel_Dscr.Sends.Check_Move;
          Channel_Dscr.Sends.Move_To;
        end loop;
      end if;

      -- Connected connections
      if not Channel_Dscr.Dests.Is_Empty then
        -- Rewind and close all connections
        Channel_Dscr.Dests.Rewind;
        loop
          Channel_Dscr.Dests.Read (Dest, Dest_List_Mng.Current);
          -- (Un)hook fd receiving data
          if Allow_Reception then
            Event_Mng.Add_Fd_Callback (Dest.Dscr.Get_Fd, True,
                                  Snd_Read_Cb'Unrestricted_Access);
          else
            Event_Mng.Del_Fd_Callback (Dest.Dscr.Get_Fd, True);
          end if;
          exit when not Channel_Dscr.Dests.Check_Move;
          Channel_Dscr.Dests.Move_To;
        end loop;
      end if;

    end Activate;

    -- Is reception active?
    function Is_Active return Boolean is
    begin
      return Channel_Dscr.Active;
    end Is_Active;

    -- Send a message to all recipients
    procedure Write (Message : in Message_Type;
                     Length : in Message_Length := 0;
                     Send_Cb : access
      procedure (Host_Name : in String;
                 Send_Ok   : in Boolean) := null)
    is
      Dest : Dest_Rec;
      Msg : Channel_Message_Type;
      Len : Message_Length;
      Res : Boolean;
      use type Socket.Socket_Dscr;
    begin
      -- Empty list?
      if Channel_Dscr.Dests.Is_Empty then
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
      Channel_Dscr.Dests.Rewind;
      loop
        Channel_Dscr.Dests.Read (Dest, Dest_List_Mng.Current);
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
          Send_Cb (Dest.Host_Name.Name.Image, Res);
        end if;
        exit when not Channel_Dscr.Dests.Check_Move;
        Channel_Dscr.Dests.Move_To;
      end loop;

    end Write;

    -- Send a message on a dscr
    procedure Send (Dscr    : in Socket.Socket_Dscr;
                    Message : in Message_Type;
                    Length  : in Message_Length) is
      Msg : Channel_Message_Type;
      Len : Message_Length;
      Res : Boolean;
      pragma Unreferenced (Res);
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
      Found : Boolean;
      use type Socket.Socket_Dscr;
    begin
      -- Get current socket
      if Channel_Dscr.Replies.Is_Empty then
        raise Not_In_Read;
      end if;
      Channel_Dscr.Replies.Read (Dscr, Reply_List_Mng.Current);

      -- Check it is still known (not closed)
      -- More probably in senders, but maybe in dests (if reply of a reply)
      S_Rec.Dscr := Dscr;
      Dscr_Search (Channel_Dscr.Sends, Found, S_Rec,
                   From => Send_List_Mng.Absolute);
      if not Found then
        D_Rec.Dscr := Dscr;
        Dscr_Search (Channel_Dscr.Dests, Found, D_Rec,
                     From => Dest_List_Mng.Absolute);
        if not Found then
          raise Reply_Failed;
        end if;
      end if;

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
      Found : Boolean;
    begin
      -- Find destination from host name
      D_Rec.Host_Name.Name := As.U.Tus (Host_Name);
      Host_Name_Search (Channel_Dscr.Dests, Found, D_Rec,
                        From => Dest_List_Mng.Absolute);
      if not Found then
        raise Unknown_Destination;
      end if;
      Channel_Dscr.Dests.Read (D_Rec, Dest_List_Mng.Current);
      Send (D_Rec.Dscr, Message, Length);
    end Send;

  begin -- Channel
    Init;
  end Channel;

  ----------------------------------------------------------------------------

  package Bus_Reply_Dyn_List_Mng is new Dynamic_List (Socket.Host_Id);
  package Bus_Reply_List_Mng renames Bus_Reply_Dyn_List_Mng.Dyn_List;
  package body Bus is

    type Bus_Dscr_Rec is record
      Init : Boolean := False;
      Active : Boolean := False;
      Subscribed : Boolean := False;
      Joined : Boolean := False;
      Bus_Name : Tcp_Util.Port_Name;
      Dest_Name : Tcp_Util.Host_Name;
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
      Bus_Dscr.Bus_Name := As.U.Tus (Bus_Name);
      Bus_Dscr.Dest_Name := As.U.Tus (Destination_Name);
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
      Bus_Dscr.Bus_Name := As.U.Tus (New_Bus_Name);
      Bus_Dscr.Dest_Name := As.U.Tus (New_Destination_Name);
      Bus_Dscr.Init := True;
    exception
      when Constraint_Error =>
        raise Name_Too_Long;
    end Change_Names;

    function Loc_Read_Cb (Fd : in Event_Mng.File_Desc; Read : in Boolean)
                     return Boolean is
      pragma Unreferenced (Read);
      Dscr : Socket.Socket_Dscr;
      Msg : Bus_Message_Type;
      Len  : Natural;
      use type Event_Mng.File_Desc;
    begin
      if Fd = Bus_Dscr.Rece_Dscr.Get_Fd then
        if Bus_Dscr.Subscribed then
          Dscr := Bus_Dscr.Rece_Dscr;
        else
          Assertion.Assert (False, "Channel.Bus receiving on rece but not subscribed");
        end if;
      elsif Fd = Bus_Dscr.Send_Dscr.Get_Fd then
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
      Bus_Dscr.Replies.Insert (Dscr.Get_Destination_Host);
      Read_Cb (Msg.Data, Len - (Msg.Diff'Size / Byte_Size), Msg.Diff);
      Bus_Dscr.Replies.Delete (Bus_Reply_List_Mng.Prev);
      return True;
    exception
      when others =>
        Assertion.Assert (False, "Channel.Bus handler error");
        return False;
    end Loc_Read_Cb;

    procedure Set_Dest_Bus (Dscr : in out Socket.Socket_Dscr) is
    begin
      Dscr.Set_Destination_Name_And_Service (
         True, Bus_Dscr.Dest_Name.Image, Bus_Dscr.Bus_Name.Image);
      Bus_Dscr.Bus_Id := Dscr.Get_Destination_Host;
    exception
      when Socket.Soc_Name_Not_Found =>
        declare
          Num : Socket.Port_Num;
          pragma Unreferenced (Num);
        begin
          Num := Socket.Port_Num_Of (Bus_Dscr.Bus_Name.Image, Socket.Udp);
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
      Bus_Dscr.Rece_Dscr.Open (Socket.Udp);
      Set_Dest_Bus (Bus_Dscr.Rece_Dscr);
      Bus_Dscr.Rece_Dscr.Link_Service (Bus_Dscr.Bus_Name.Image);
      Event_Mng.Add_Fd_Callback (Bus_Dscr.Rece_Dscr.Get_Fd,
                                 True, Loc_Read_Cb'Unrestricted_Access);
    end Subscribe;

    -- Close reception from bus
    -- May raise Not_Subscribed if not subscribed to this channel
    procedure Unsubscribe is
    begin
      if not Bus_Dscr.Subscribed then
        raise Not_Subscribed;
      end if;
      Event_Mng.Del_Fd_Callback (Bus_Dscr.Rece_Dscr.Get_Fd, True);
      Bus_Dscr.Rece_Dscr.Close;
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
      Bus_Dscr.Send_Dscr.Open (Socket.Udp);
      Set_Dest_Bus (Bus_Dscr.Send_Dscr);
      -- This is for receiving replies
      Bus_Dscr.Send_Dscr.Link_Dynamic;
      Event_Mng.Add_Fd_Callback (Bus_Dscr.Send_Dscr.Get_Fd,
                                 True, Loc_Read_Cb'Unrestricted_Access);
    end Join;

    -- Leave a bus
    -- May raise Not_Joined if not joined
    procedure Leave is
    begin
      if not Bus_Dscr.Joined then
        raise Not_Joined;
      end if;
      Event_Mng.Del_Fd_Callback (Bus_Dscr.Send_Dscr.Get_Fd, True);
      Bus_Dscr.Send_Dscr.Close;
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
      Bus_Dscr.Send_Dscr.Change_Destination_Host (Bus_Dscr.Bus_Id);
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
      if Bus_Dscr.Replies.Is_Empty then
        raise Not_In_Read;
      end if;
      Bus_Dscr.Replies.Read (Id, Bus_Reply_List_Mng.Current);
      -- Reply to it
      Bus_Dscr.Rece_Dscr.Change_Destination_Host (Id);
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
        Bus_Dscr.Send_Dscr.Change_Destination_Name (False, Host_Name);
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

