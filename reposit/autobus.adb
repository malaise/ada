with Ada.Unchecked_Deallocation, Ada.Exceptions;
with Basic_Proc, Environ, Images, Ip_Addr, Socket_Util, Tcp_Util, Event_Mng;
package body Autobus is
  -- Design
  ---------
  -- A Bus is the sharing of an IPM address and port. For each Bus we:
  --  * bind to this IPM address
  --  * bind to a dynamic TCP port
  --  * send periodically an Alive message with local host and the TCP port
  --  * send a Dead message when the Bus is reset
  --  * when discovering a new partner (in an Alive message), the smaller of
  --    local and partner's host address connects to the higher. So if local
  --    host address is the same or larger we connect in TCP to the address
  --    received, otherwise we just send an Alive message and the partner will
  --    connect to us and will send us a TCP service message with its TCP
  --    address.
  --  * keep a list of partners connected in TCP. This list is updated:
  --   - when receiving an Alive (we update the date of death to
  --     current_time + max__to_miss * period)
  --   - when receiving a Dead message (we disconnect and remove partner)
  --   - periodically for detecting silent death of partners (date of death
  --     reached) and removing them.
  --
  -- Note: The remote host id that we get when accepting a partner may not be
  --  the local host that it sees itself (and sends in the Alive message),
  --  for example if it has several LAN inerfaces.
  --  This is why, after connecting we always send a TCP service message with
  --  our local host address, and when accepting we always expect this message.
  --  There is no such problem when connecting to a partner because we
  --  connect to the address that we have received in an Alive message.
  -- Note: This mechanism also applies to ourself, so we always see ourself
  --  twice as a partner (one connected and one accepted). The accepted one is
  --  passive (with no Alive message nor associated timeout and no sending on
  --  it).
  --
  -- Sending a message consists in sending it successively to all the partners
  --  except the passive connection.
  -- Receiving a TCP message (except the first TCP service message on each
  --  accepted connection) consists in dispatching it on all the observer,
  --  which means for each:
  --  * check if the message comes from ourself and if the subscriber wants
  --    to receive the echo,
  --  * check the message content versus the filter if any,
  --  * call the observer if the message passes.
  --
  -- Tuning: The following parameters can be tuned for a Bus:
  -- The period for sending Alive message and of checking sudden death), in
  --  seconds (default 1.0).
  -- The max number of missing Alive messages before considering that
  --  a partner is dead (default 3). So by default a silent death is detected
  --  in 3 seconds.
  -- The timeout in seconds for sending messages (default 0.5).
  -- The TTL for IPM and TCP frames (default 5).

  --------------
  -- INTERNAL --
  --------------
  -- Static data: List of Buses and Partners,
  In_Receive : Boolean := False;

  -- Access to Subscriber_Rec
  type Subscriber_Access is access all Subscriber_Rec;

  -- Ipm message type
  -- 'A' or 'D' then '/' then IPM address then ":" then port num
  --  Ex: "A/123.123.123.123:65535"
  -- Message received shall not exceed 23 chars
  -- We read a message of 24 characters to check that it is shorter than 23
  Ipm_Message_Max_Length : constant := 23 + 1;
  subtype Ipm_Message_Str is String (1 .. Ipm_Message_Max_Length);
  package Ipm_Reception_Mng is new Tcp_Util.Reception (Ipm_Message_Str);

  -- Tcp message type
  Tcp_Message_Max_Length : constant := Message_Max_Length;
  subtype Tcp_Message_Str is String (1 .. Tcp_Message_Max_Length);
  package Tcp_Reception_Mng is new Tcp_Util.Reception (Tcp_Message_Str);

  -- Internal inconsistency
  Internal_Error : exception;

  -- Image of a port num
  function Port_Image is new Images.Int_Image (Socket.Port_Num);

  -- Image of a full address
  function Image (Host : Socket.Host_Id; Port : Socket.Port_Num)
                 return String is
  begin
    return Ip_Addr.Image (Socket.Id2Addr (Host)) & ":" & Port_Image (Port);
  end Image;

  -- Debug
  Debug_Got : Boolean := False;
  Debug_Set : Boolean := False;
  Debug_Str : constant String := "AUTOBUS_DEBUG";
  procedure Debug (Message : in String) is
  begin
    if not Debug_Got then
      Debug_Set := Environ.Is_Yes (Debug_Str);
      Debug_Got := True;
    end if;
    if Debug_Set then
      Basic_Proc.Put_Line_Output ("Autobus: " & Message);
    end if;
  end Debug;

  -- Log an exception
  procedure Log_Exception (Operation, Exception_Name, Message : in String) is
  begin
    Basic_Proc.Put_Error ("Autobus: Exception " & Exception_Name
                        & " raised in " & Operation);
    if Message = "" then
      Basic_Proc.Put_Line_Error (".");
    else
      Basic_Proc.Put_Line_Error (", " & Message & '.');
    end if;
  end Log_Exception;

  -- Log an error
  procedure Log_Error (Operation, Error, Message : in String) is
  begin
    Basic_Proc.Put_Error ("Autobus: Error " & Error
                        & " detected in " & Operation);
    if Message = "" then
      Basic_Proc.Put_Line_Error (".");
    else
      Basic_Proc.Put_Line_Error (", " & Message & '.');
    end if;
  end Log_Error;

  -- Get tuning
  package Config is
    -- Get for the bus the heartbeat period, the heartbeat max missed number
    -- and the timeout of connection and send
    -- May raise Config_Error
    procedure Get_Tuning (Name : in String;
                          Heartbeat_Period : out Duration;
                          Heartbeat_Max_Missed : out Positive;
                          Timeout : out Duration;
                          Ttl : out Socket.Ttl_Range);
  end Config;
  package body Config is separate;

  -- Search Partner
  -- By address
  function Partner_Match_Addr (Curr, Crit : Partner_Rec) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Curr.Addr = Crit.Addr;
  end Partner_Match_Addr;
  -- By socket
  function Partner_Match_Sock (Curr, Crit : Partner_Rec) return Boolean is
    use type Socket.Socket_Dscr;
  begin
    return Curr.Sock = Crit.Sock;
  end Partner_Match_Sock;
  -- By Host and port
  function Partner_Match_Hp (Curr, Crit : Partner_Rec) return Boolean is
    use type Socket.Host_Id, Socket.Port_Num;
  begin
    return Curr.Host = Crit.Host and then Curr.Port = Crit.Port;
  end Partner_Match_Hp;
  -- By access
  function Partner_Match_Acc (Curr, Crit : Partner_Access) return Boolean is
  begin
    return Curr = Crit;
  end Partner_Match_Acc;

  -- Search Bus
  -- By admin socket
  function Bus_Match_Admin (Curr, Crit : Bus_Rec) return Boolean is
    use type Socket.Socket_Dscr;
  begin
    return Curr.Admin = Crit.Admin;
  end Bus_Match_Admin;
  -- By accept socket
  function Bus_Match_Accep (Curr, Crit : Bus_Rec) return Boolean is
    use type Socket.Socket_Dscr;
  begin
    return Curr.Accep = Crit.Accep;
  end Bus_Match_Accep;
  -- By timer Id
  function Bus_Match_Timer (Curr, Crit : Bus_Rec) return Boolean is
    use type Timers.Timer_Id;
  begin
    return Curr.Timer = Crit.Timer;
  end Bus_Match_Timer;

  -- Raise Reset_In_Receive if In_Receive
  procedure Check_In_Receive is
  begin
    if In_Receive then
      raise Reset_In_Receive;
    end if;
  end Check_In_Receive;

  -- Remove current (in Partners list) Partner
  -- Remove its ref in its Bus list and remove it from Partners list
  -- Move to next in both lists
  procedure Deallocate is new Ada.Unchecked_Deallocation (
           Chronos.Passive_Timers.Passive_Timer, Timer_Access);
  procedure Remove_Current_Partner (Close : in Boolean) is
    Partner_Acc : Partner_Access;
    Partner_Found : Boolean;
    Moved : Boolean;
  begin
    -- Find partner ref in Bus
    Partner_Acc := Partner_Access (Partners.Access_Current);
    Debug ("Removing partner " & Partner_Acc.Addr.Image);
    Partner_Acc.Bus.Partners.Search_Match (
           Partner_Found, Partner_Match_Acc'Access, Partner_Acc,
           From => Partner_Access_List_Mng.Absolute);
    if not Partner_Found then
      Log_Error ("Remove_Current_Partner", "not found", "in buses list");
      return;
    end if;

    -- Delete this partner from bus list
    Partner_Acc.Bus.Partners.Delete (Moved => Moved);

    -- Close this partner resources
    Deallocate (Partner_Acc.Timer);
    if Partner_Acc.Sock.Is_Open then
      -- This partner is connected
      if Close then
        Debug ("Removing partner -> Closing socket");
        Tcp_Reception_Mng.Remove_Callbacks (Partner_Acc.Sock);
        Partner_Acc.Sock.Close;
      end if;
    else
      -- There is a pending connection attempt to this partner
      Debug ("Removing partner -> Aborting connection");
      begin
        -- Connection was requested with host and port Ids
        Tcp_Util.Abort_Connect (
           Host => (Kind => Tcp_Util.Host_Id_Spec, Id => Partner_Acc.Host),
           Port => (Kind => Tcp_Util.Port_Num_Spec, Num => Partner_Acc.Port));
      exception
        when Tcp_Util.No_Such =>
          Log_Error ("Remove_Current_Partner", "no such",
                     "while aborting pending tcp connection");
          return;
      end;
    end if;

    -- Delete this partner from Partners
    Partners.Search_Access (Partner_Found, Partner_Acc);
    Partners.Delete (Moved => Moved);

  end Remove_Current_Partner;

  -- Insert Partner in current Bus
  procedure Insert_Partner (Partner : in Partner_Rec) is
    Partner_Acc : Partner_Access;
  begin
    -- Insert in Partners list
    Partners.Rewind (False, Partner_List_Mng.Next);
    Partners.Insert (Partner);
    Partner_Acc := Partner_Access(Partners.Access_Current);
    -- Insert a partner access in the list of partners of the bus
    Partner_Acc.Bus.Partners.Rewind (False, Partner_Access_List_Mng.Next);
    Partner_Acc.Bus.Partners.Insert (Partner_Acc);
  end Insert_Partner;

  -- Start passive timer on current partner
  procedure Start_Partner_Timer (Bus : Bus_Access) is
    Partner_Acc : Partner_Access;
    Timeout : Timers.Delay_Rec (Timers.Delay_Sec);
  begin
    Partner_Acc := Partner_Access(Partners.Access_Current);
    Timeout.Delay_Seconds := Bus.Heartbeat_Max_Missed * Bus.Heartbeat_Period;
    Partner_Acc.Timer.Start (Timeout);
  end Start_Partner_Timer;

  -- Remove current (in current Bus list) subscriber
  --  and move to next
  procedure Deallocate is new Ada.Unchecked_Deallocation (
            Regular_Expressions.Compiled_Pattern, Filter_Access);
  procedure Remove_Current_Subscriber is
    Subscriber_Acc : Subscriber_Access;
    Subscriber_Found : Boolean;
    Moved : Boolean;
  begin
    -- Get access and move to current
    Subscriber_Acc := Subscriber_Access(
                  Buses.Access_Current.Subscribers.Access_Current);
    Buses.Access_Current.Subscribers.Search_Access (Subscriber_Found,
                                                    Subscriber_Acc);
    if not Subscriber_Found then
      Log_Error ("Remove_Current_Subscriber", " subscriber not found",
                 "in bus list");
      return;
    end if;

    -- Clear client reference
    Subscriber_Acc.Client.Acc := null;
    -- Free Filter and delete from Bus list
    if Subscriber_Acc.Filter /= null then
      Regular_Expressions.Free (Subscriber_Acc.Filter.all);
      Deallocate (Subscriber_Acc.Filter);
    end if;
    Buses.Access_Current.Subscribers.Delete (Moved => Moved);
  end Remove_Current_Subscriber;

  procedure Tcp_Disconnection_Cb (Dscr : in Socket.Socket_Dscr) is
    Partner : Partner_Rec;
    Partner_Found : Boolean;
  begin
    -- Find partner by socket
    Partner.Sock := Dscr;
    Partners.Search_Match (Partner_Found, Partner_Match_Sock'Access, Partner,
                           From => Partner_List_Mng.Absolute);
    if not Partner_Found then
      Log_Error ("Tcp_Disconnection_Cb", " partner not found",
                 "in partners list");
      return;
    end if;
    -- Don't close socket (Tcp_Util will do it when we return)
    Debug ("Disconnection of partner " & Partners.Access_Current.Addr.Image);
    Remove_Current_Partner (False);
  end Tcp_Disconnection_Cb;

  -- Remove partners that are not alive (alive timeout expired)
  --  or remove ALL partners. Of current Bus.
  procedure Remove_Partners (Remove_All : in Boolean) is
    Bus_Acc : Bus_Access;
    Partner_Acc : Partner_Access;
    Remove : Boolean;
    Partner_Found : Boolean;
    Moved : Boolean;
  begin
    Bus_Acc := Bus_Access(Buses.Access_Current);
    Bus_Acc.Partners.Rewind (False, Partner_Access_List_Mng.Next);
    loop
      exit when Bus_Acc.Partners.Is_Empty;
      Bus_Acc.Partners.Read (Partner_Acc, Moved => Moved);
      Remove := False;
      if Remove_All then
        Remove := True;
      else
        if Partner_Acc.Timer.Exists and then Partner_Acc.Timer.Has_Expired then
          -- This partner is not alive (alive timeout has expired)
          Debug ("Alive timeout of partner " & Partner_Acc.Addr.Image);
          Remove := True;
        end if;
      end if;
      if Remove then
        Partners.Search_Access (Partner_Found, Partner_Acc);
        if not Partner_Found  then
          Log_Error ("Remove_Partners", "partner not found",
                     "in partners list");
        else
          Remove_Current_Partner (True);
        end if;
      end if;
      exit when not Moved;
    end loop;
  end Remove_Partners;

  -- Dispatch a message to the Subscribers of current bus
  procedure Dispatch (Message : in String; Local : in Boolean);

  -- TCP Reception Cb
  function Tcp_Reception_Cb (Dscr    : Socket.Socket_Dscr;
                             Message : Tcp_Message_Str;
                             Length  : Natural) return Boolean is
    Msg : constant String := Message (1 .. Length);
    Partner : Partner_Rec;
    Partner_Found : Boolean;
    Partner_Acc : Partner_Access;
    Bus_Found : Boolean;
    use type As.U.Asu_Us;
  begin
    -- Find partner by Socket
    Partner.Sock := Dscr;
    Partners.Search_Match (Partner_Found, Partner_Match_Sock'Access, Partner,
                           From => Partner_List_Mng.Absolute);
    if not Partner_Found then
      Log_Error ("Tcp_Reception_Cb", " partner not found",
                 "in partners list");
      return False;
    end if;
    Partner_Acc := Partner_Access(Partners.Access_Current);

    -- Find Bus by access
    Buses.Search_Access (Bus_Found, Partner_Acc.Bus);

    if not Partner_Acc.Addr.Is_Null then
      -- Not the first message, so this is Data => dispatch
      Dispatch (Message (1 .. Length),
                Partner_Acc.Addr = Buses.Access_Current.Addr);
      return True;
    end if;

    -- The partner (just connected to us) sends us its accept address
    declare
      Rem_Host : Tcp_Util.Remote_Host;
      Rem_Port : Tcp_Util.Remote_Port;
    begin
      Ip_Addr.Parse (Msg, Rem_Host, Rem_Port);
    exception
      when Ip_Addr.Parse_Error =>
        Log_Error ("Tcp_Reception_Cb", "invalid identification",
                 Msg & " from " & Partner_Acc.Addr.Image);
        Remove_Current_Partner (True);
        return False;
    end;
    Partner_Acc.Addr := As.U.Tus (Msg);
    if Partner_Acc.Addr = Buses.Access_Current.Addr then
      Debug ("Reception of own identification");
      -- A stopped timer identifies the unique connection to ourself
      --  (amoung both) that is passive: no alive timeout check and
      --  no sending of message
      Partner_Acc.Timer.Stop;
    else
      Debug ("Reception of identification from " & Partner_Acc.Addr.Image);
    end if;
    return False;
  end Tcp_Reception_Cb;

  -- Pending connection Cb
  function Tcp_Send is new Tcp_Util.Send (Tcp_Message_Str);
  procedure Tcp_Connection_Cb (Remote_Host_Id  : in Tcp_Util.Host_Id;
                               Remote_Port_Num : in Tcp_Util.Port_Num;
                               Connected       : in Boolean;
                               Dscr            : in Socket.Socket_Dscr) is
    Partner : Partner_Rec;
    Partner_Found : Boolean;
    Partner_Acc : Partner_Access;
    Message : Tcp_Message_Str;
    Message_Length : Natural;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    if not Connected then
      -- This should not occur because the number of connection retries
      --  is infinite
      Log_Error ("Tcp_Connection_Cb", "failure",
                 "shall not occur");
      return;
    end if;

    -- Find partner by Host and Port
    Partner.Host := Remote_Host_Id;
    Partner.Port := Remote_Port_Num;
    -- Find partner by address
    Partners.Search_Match (Partner_Found, Partner_Match_Hp'Access, Partner,
                           From => Partner_List_Mng.Absolute);
    if not Partner_Found then
      Log_Error ("Tcp_Connection_Cb", " partner not found",
                 "in partners list");
      return;
    end if;
    Partner_Acc := Partner_Access(Partners.Access_Current);
    Debug ("Connection to partner " & Partner_Acc.Addr.Image);

    -- Set TTL and send identification message
    Partner_Acc.Sock := Dscr;
    Partner_Acc.Sock.Set_Ttl (Partner_Acc.Bus.Ttl);
    Message_Length := Partner_Acc.Bus.Addr.Length;
    Message(1 .. Message_Length) := Partner_Acc.Bus.Addr.Image;
    Dummy := Tcp_Send (Partner_Acc.Sock, null, null, Partner_Acc.Bus.Timeout,
                       Message, Message_Length);

    -- Hook callbacks
    Tcp_Reception_Mng.Set_Callbacks (Dscr,
                                     Tcp_Reception_Cb'Access,
                                     Tcp_Disconnection_Cb'Access);
    --  Create and start its timer
    Start_Partner_Timer (Partner_Acc.Bus);
  exception
    when Socket.Soc_Conn_Lost =>
      -- Not working
      Debug ("Sending ident to partner raises Conn_Lost, closing");
      Partner_Acc.Sock.Close;
    when Tcp_Util.Timeout_Error =>
      -- Not working
      Debug ("Sending ident to partner raises Timeout_Error, closing");
      Partner_Acc.Sock.Close;
  end Tcp_Connection_Cb;

  -- Accept connection Cb
  procedure Tcp_Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                           Local_Dscr      : in Socket.Socket_Dscr;
                           Remote_Host_Id  : in Tcp_Util.Host_Id;
                           Remote_Port_Num : in Tcp_Util.Port_Num;
                           New_Dscr        : in Socket.Socket_Dscr) is
    pragma Unreferenced (Local_Port_Num);
    Bus : Bus_Rec;
    Bus_Found : Boolean;
    Partner : Partner_Rec;
  begin
    -- Find bus and insert partner
    Bus.Accep := Local_Dscr;
    Buses.Search_Match (Bus_Found, Bus_Match_Accep'Access, Bus,
                        From => Bus_List_Mng.Absolute);
    if not Bus_Found then
      Log_Error ("Accept_Cb", "bus not found", "in buses list");
      return;
    end if;
    -- The Addr remains empty until the partner sends it on the connection
    --  (first message received on the conection)
    Partner.Host := Remote_Host_Id;
    Partner.Port := Remote_Port_Num;
    Partner.Sock := New_Dscr;
    Partner.Timer := new Chronos.Passive_Timers.Passive_Timer;
    Partner.Bus := Bus_Access(Buses.Access_Current);
    -- Insert in Bus a reference to this partner
    Insert_Partner (Partner);
    Debug ("Acception of partner " & Partner.Addr.Image);
    Start_Partner_Timer (Partner.Bus);
    -- Set reception callback
    New_Dscr.Set_Ttl (Partner.Bus.Ttl);
    Tcp_Reception_Mng.Set_Callbacks (New_Dscr,
                                     Tcp_Reception_Cb'Access,
                                     Tcp_Disconnection_Cb'Access);

  end Tcp_Accept_Cb;

  -- Send Alive message of current Bus
  procedure Ipm_Send is new Socket.Send (Ipm_Message_Str,
                                         Ipm_Message_Max_Length - 1);
  procedure Send_Ipm (Alive : in Boolean) is
    Message_Len : Natural;
    Message : Ipm_Message_Str;
    Bus_Acc : Bus_Access;
  begin
    Bus_Acc := Bus_Access(Buses.Access_Current);
    Message_Len := Bus_Acc.Addr.Length + 2;
    if Alive then
      Message(1 .. Message_Len) := "A/" & Bus_Acc.Addr.Image;
    else
      Message(1 .. Message_Len) := "D/" & Bus_Acc.Addr.Image;
    end if;
    Ipm_Send (Bus_Acc.Admin, Message, Message_Len);
  end Send_Ipm;

  -- IPM Reception Cb
  function Ipm_Reception_Cb (Dscr    : Socket.Socket_Dscr;
                             Message : Ipm_Message_Str;
                             Length  : Natural) return Boolean is
    Address : constant String := Message (3 .. Length);
    Rem_Host : Tcp_Util.Remote_Host;
    Rem_Port : Tcp_Util.Remote_Port;
    Partner_Found : Boolean;
    Connected : Boolean;
    pragma Unreferenced (Connected);
    -- Partner is filled progressively (excep its Sock and Timer)
    Partner : Partner_Rec;
    use type Tcp_Util.Remote_Host_List, Tcp_Util.Remote_Port_List;
    use type Socket.Host_Id;
    use type As.U.Asu_Us;
  begin
    -- Check validity of string, drop if KO
    -- Mini is A/1.1.1.1:1
    if Length < 11 or else Length > Ipm_Message_Max_Length - 1
    or else (Message(1) /= 'A' and then Message(1) /= 'D')
    or else Message(2) /= '/' then
      return False;
    end if;
    begin
      -- Check address IP part
      Ip_Addr.Parse (Address, Rem_Host, Rem_Port);
      if Rem_Host.Kind = Tcp_Util.Host_Name_Spec
      or else Rem_Port.Kind = Tcp_Util.Port_Name_Spec then
        -- Not an IP address or not a port num
        return False;
      end if;
      -- Set partner address
      Partner.Addr := As.U.Tus (Address);
      Partner.Host := Rem_Host.Id;
      Partner.Port := Rem_Port.Num;
    exception
      when others => return False;
    end;

    -- Find bus by admin socket
    declare
      Bus_Found : Boolean;
      Crit : Bus_Rec;
    begin
      Crit.Admin := Dscr;
      Buses.Search_Match (Bus_Found, Bus_Match_Admin'Access, Crit,
                          From => Bus_List_Mng.Absolute);
      if not Bus_Found then
        Log_Error ("Ipm_Reception_Cb", "bus not found", "in buses list");
        return False;
      end if;
      -- Set partner bus
      Partner.Bus := Bus_Access(Buses.Access_Current);
    end;

    -- Find partner by address
    Partners.Search_Match (Partner_Found, Partner_Match_Addr'Access, Partner,
                           From => Partner_List_Mng.Absolute);

    -- Handle Death
    if Message(1) = 'D' then
      if Partner_Found then
        -- Death of a known partner, remove it and close connection
        Debug ("Ipm: Death of partner " & Partner.Addr.Image);
        Remove_Current_Partner (True);
      end if;
      -- End of processing of a death message
      return False;
    end if;

    -- Handle Alive
    if not Partner_Found then
      -- New (unknown yet) partner
      if Partner.Addr < Partner.Bus.Addr then
        -- Addr < own: we send an live, then the partner will connect to us
        --  (and we will accept) then it will send its address
        Debug ("Ipm: Waiting for identification of " & Partner.Addr.Image);
        Send_Ipm (True);
        return False;
      end if;
      -- Addr => own: add partner and start connect
      Debug ("Ipm: Connecting to new partner " & Partner.Addr.Image);
      Partner.Timer := new Chronos.Passive_Timers.Passive_Timer;
      Insert_Partner (Partner);
      -- The callback can be called synchronously
      Connected := Tcp_Util.Connect_To (
          Socket.Tcp_Header,
          Rem_Host, Rem_Port,
          Tcp_Connection_Cb'Access,
          Partner.Bus.Timeout, 0,
          Partner.Bus.Ttl);
    else
      -- This partner is known, restart its keep alive timer
      Start_Partner_Timer (Partner.Bus);
    end if;
    return False;
  end Ipm_Reception_Cb;

  -- Timer Cb
  function Timer_Cb (Id : in Timers.Timer_Id;
                     Data : in Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Data);
    Bus_Found : Boolean;
    Bus : Bus_Rec;
  begin
    -- Find Bus
    Bus.Timer := Id;
    Buses.Search_Match (Bus_Found, Bus_Match_Timer'Access, Bus,
                        From => Bus_List_Mng.Absolute);
    if not Bus_Found then
      Log_Error ("Timer_Cb", "bus not found", "in buses list");
      return False;
    end if;

    -- Send Alive message
    Send_Ipm (True);

    -- Check partners keep alive timers and remove dead ones
    Remove_Partners (False);
    return False;
  end Timer_Cb;

  ------------
  -- PUBLIC --
  ------------

  -- Initialise a Bus
  procedure Init (Bus : in out Bus_Type;
                  Address : in String) is
    Rbus : Bus_Rec;
    Port_Num : Socket.Port_Num;
    Timeout : Timers.Delay_Rec (Timers.Delay_Sec);
  begin
    -- Check that Bus is not already initialised
    if Bus.Acc /= null then
      raise Status_Error;
    end if;

    -- Create socket, Parse and check address, Link
    declare
      Rem_Host : Tcp_Util.Remote_Host;
      Rem_Port : Tcp_Util.Remote_Port;
    begin
      Rbus.Admin.Open (Socket.Udp);
      Ip_Addr.Parse (Address, Rem_Host, Rem_Port);
      Socket_Util.Set_Destination (Rbus.Admin, Lan => True,
                                   Host => Rem_Host, Port => Rem_Port);
      Socket_Util.Link (Rbus.Admin, Rem_Port);
      -- Name is "<ip_address>:<port>"
      Rbus.Name := As.U.Tus (Image (Rbus.Admin.Get_Destination_Host,
                                    Rbus.Admin.Get_Destination_Port));
    exception
      when Ip_Addr.Parse_Error =>
        raise Invalid_Address;
      when Socket.Soc_Name_Not_Found =>
        raise Name_Error;
      when Except:others =>
        Log_Exception ("Bus.Init", Ada.Exceptions.Exception_Name (Except),
                       "IPM socket creation");
        raise System_Error;
    end;

    -- Get tuning for this bus
    Config.Get_Tuning (Rbus.Name.Image,
                       Rbus.Heartbeat_Period,
                       Rbus.Heartbeat_Max_Missed,
                       Rbus.Timeout,
                       Rbus.Ttl);

    -- Set admin callback
    Ipm_Reception_Mng.Set_Callbacks (Rbus.Admin, Ipm_Reception_Cb'Access, null);

    -- Create Accep Tcp socket, set accep callback
    Tcp_Util.Accept_From (Socket.Tcp_Header,
                          (Kind => Tcp_Util.Port_Dynamic_Spec),
                          Tcp_Accept_Cb'Access,
                          Rbus.Accep, Port_Num);
    Rbus.Addr := As.U.Tus (Image (Socket.Local_Host_Id , Port_Num));

    -- Set TTL on Admin and Accept sockets
    Rbus.Admin.Set_Ttl (Rbus.Ttl);
    Rbus.Accep.Set_Ttl (Rbus.Ttl);

    -- Arm Bus active timer
    Timeout.Delay_Seconds := 0.0;
    Timeout.Period := Rbus.Heartbeat_Period;
    Rbus.Timer.Create (Timeout, Timer_Cb'Access);

    -- Done: Insert in list, return access
    Buses.Rewind (False, Bus_List_Mng.Next);
    Buses.Insert (Rbus);
    Bus.Acc := Buses.Access_Current;
    Debug ("Bus " & Rbus.Name.Image & " created at " & Rbus.Addr.Image);
    Debug (" with Period: " & Images.Dur_Image (Rbus.Heartbeat_Period, 1, False)
       & ", MaxMissed: " & Images.Integer_Image(Rbus.Heartbeat_Max_Missed)
       & " and Timeout: " &  Images.Dur_Image (Rbus.Timeout, 1, False));

    -- Wait a little bit (100ms) for "immediate" connections to establish
    Event_Mng.Pause (100);

  end Init;

  -- Is a Bus initialised
  function Is_Init (Bus : Bus_Type) return Boolean is
  begin
    return Bus.Acc /= null;
  end Is_Init;

  -- Reset a Bus (make it re-usable)
  procedure Reset (Bus : in out Bus_Type) is
    Bus_Found : Boolean;
    Moved : Boolean;
    use type Socket.Socket_Dscr;
  begin
    Check_In_Receive;
    -- Check that Bus is initialised
    if Bus.Acc = null then
      raise Status_Error;
    end if;
    Buses.Search_Access (Bus_Found, Bus.Acc);
    Debug ("Bus.Reset " & Bus.Acc.Name.Image);

    -- Send Death info
    Send_Ipm (False);

    -- Close resources
    Ipm_Reception_Mng.Remove_Callbacks (Bus.Acc.Admin);
    Tcp_Util.Abort_Accept (Socket.Tcp_Header, Bus.Acc.Accep.Get_Linked_To);

    -- Remove all partners
    Remove_Partners (True);

    -- Remove all subscribers
    Bus.Acc.Subscribers.Rewind (False, Subscriber_List_Mng.Next);
    loop
      exit when Bus.Acc.Subscribers.Is_Empty;
      Remove_Current_Subscriber;
    end loop;

    -- Delete current Bus from list
    Buses.Delete (Moved => Moved);
    Bus.Acc := null;
  end Reset;

  -- Send a Message on a Bus
  procedure Send (Bus : in out Bus_Type; Message : in String) is
    Msg : Tcp_Message_Str;
    Partner_Acc : Partner_Access;
    Moved : Boolean;
    Partner_Found : Boolean;
    Success : Boolean;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    -- Check that Bus is initialised
    if Bus.Acc = null then
      raise Status_Error;
    end if;

    if Bus.Acc.Partners.Is_Empty then
      -- No partner
      return;
    end if;

    -- Prepare message and list of parners
    if Message'Length > Msg'Length then
      raise Message_Too_Long;
    end if;
    Msg(1 .. Message'Length) := Message;
    Bus.Acc.Partners.Rewind (False, Partner_Access_List_Mng.Next);

    -- Send message on each partner except on the passive connection to ourself
    --  (so we send it to ourself only once).
    loop
      exit when Bus.Acc.Partners.Is_Empty;
      Bus.Acc.Partners.Read (Partner_Acc, Moved => Moved);
      begin
        if Partner_Acc.Timer.Exists then
          Dummy := Tcp_Send (Partner_Acc.Sock, null, null,
                             Bus.Acc.Timeout, Msg, Message'Length);
        end if;
        Success := True;
      exception
        when Socket.Soc_Conn_Lost =>
          Debug ("Lost connection to " & Partner_Acc.Addr.Image);
          Success := False;
        when Tcp_Util.Timeout_Error =>
          Debug ("Timeout while sending to " & Partner_Acc.Addr.Image);
          Success := False;
        when Except:others =>
          Log_Exception ("Tcp_Send", Ada.Exceptions.Exception_Name (Except),
                         "sending to " & Partner_Acc.Addr.Image);
          Success := False;
      end;
      if not Success then
        -- Remove partner with which error occurs
        Partners.Search_Access (Partner_Found, Partner_Acc);
        Remove_Current_Partner (True);
      end if;
      exit when not Moved;
    end loop;
  end Send;

  -------------------
  -- The Subsciber --
  -------------------
  -- type Subscriber_Type is tagged private;

  -- The Observer is notified with the Messages (sent on the Bus)
  --  that pass the Filter
  -- Filter is a PCRE regular expression
  -- Empty filter lets all messages pass through
  -- type Observer_Type is limited interface;

  -- procedure Bus_Receive (Observer : in out Observer_Type;
  --                        Message : in String) is abstract;

  -- Initialise a Subscriber on a Bus, may raise
  -- On incorrect filter expression
  -- Invalid_Filter : exception;

  procedure Init (Subscriber : in out Subscriber_Type;
                  Bus : in Bus_Access_Type;
                  Observer : access Observer_Type'Class;
                  Filter : in String := "";
                  Echo : in Boolean := False) is
    Bus_Found : Boolean;
    Subs : Subscriber_Rec;
    Ok : Boolean;
    Position : Natural;
  begin
    -- Check that this Bus is initialised
    if Bus = null then
      raise Status_Error;
    end if;
    Buses.Search_Access (Bus_Found, Bus.Acc);
    if not Bus_Found then
      raise Status_Error;
    end if;
    if Observer = null then
       raise Status_Error;
    end if;
    if Subscriber.Acc /= null then
      raise Status_Error;
    end if;
    Subs.Bus := Bus_Access(Bus.Acc);
    Subs.Observer := Observer_Access(Observer);
    Subs.Client := Subscriber'Unrestricted_Access;
    Subs.Echo := Echo;

    -- Compile Filter
    if Filter /= "" then
      Subs.Filter := new Regular_Expressions.Compiled_Pattern;
      Regular_Expressions.Compile (Subs.Filter.all, Ok, Filter);
      if not Ok then
        Debug ("Subscriber.Init regexp error "
             & Regular_Expressions.Error (Subs.Filter.all));
        Regular_Expressions.Free (Subs.Filter.all);
        Deallocate (Subs.Filter);
        raise Invalid_Filter;
      end if;
    end if;

    -- Store in Bus, save position in case we are dispatching
    if Bus.Acc.Subscribers.Is_Empty then
      Position := 0;
    else
      Position := Bus.Acc.Subscribers.Get_Position;
    end if;
    Bus.Acc.Subscribers.Rewind (False, Subscriber_List_Mng.Next);
    Bus.Acc.Subscribers.Insert (Subs);
    Subscriber.Acc := Bus.Acc.Subscribers.Access_Current;
    if Position /= 0 then
      Bus.Acc.Subscribers.Move_At (Position);
    end if;

    if Filter = "" then
      Debug ("Subscriber init ok");
    else
      Debug ("Subscriber " & Filter & " init ok");
    end if;
  end Init;

  -- Is a Subscriber initialised
  function Is_Init (Subscriber : Subscriber_Type) return Boolean is
  begin
    return Subscriber.Acc /= null;
  end Is_Init;

  -- Reset a Subscriber (make it re-usable)
  procedure Reset (Subscriber : in out Subscriber_Type) is
    Bus_Found : Boolean;
    Subscriber_Found : Boolean;
  begin
    Check_In_Receive;
    if Subscriber.Acc = null then
      raise Status_Error;
    end if;
    if Subscriber.Acc.Bus = null then
      raise Status_Error;
    end if;
    -- Find bus
    Buses.Search_Access (Bus_Found, Subscriber.Acc.Bus);
    if not Bus_Found then
      raise Status_Error;
    end if;
    -- Find subscriber in bus list
    Subscriber.Acc.Bus.Subscribers.Search_Access (Subscriber_Found,
                                                  Subscriber.Acc);
    if not Subscriber_Found then
      Debug ("Subscriber.Reset subscriber unknown by its bus!");
      raise Status_Error;
    end if;

    Debug ("Subscriber.Reset " & Subscriber.Acc.Bus.Name.Image);
    Remove_Current_Subscriber;
    Subscriber.Acc := null;
  end Reset;

  -- Dispatch the message to the subscribers of current bus
  procedure Dispatch (Message : in String; Local : in Boolean) is
    Bus : Bus_Access;
    Subs : Subscriber_Access;
    Match_Info : Regular_Expressions.Match_Array (1 .. 1);
    N_Match : Natural;
    Ok : Boolean;
  begin
    Bus := Bus_Access(Buses.Access_Current);

    -- Notify matching Subscribers
    if Bus.Subscribers.Is_Empty then
      return;
    end if;
    Bus.Subscribers.Rewind;
    loop
      Subs := Subscriber_Access(Bus.Subscribers.Access_Current);
      Ok := False;
      if not Local or else Subs.Echo then
        -- This a remote message or this observer wants local messages
        if Subs.Filter = null then
          Ok := True;
        else
          -- See if message matches this Filter
          Regular_Expressions.Exec (Subs.Filter.all, Message,
                                    N_Match, Match_Info);
          Ok := N_Match = 1
             and then Regular_Expressions.Valid_Match (Match_Info(1))
             and then Match_Info(1).First_Offset = Message'First
             and then  Match_Info(1).Last_Offset_Stop = Message'Last;
        end if;
      end if;
      if Ok then
        begin
          In_Receive := True;
          Subs.Observer.Receive (Subs.Client, Message);
          In_Receive := False;
        exception
          when others =>
            In_Receive := False;
        end;
      end if;
      exit when not Bus.Subscribers.Check_Move;
      Bus.Subscribers.Move_To;
    end loop;

  end Dispatch;

  ---------------
  -- INTERNAL --
  ---------------
  -- Internal: Copy a Bus_Rec, for dynamic list of Buses
  -- This is used only to store the bus in the Buses at creation
  -- Other accesses are done by Access_Current
  procedure Set (To : out Bus_Rec; Val : in Bus_Rec) is
  begin
    -- Copy all fields except the lists (of Partners and Subscribers)
    To.Name := Val.Name;
    To.Addr := Val.Addr;
    To.Admin := Val.Admin;
    To.Accep := Val.Accep;
    To.Timer := Val.Timer;
    -- Lists must be empty because this is Bus creation
    if not Val.Partners.Is_Empty or else not Val.Subscribers.Is_Empty then
      raise Internal_Error;
    end if;
  end Set;

  -- Internal: Finalizations
  overriding procedure Finalize (Bus : in out Bus_Type) is
  begin
    if Debug_Set then
      Basic_Proc.Put_Line_Output ("Autobus: finalizing Bus");
    end if;
    if Bus.Acc /= null then
      Reset (Bus);
    end if;
    if Debug_Set then
      Basic_Proc.Put_Line_Output ("Autobus: Bus finalized");
    end if;
  end Finalize;

  overriding procedure Finalize (Subscriber : in out Subscriber_Type) is
  begin
    if Debug_Set then
      Basic_Proc.Put_Line_Output ("Autobus: finalizing Subscriber");
    end if;
    if Subscriber.Acc /= null then
      Reset (Subscriber);
    end if;
    if Debug_Set then
      Basic_Proc.Put_Line_Output ("Autobus: Subscriber finalized");
    end if;
  end Finalize;

end Autobus;

