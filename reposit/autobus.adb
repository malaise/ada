with Ada.Unchecked_Deallocation, Ada.Exceptions;
with Environ, Images, Ip_Addr, Socket_Util, Tcp_Util, Event_Mng, Trace.Loggers,
     Long_Longs, As.U.Utils, Str_Util.Regex, Mod_Utils, Sys_Calls,
     Normal, Mixed_Str;
package body Autobus is
  -- Design
  ---------
  -- A Reliable Bus is the sharing of an IPM address and port. For each Bus we:
  --  * bind to this IPM address (use the config to set a specific interface)
  --  * bind to a dynamic TCP port (use the config to set a specific interface)
  --  * send periodically an Alive message with local host (the interface IP
  --    address) and the TCP port
  --  * send a Dead message when the Bus is reset
  --  * when discovering a new partner (in an Alive message), the smaller of
  --    local and partner's host address connects to the higher (according to
  --    a specific policy described below). So if local host address is the
  --    same or larger, we connect in TCP to the address received, otherwise we
  --    just send an Alive message and the partner will connect to us
  --  * when connected to a partner, send him a first TCP (service) message
  --    with our TCP address
  --  * keep a list of partners connected in TCP. This list is updated:
  --   - when receiving an Alive message we update the date of death (to
  --     current_time + max__to_miss * period) of active partners
  --   - when receiving a Dead message (we disconnect and remove partner)
  --   - periodically for detecting silent death of active partners (date of
  --     death reached) and removing them
  --   - when the sending of a message to it fails on timeout.
  --   - when a partner disconnects from us
  --
  -- Note: The remote host id that we get when accepting a partner may not be
  --  the local host that it sees itself (and sends in the Alive message),
  --  for example if it has several LAN interfaces.
  --  This is why, just after getting connected, we always send a TCP service
  --  message with our local host address, and when accepting we always expect
  --  this message (the partner address is not set until this reception).
  --  There is no such problem when connecting to a partner because we
  --  connect to the address that we have received in an Alive message.
  -- Note: This mechanism also applies to ourself, so we always see ourself
  --  twice as a partner (one connected and one accepted). Once this connection
  --  is established these partners not not have have an active timer.
  --  The connected partner remains active (to send message to ourself) while
  --  the accepted partner becomes passive (to receive messages from ourself).
  --
  -- A partner represents a connection. It can be:
  --  - Init: Either when a connection is accepted and we wait until the
  --     the connecting partner sends its address, or when connecting until the
  --     connection is accepted.
  --  - Active: Connection established to/from a remote partner, or to ourself
  --  - Passive: Connection from ourslef accepted and first message received
  -- Init connections always have a timer (in order to give up on timeout).
  -- Active connections to remote also have a timer (in order to detect death)
  -- Active and passive connections to ourself have no timer.
  --
  -- The connection sequence is:
  --  - Receive a Alive message from an unknown partner
  --    * If its address is lower than ours (according to the policy), then send
  --      an Alive message and he will connect to us.
  --    * Else add partner as Init and connect to it (asynchronously)
  --  - Accept a connection request. Add partner as Init (and wait for his
  --    first message
  --  - Receive first message on an accepted connection, set partner Active
  --    (except if it is ourself, set it passive).
  --  - Receive connection success. Set partner Active
  --  When setting a partner Active or passive, disable timer if this partner is
  --   ourself
  --
  -- Sending a message consists in sending it successively to all the active
  --  partners (so not on the passive connection from ourself).
  -- In Multicast, prepend the image of our pid
  -- Receiving a TCP message (except the first TCP service message on each
  --  accepted connection) consists in dispatching it on all the observer,
  --  which means for each:
  --  * check if the message comes from ourself and if the subscriber wants
  --    to receive the echo,
  --  * check the message content versus the filter if any,
  --  * call the observer if the message passes.
  --
  -- Policy for sorting addressses:
  -- The goal is to avoid that some nodes always connect and some others always
  --  accept, which could lead to exhaust some resources
  -- The idea is to represent addresses by numbers: pad with zeros the 4 bytes
  --  of the IP address on 3 digits and the port num on 5 digits, max value
  --  M=25525525525565535 can be stored in a Long_Long
  -- Compute the modulus of the address by the number of slices (fixed to 10)
  -- Compare the result of local and received address:
  --  - If modulus are equal then compare the addresses
  --  - Otherwise compare the modulus Ma and Mb. Ma < Mb if:
  --    * if Ma < Mb and then Mb - Ma < M / 2
  --    * if Ma > Mb and then Ma - Mb > M / 2
  --
  -- Multicast and compatibility
  -- If Multicast bus, create a Ptp UDP socket on a dynamic port instead
  --  of the acceptance socket
  -- When receiving a Reliable live message, create a connection to the sender.
  -- When sending, send in multicast and on the connections


  --------------
  -- INTERNAL --
  --------------
  -- Static data:
  -- True while calling Receive of the application, in order to
  --  ensure that Reply is called from Receive
  --  and prevent the application from callin us from Receive
  Calling_Receive : Boolean := False;
  -- True while calling Sup_Cb of the application, in order to
  --  and prevent the application from callin us from Sup_Cb
  Calling_Supcb   : Boolean := False;

  -- Own pid
  -- Each multicast message is made of
  -- M/PID/DATA
  Max_Pid_Image : constant String := Images.Integer_Image (Integer'Last);
  Pid_Image : String (1 .. Max_Pid_Image'Last) := (others => ' ');

  -- Access to Subscriber_Rec
  type Subscriber_Access is access all Subscriber_Rec;

  -- Ipm live message type
  -- 'A' or 'P' or 'D' then '/' then TCP address then ":" then port num
  --  Ex: "A/345.789.123.567:90123"
  --  These messages shall not exceed 23 chars
  --  We read a message of 24 characters to check that it is shorter than 23
  Ipm_Message_Max_Length : constant := 23;
  subtype Ipm_Message_Str is String (1 .. Ipm_Message_Max_Length + 1);
  subtype Message_Str is String (1 .. Message_Max_Length);
  package Ipm_Reception_Mng is new Tcp_Util.Reception (Message_Str);
  -- Mini is A/3.5.7.9:1
  Message_Min_Length : constant := 11;

  -- Tcp message type
  Tcp_Message_Max_Length : constant := Message_Max_Length;
  subtype Tcp_Message_Str is String (1 .. Tcp_Message_Max_Length);
  package Tcp_Reception_Mng is new Tcp_Util.Reception (Tcp_Message_Str);

  -- Internal inconsistency
  Internal_Error : exception;

  -- Image of a full address
  function Image (Host : Socket.Host_Id; Port : Socket.Port_Num)
                 return String is
  begin
    return Ip_Addr.Image (Host, Port);
  end Image;

  -- Traces
  Logger : Trace.Loggers.Logger;
  Finalizations : constant Trace.Severities := 16#20#;
  All_Timers    : constant Trace.Severities := 16#40#;

  -- Log an exception
  procedure Log_Exception (Operation, Exception_Name, Message : in String) is
  begin

    Logger.Log_Error ("Exception " & Exception_Name
                    & " raised in " & Operation
                    & (if Message /= "" then ", " & Message else "") & ".");
  end Log_Exception;

  -- Log an error
  procedure Log_Error (Operation, Error, Message : in String) is
  begin
    Logger.Log_Error ("Error " & Error & " in " & Operation
                    & (if Message /= "" then ", " & Message else "") & ".");
  end Log_Error;

  -- Get tuning
  package Config is
    -- Get for the bus the heartbeat period, the heartbeat max missed number,
    -- the timeout of connection and send
    -- May raise Config_Error
    procedure Get_Tuning (Name : in String;
                          Heartbeat_Period : out Duration;
                          Heartbeat_Max_Missed : out Positive;
                          Timeout : out Duration;
                          Ttl : out Socket.Ttl_Range;
                          Passive_Factor : out Positive);
    -- Return the Host_Id denoting the interface to use
    function Get_Interface (Name : String) return Socket.Host_Id;
  end Config;
  package body Config is separate;

  -- Search Partner
  -- By socket
  function Partner_Match_Sock (Curr, Crit : Partner_Rec) return Boolean is
    use type Socket.Socket_Dscr;
  begin
    return Curr.Sock = Crit.Sock;
  end Partner_Match_Sock;
  -- By host, port and mode
  function Partner_Match_Hpm (Curr, Crit : Partner_Rec) return Boolean is
    use type Socket.Host_Id, Socket.Port_Num;
  begin
    return Curr.Host = Crit.Host and then Curr.Port = Crit.Port
           and then Curr.Mode = Crit.Mode;
  end Partner_Match_Hpm;
  -- By access
  function Partner_Match_Acc (Curr, Crit : Partner_Access) return Boolean is
  begin
    return Curr = Crit;
  end Partner_Match_Acc;

  -- Search Bus
  -- By admin socket
  function Bus_Match_Adm (Curr, Crit : Bus_Rec) return Boolean is
    use type Socket.Socket_Dscr;
  begin
    return Curr.Adm = Crit.Adm;
  end Bus_Match_Adm;
  -- By accept socket
  function Bus_Match_Acc (Curr, Crit : Bus_Rec) return Boolean is
    use type Socket.Socket_Dscr;
  begin
    return Curr.Acc = Crit.Acc;
  end Bus_Match_Acc;
  -- By timer Id
  function Bus_Match_Timer (Curr, Crit : Bus_Rec) return Boolean is
    use type Timers.Timer_Id;
  begin
    return Curr.Heartbeat_Timer = Crit.Heartbeat_Timer;
  end Bus_Match_Timer;
  -- By Address name
  function Bus_Match_Name (Curr, Crit : Bus_Rec) return Boolean is
    use type As.U.Asu_Us;
   begin
    return Curr.Name = Crit.Name;
  end Bus_Match_Name;

  -- Raise In_Receive if Calling_Receive
  procedure Check_In_Callback is
  begin
    if Calling_Receive or else Calling_Supcb then
      raise In_Callback;
    end if;
  end Check_In_Callback;

  -- Sort partner addresses
  -- Numeric representation of an address 255.255.255.255:6535
  subtype Addr_Num is Long_Longs.Llu_Natural range 0 .. 25525525525556535;
  -- Number of slices of modulus, must be > 2
  Slices : constant := 10;
  -- Convert an address into num
  function Addr_Num_Of (A : As.U.Asu_Us) return Addr_Num is
    Strs : As.U.Utils.Asu_Array (1 .. 5);
    Str : As.U.Asu_Us;
    Pad : constant String (1 .. 5) := (others => '0');
  begin
    Strs := Str_Util.Regex.Split_Sep (A.Image, "[:.]");
    -- Pad each byte of the address on 3 digits
    for I in 1 .. 4 loop
      Strs(I).Prepend (Pad(1 .. 3 - Strs(I).Length));
      Str.Append (Strs(I));
    end loop;
    -- Pad the port on 5 digits
    Strs(5).Prepend (Pad(1 .. 5 - Strs(5).Length));
    Str.Append (Strs(5));
    return Addr_Num'Value (Str.Image);
  exception
    when Constraint_Error =>
      Log_Error ("Address Num Of", "invalid address", A.Image);
      raise Invalid_Address;
  end Addr_Num_Of;

  -- Is Val < Crit
  function Smaller (Val, Crit : As.U.Asu_Us) return Boolean is
    Nv, Nc : Addr_Num;
    Mv, Mc : Natural;
    Sl : constant Addr_Num := Addr_Num (Slices);
    use type As.U.Asu_Us, Addr_Num;
  begin
    -- Convert to num
    Nv := Addr_Num_Of (Val);
    Nc := Addr_Num_Of (Crit);
    -- Compute modulus
    Mv := Natural (Nv mod Sl);
    Mc := Natural (Nc mod Sl);
    if Mv = Mc then
      -- Compare addresses if same modulus or not enough slices
      return Nv < Nc;
    else
      -- Compare modulus
      return Mod_Utils.Smaller (Mv, Mc, Slices);
    end if;
  end Smaller;

  -- Notify sup callback on current Bus of a partner or local address
  procedure Notify_Sup (Bus : in Bus_Access;
                        Address : in As.U.Asu_Us;
                        State   : in Trilean.Trilean) is
  begin
    if Bus.Sup_Cb = null then
      return;
    end if;
    Calling_Supcb := True;
    begin
      Bus.Sup_Cb ( (Address, State) );
    exception
      when Error:others =>
        Logger.Log_Debug ("Sup callback raised exception: "
                        & Ada.Exceptions.Exception_Name (Error));
    end;
    Calling_Supcb := False;
  end Notify_Sup;

  -- Remove current (in Partners list) Partner
  -- Remove its ref in its Bus list and remove it from Partners list
  -- Move to next in both lists
  procedure Deallocate is new Ada.Unchecked_Deallocation (
           Chronos.Passive_Timers.Passive_Timer, Timer_Access);
  procedure Remove_Current_Partner (Close : in Boolean) is
    Partner_Acc : Partner_Access;
    Moved : Boolean;
  begin
    -- Find partner ref in Bus
    Partner_Acc := Partner_Access (Partners.Access_Current);
    Logger.Log_Debug ("Removing partner " & Partner_Acc.Addr.Image);
    if not Partner_Acc.Bus.Partners.Search_Match (
             Partner_Match_Acc'Access, Partner_Acc,
             From => Partner_Access_List_Mng.Current_Absolute) then
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
        Logger.Log_Debug ("Removing partner -> Closing socket");
        Tcp_Reception_Mng.Remove_Callbacks (Partner_Acc.Sock);
        Partner_Acc.Sock.Close;
      end if;
    else
      -- There is a pending connection attempt to this partner
      Logger.Log_Debug ("Removing partner -> Aborting connection");
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
    declare
      Dummy_Partner_Found : Boolean;
    begin
      Dummy_Partner_Found := Partners.Search_Access (Partner_Acc);
    end;
    Partners.Delete (Moved => Moved);

  end Remove_Current_Partner;

  -- Start passive timer on current partner
  procedure Start_Partner_Timer (Bus : Bus_Access) is
    Partner_Acc : Partner_Access;
    Timeout : Timers.Delay_Rec (Timers.Delay_Sec);
  begin
    Partner_Acc := Partner_Access(Partners.Access_Current);
    Logger.Log (All_Timers, "Starting timer for " & Partner_Acc.Addr.Image);
    Timeout.Delay_Seconds := Bus.Heartbeat_Max_Missed * Bus.Heartbeat_Period;
    Partner_Acc.Timer.Start (Timeout);
  end Start_Partner_Timer;

  -- Insert Partner in current Bus
  procedure Insert_Partner (Partner : in out Partner_Rec) is
    Partner_Acc : Partner_Access;
  begin
    Partner.State := Init;
    -- Insert in Partners list
    Partners.Rewind (Partner_List_Mng.Next, False);
    Partners.Insert (Partner);
    Partner_Acc := Partner_Access(Partners.Access_Current);
    -- Insert a partner access in the list of partners of the bus
    Partner_Acc.Bus.Partners.Rewind (Partner_Access_List_Mng.Next, False);
    Partner_Acc.Bus.Partners.Insert (Partner_Acc);
    -- Start Timer on this partner
    Start_Partner_Timer (Partner_Acc.Bus);
  end Insert_Partner;

  -- Remove current (in current Bus list) subscriber
  --  and move to next
  procedure Deallocate is new Ada.Unchecked_Deallocation (
            Regular_Expressions.Compiled_Pattern, Filter_Access);
  procedure Remove_Current_Subscriber is
    Subscriber_Acc : Subscriber_Access;
    Moved : Boolean;
  begin
    -- Get access and move to current
    Subscriber_Acc := Subscriber_Access(
                  Buses.Access_Current.Subscribers.Access_Current);
    if not Buses.Access_Current.Subscribers.Search_Access (Subscriber_Acc) then
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
    Partner_Acc : Partner_Access;
  begin
    -- Find partner by socket
    Partner.Sock := Dscr;
    if not Partners.Search_Match (Partner_Match_Sock'Access, Partner,
                          From => Partner_List_Mng.Current_Absolute) then
      Log_Error ("Tcp_Disconnection_Cb", " partner not found",
                 "in partners list");
      return;
    end if;
    -- Don't close socket (Tcp_Util will do it when we return)
    Partner_Acc := Partners.Access_Current;
    Logger.Log_Debug ("Disconnection of partner "
                    & Partner_Acc.Addr.Image);
    Notify_Sup (Partner_Acc.Bus, Partner_Acc.Addr, Trilean.False);
    -- remove partner without closing (socket is already closed)
    Remove_Current_Partner (False);
  end Tcp_Disconnection_Cb;

  -- Remove active partners that are not alive (alive timeout expired)
  --  or remove ALL partners. Of current Bus.
  procedure Remove_Partners (Remove_All : in Boolean) is
    Bus_Acc : Bus_Access;
    Partner_Acc : Partner_Access;
    Remove : Boolean;
    Moved : Boolean;
  begin
    Bus_Acc := Bus_Access(Buses.Access_Current);
    Bus_Acc.Partners.Rewind (Partner_Access_List_Mng.Next, False);
    loop
      exit when Bus_Acc.Partners.Is_Empty;
      Bus_Acc.Partners.Read (Partner_Acc, Moved => Moved);
      Remove := False;
      if Remove_All then
        Remove := True;
      else
        if Partner_Acc.State = Active and then Partner_Acc.Timer.Running
        and then Partner_Acc.Timer.Has_Expired then
          -- This partner is not alive (alive timeout has expired)
          Logger.Log_Debug ("Alive timeout of partner "
                          & Partner_Acc.Addr.Image);
          Logger.Log (All_Timers, "Expiration of  timer for "
                                & Partner_Acc.Addr.Image);
          Remove := True;
        end if;
      end if;
      if Remove then
        if not Partners.Search_Access (Partner_Acc) then
          Log_Error ("Remove_Partners", "partner not found",
                     "in partners list");
        else
          Remove_Current_Partner (True);
        end if;
      end if;
      exit when not Moved;
    end loop;
  end Remove_Partners;

  -- Dispatch a message to the Subscribers of the given bus
  procedure Dispatch (Message : in String;
                      Bus     : in Bus_Access;
                      Local   : in Boolean);

  -- TCP Reception Cb
  function Tcp_Reception_Cb (Dscr    : Socket.Socket_Dscr;
                             Message : Tcp_Message_Str;
                             Length  : Natural) return Boolean is
    Msg : constant String := Message (1 .. Length);
    Addr : As.U.Asu_Us;
    Partner : Partner_Rec;
    Partner_Acc : Partner_Access;
    use type As.U.Asu_Us;
  begin
    -- Find partner by Socket
    Partner.Sock := Dscr;
    if not Partners.Search_Match (Partner_Match_Sock'Access, Partner,
                          From => Partner_List_Mng.Current_Absolute) then
      Log_Error ("Tcp_Reception_Cb", " partner not found",
                 "in partners list");
      return False;
    end if;
    Partner_Acc := Partners.Access_Current;

    if Partner_Acc.State /= Init then
      -- Not the first message, so this is Data => dispatch
      -- Message is local if same Bus address and partner is not multicast
      Dispatch (Message (1 .. Length),
                Partner_Acc.Bus,
                Partner_Acc.Addr = Partner_Acc.Bus.Addr
                and then Partner_Acc.State /= Multicast);
      return True;
    end if;

    -- The partner (that has just connected to us) sends us its unique address
    -- <kind>/<ipaddr>:<port>
    if Length < Message_Min_Length or else Length > Ipm_Message_Max_Length
    or else (Msg(1) /= 'A' and then Msg(1) /= 'P' and then Msg(1) /= 'M')
    or else Msg(2) /= '/' then
      Logger.Log_Warning (
          "Tcp_Reception_Cb received invalid identification header >"
        & Msg & "< from " & Partner_Acc.Addr.Image);
    end if;

    -- Check validity of received address
    Addr := As.U.Tus (Msg(3 .. Length));
    declare
      Rem_Host : Tcp_Util.Remote_Host;
      Rem_Port : Tcp_Util.Remote_Port;
      use type Tcp_Util.Remote_Host_List, Tcp_Util.Remote_Port_List;
    begin
      Ip_Addr.Parse (Addr.Image, Rem_Host, Rem_Port);
      -- Must be Ids
      if Rem_Host.Kind /= Tcp_Util.Host_Id_Spec
      or else Rem_Port.Kind /= Tcp_Util.Port_Num_Spec then
        raise Ip_Addr.Parse_Error;
      end if;
      -- Ok, store it
      Partner_Acc.Host := Rem_Host.Id;
      Partner_Acc.Port := Rem_Port.Num;
      Partner_Acc.Mode := (if Msg(1) = 'M' then Multicast else Reliable);
      Partner_Acc.Addr := Addr & Mode_Suffix (Partner_Acc.Mode);
    exception
      when Ip_Addr.Parse_Error =>
        Logger.Log_Warning (
            "Tcp_Reception_Cb received invalid identification >"
          & Msg & "< from " & Partner_Acc.Addr.Image);
        Remove_Current_Partner (True);
        return False;
      when Ip_Addr.Name_Error =>
        Logger.Log_Warning (
            "Tcp_Reception_Cb received unknown identification >"
          & Msg & "< from " & Partner_Acc.Addr.Image);
        Remove_Current_Partner (True);
        return False;
    end;

    -- Detect own address
    if Partner_Acc.Addr = Partner_Acc.Bus.Addr then
      Logger.Log_Debug ("Reception of own identification");
      Logger.Log (All_Timers, "Stopping timer for " & Partner_Acc.Addr.Image);
      -- Stop timer on the connection from ourself and make it passive
      --  (no sending of message)
      Partner_Acc.Timer.Stop;
      Partner_Acc.State := Shadow;
    else
      Logger.Log_Debug ("Reception of identification from "
                      & Partner_Acc.Addr.Image);
      Partner_Acc.State := (if Msg(1) = 'A' then Active
                            elsif Msg(1) = 'P' then Passive
                            else Multicast);
      if Partner_Acc.State /= Active
      or else Partner_Acc.Bus.Kind = Multicast then
        -- Reliable partner of Multicast is managed as passive
        Partner_Acc.State := Passive;
        Partner_Acc.Timer.Stop;
        Logger.Log (All_Timers, "Stopping timer for " & Partner_Acc.Addr.Image);
      end if;
      if Partner_Acc.State /= Multicast then
        Notify_Sup (Partner_Acc.Bus, Partner_Acc.Addr, Trilean.True);
      end if;
    end if;
    Logger.Log_Debug ("Partner is " & Mixed_Str (Partner_Acc.State'Img));
    return False;
  end Tcp_Reception_Cb;

  -- Pending connection Cb
  function Tcp_Send is new Tcp_Util.Send (Tcp_Message_Str);
  procedure Tcp_Connection_Cb (Remote_Host_Id  : in Tcp_Util.Host_Id;
                               Remote_Port_Num : in Tcp_Util.Port_Num;
                               Connected       : in Boolean;
                               Dscr            : in Socket.Socket_Dscr) is
    Partner : Partner_Rec;
    Partner_Acc : Partner_Access;
    Message : Tcp_Message_Str;
    Message_Length : Natural;
    Dummy : Boolean;
    use type As.U.Asu_Us;
  begin
    if not Connected then
      -- This should not occur because the number of connection retries
      --  is infinite (in the worst case the connection is cancelled on alive
      --  timeout)
      Log_Error ("Tcp_Connection_Cb", "failure", "shall not occur");
      return;
    end if;

    -- Find partner by Host and Port
    Partner.Host := Remote_Host_Id;
    Partner.Port := Remote_Port_Num;
    Partner.Mode := Reliable;
    -- Find partner by address
    if not Partners.Search_Match (Partner_Match_Hpm'Access, Partner,
                          From => Partner_List_Mng.Current_Absolute) then
      Log_Error ("Tcp_Connection_Cb", " partner not found",
                 "in partners list");
      return;
    end if;
    Partner_Acc := Partners.Access_Current;
    Logger.Log_Debug ("Connection to partner " & Partner_Acc.Addr.Image);

    -- Hook callbacks
    Tcp_Reception_Mng.Set_Callbacks (Dscr,
                                     Tcp_Reception_Cb'Access,
                                     Tcp_Disconnection_Cb'Access);

    -- Set TTL and send identification message: our TCP address
    Partner_Acc.Sock := Dscr;
    Partner_Acc.Sock.Set_Ttl (Partner_Acc.Bus.Ttl);
    Message_Length := Partner_Acc.Bus.Ipaddr.Length + 2;
    Message(1 .. Message_Length) :=
        (case Partner_Acc.Bus.Kind is
          when Active => 'A',
          when Passive => 'P',
          when Multicast => 'M') & "/" & Partner_Acc.Bus.Ipaddr.Image;
    Logger.Log_Debug ("Sending our address " & Message(1 .. Message_Length));
    Dummy := Tcp_Send (Partner_Acc.Sock, null, null, Partner_Acc.Bus.Timeout,
                       Message, Message_Length);

    -- Update partner state and timer
    Partner_Acc.State := Partner_Acc.Init_State;
    if Partner_Acc.Addr = Partner_Acc.Bus.Addr then
      -- This is the connection to ourself. Active with no timer
      Logger.Log_Debug ("Stopping timer to ourself");
      Logger.Log (All_Timers, "Stopping timer for " & Partner_Acc.Addr.Image);
      Partner_Acc.State := (if Partner_Acc.Bus.Kind = Active then Active
                            else Passive);
      Partner_Acc.Timer.Stop;
    elsif Partner_Acc.Bus.Kind = Multicast then
      -- Reliable partner of Multicast is managed as passive
      Logger.Log (All_Timers, "Stopping timer for " & Partner_Acc.Addr.Image);
      Partner_Acc.State :=  Passive;
      Partner_Acc.Timer.Stop;
    end if;
    Logger.Log_Debug ("Partner is " & Mixed_Str (Partner_Acc.State'Img));
    Notify_Sup (Partner_Acc.Bus, Partner_Acc.Addr,
                (if Partner_Acc.Timer.Running then Trilean.True
                 else Trilean.Other));
  exception
    when Socket.Soc_Conn_Lost =>
      -- Not working
      Logger.Log_Debug ("Sending ident to partner raises Conn_Lost, closing");
      Partner_Acc.Sock.Close;
    when Tcp_Util.Timeout_Error =>
      -- Not working
      Logger.Log_Debug ("Sending ident to partner raises Timeout_Error,"
                      & " closing");
      Partner_Acc.Sock.Close;
  end Tcp_Connection_Cb;

  -- Accept connection Cb
  procedure Tcp_Accept_Cb (Unused_Port_Num : in Tcp_Util.Port_Num;
                           Local_Dscr      : in Socket.Socket_Dscr;
                           Remote_Host_Id  : in Tcp_Util.Host_Id;
                           Remote_Port_Num : in Tcp_Util.Port_Num;
                           New_Dscr        : in Socket.Socket_Dscr) is
    Bus : Bus_Rec;
    Partner : Partner_Rec;
  begin
    -- Find bus and insert partner
    Bus.Acc := Local_Dscr;
    if not Buses.Search_Match (Bus_Match_Acc'Access, Bus,
                               From => Bus_List_Mng.Current_Absolute) then
      Log_Error ("Accept_Cb", "bus not found", "in buses list");
      return;
    end if;
    -- The Host/Port/mode are the one of the connection and the Addr remains
    --  empty until the partner sends them on the connection
    --  (first message received on the connection)
    Partner.Host := Remote_Host_Id;
    Partner.Port := Remote_Port_Num;
    Partner.Mode := Reliable;
    Partner.Sock := New_Dscr;
    Partner.Timer := new Chronos.Passive_Timers.Passive_Timer;
    Partner.Bus := Bus_Access(Buses.Access_Current);
    -- Insert in Bus a reference to this partner
    Logger.Log_Debug ("Acception of partner");
    Insert_Partner (Partner);
    -- Set reception callback
    New_Dscr.Set_Ttl (Partner.Bus.Ttl);
    Tcp_Reception_Mng.Set_Callbacks (New_Dscr,
                                     Tcp_Reception_Cb'Access,
                                     Tcp_Disconnection_Cb'Access);

  end Tcp_Accept_Cb;

  -- Send Alive message of current Bus
  procedure Ipm_Send is new Socket.Send (String,
                                         Message_Max_Length);
  procedure Send_Adm (Active : in Trilean.Trilean) is
    Addr : As.U.Asu_Us;
    Message : Ipm_Message_Str;
    use type As.U.Asu_Us;
  begin
    Addr := Buses.Access_Current.Ipaddr;
    -- Append paddr to prefix 'A', 'P' or 'D'
    Addr.Prepend ( (case Active is
                      when Trilean.True  => "A",
                      when Trilean.False => "P",
                      when Trilean.Other => "D") & "/");
    Message(1 .. Addr.Length) := Addr.Image;
    Ipm_Send (Buses.Access_Current.Adm, Message, Addr.Length);
  end Send_Adm;

  -- IPM Reception Cb
  function Ipm_Reception_Cb (Dscr    : Socket.Socket_Dscr;
                             Message : Message_Str;
                             Length  : Natural) return Boolean is
    Rem_Host : Tcp_Util.Remote_Host;
    Rem_Port : Tcp_Util.Remote_Port;
    Partner_Found : Boolean;
    Partner_Ipaddr : As.U.Asu_Us;
    Partner_Acc : Partner_Access;
    Unused_Connected : Boolean;
    -- Partner is filled progressively (excep its Sock and Timer)
    Partner : Partner_Rec;
    use type Tcp_Util.Remote_Host_List, Tcp_Util.Remote_Port_List;
    use type Socket.Host_Id, Socket.Port_Num;
  begin
    -- Find bus by admin socket
    declare
      Crit : Bus_Rec;
    begin
      Crit.Adm := Dscr;
      if not Buses.Search_Match (Bus_Match_Adm'Access, Crit,
                                 From => Bus_List_Mng.Current_Absolute) then
        Crit.Acc := Dscr;
        if not Buses.Search_Match (Bus_Match_Acc'Access, Crit,
                                   From => Bus_List_Mng.Current_Absolute) then
          Log_Error ("Ipm_Reception_Cb", "bus not found", "in buses list");
          return False;
        elsif Buses.Access_Current.Kind /= Multicast then
          Log_Error ("Ipm_Reception_Cb", "bus not found by admin",
                     "in buses list");
          return False;
        end if;
      end if;
      -- Set partner bus
      Partner.Bus := Bus_Access(Buses.Access_Current);
    end;

    -- Handle Multicast bus data message
    if Length > 0 and then Message(1) = 'M' then
      if Buses.Access_Current.Kind = Multicast then
        -- M/Pid/Data
        if Length < Pid_Image'Length + 3 then
          Logger.Log_Warning (
              "Ipm_Reception_Cb received multicast message too short >"
            & Message(1 .. Length) & "<");
          return False;
        end if;
        if Message (2) /= '/'
        or else Message (Pid_Image'Length + 3) /= '/' then
          Logger.Log_Warning (
              "Ipm_Reception_Cb received invalid multicast message >"
            & Message(1 .. Length) & "<");
          return False;
        end if;

        -- Message is local if the sender is ourself
        -- The sender host is the dest after receive (Set_For_Reply=True)
        -- Check Pid if same host
        Dispatch (Message (Pid_Image'Length + 4 .. Length),
            Partner.Bus,
            Local => Socket.Get_Destination_Host (Dscr) = Partner.Bus.Host_If
            and then Socket.Get_Destination_Port (Dscr) = Partner.Bus.Port
            and then Message(3 .. Pid_Image'Length + 2) = Pid_Image);
        return False;
      else
        -- Reliable bus discards multicast message
        return False;
      end if;
    end if;

    -- Message is an alive message from a reliable bus
    -- Check validity of string, drop if KO
    if Length < Message_Min_Length or else Length > Ipm_Message_Max_Length
    or else (Message(1) /= 'A' and then Message(1) /= 'P'
             and then Message(1) /= 'D')
    or else Message(2) /= '/' then
      Logger.Log_Warning ("Ipm_Reception_Cb received invalid IPM message: >"
                        & Message(1 .. Length) & "<");
      return False;
    end if;
    declare
      Address : constant String := Message (3 .. Length);
    begin
      -- Check address IP part
      Ip_Addr.Parse (Address, Rem_Host, Rem_Port);
      if Rem_Host.Kind = Tcp_Util.Host_Name_Spec
      or else Rem_Port.Kind = Tcp_Util.Port_Name_Spec then
        -- Not an IP address or not a port num
        -- Consider this as an error because the frame started all right
        Log_Error ("Ipm_Reception_Cb", "invalid IPM address", Address);
        return False;
      end if;
      -- Set partner address
      Partner_Ipaddr := As.U.Tus (Address);
      Partner.Host := Rem_Host.Id;
      Partner.Port := Rem_Port.Num;
      Partner.Mode := Reliable;
      Partner.Addr := As.U.Tus (Address & Mode_Suffix (Partner.Mode));
    exception
      when others => return False;
    end;

    -- Find partner by host, port, mode
    Partner_Found := Partners.Search_Match (Partner_Match_Hpm'Access, Partner,
                                    From => Partner_List_Mng.Current_Absolute);
    if Partner_Found then
      Partner_Acc := Partners.Access_Current;
    end if;

    -- Handle Death
    if Message(1) = 'D' then
      if Partner_Found then
        -- Death of a known partner, remove it and close connection
        Logger.Log_Debug ("Ipm: Death of partner " & Partner.Addr.Image);
        Notify_Sup (Partner_Acc.Bus, Partner_Acc.Addr, Trilean.False);
        Remove_Current_Partner (True);
      end if;
      -- End of processing of a death message
      return False;
    end if;

    -- Handle Alive
    if not Partner_Found then
      -- New (unknown yet) partner
      if Buses.Access_Current.Kind /= Multicast
      and then Smaller (Partner_Ipaddr, Partner.Bus.Ipaddr) then
        -- We are Reliable (not multicast), and
        -- Partner < Own: we send an Alive, then the partner will connect to us
        --  (and we will accept) then it will send its address
        Logger.Log_Debug ("Ipm: Waiting for connection from "
                        & Partner.Addr.Image);
        Send_Adm ((if Partner.Bus.Kind = Active then Trilean.True
                   else Trilean.False));
      else
        -- We are Multicast or partner >= Own: add partner and start connect
        Logger.Log_Debug ("Ipm: Connecting to new partner "
                        & Partner.Addr.Image);
        Partner.State := Init;
        -- Save partner state received, for when connection establishes
        Partner.Init_State := (if Message(1) = 'A' then Active else Passive);
        Partner.Timer := new Chronos.Passive_Timers.Passive_Timer;
        Insert_Partner (Partner);
        -- The callback can be called synchronously
        Unused_Connected := Tcp_Util.Connect_To (
            Socket.Tcp_Header,
            Rem_Host, Rem_Port,
            Tcp_Connection_Cb'Access,
            Partner.Bus.Timeout, 0,
            Partner.Bus.Ttl);
      end if;
    else
      -- Partner found
      if Partner_Acc.Timer.Running then
        -- This partner is known with a timer, restart its keep alive timer
        Start_Partner_Timer (Partner_Acc.Bus);
      end if;
    end if;
    return False;
  end Ipm_Reception_Cb;

  -- Cb of the active Timer for the Bus
  function Timer_Cb (Id : in Timers.Timer_Id;
                     Unused_Data : in Timers.Timer_Data) return Boolean is
    Bus : Bus_Rec;
    Bus_Acc : Bus_Access;
  begin
    Logger.Log (All_Timers, "Expiration of Bus active timer");
    -- Find Bus
    Bus.Heartbeat_Timer := Id;
    if not Buses.Search_Match (Bus_Match_Timer'Access, Bus,
                       From => Bus_List_Mng.Current_Absolute) then
      Log_Error ("Timer_Cb", "bus not found", "in buses list");
      return False;
    end if;

    -- Send Alive message if active
    Bus_Acc := Buses.Access_Current;
    if Bus_Acc.Kind = Active or else Bus_Acc.Passive_Timer.Has_Expired then
      Logger.Log (All_Timers, "Expiration of Bus passive timer");
      Send_Adm ((if Bus_Acc.Kind = Active then Trilean.True
                 else Trilean.False));
    end if;

    -- Check partners keep alive timers and remove dead ones
    Remove_Partners (False);
    return False;
  end Timer_Cb;

  -- Is a partner connected (connection is not Init nor Shadow)
  function Talk_To (State : Partner_State_List) return Boolean is
  begin
    return State /= Init and then State /= Shadow;
  end Talk_To;

  -- Check that message is not empty and not too long
  procedure Check_Message (Msg : in String) is
  begin
    if Msg'Length = 0 then
      raise Empty_Message;
    end if;
    if Msg'Length > Tcp_Message_Str'Length then
      raise Message_Too_Long;
    end if;
  end Check_Message;

  ------------
  -- PUBLIC --
  ------------

  -- Initialise a Bus
  procedure Init (Bus : in out Bus_Type;
                  Address : in String;
                  Kind : in Bus_Kind := Active;
                  Sup_Cb : in  Sup_Callback := null) is
    Rbus : Bus_Rec;
    Port_Num : Socket.Port_Num;
    Timeout : Timers.Delay_Rec (Timers.Delay_Sec);
  begin
    Logger.Init ("Autobus");
    Check_In_Callback;
    -- Init Pid image
    if Pid_Image(Pid_Image'First) = ' ' then
      Pid_Image := Normal (Integer(Sys_Calls.Get_Pid),
                           Pid_Image'Length, Gap => '0');
    end if;
    -- Check that Bus is not already initialised
    if Bus.Acc /= null then
      raise Status_Error;
    end if;

    -- Create socket, parse and check address, configure socket
    declare
      Rem_Host : Tcp_Util.Remote_Host;
      Rem_Port : Tcp_Util.Remote_Port;
      use type Socket.Host_Id;
    begin
      -- Name is "<ip_address>:<port_num>"
      Ip_Addr.Parse (Address, Rem_Host, Rem_Port);
      Rbus.Name := As.U.Tus (Image (Ip_Addr.Resolve (Rem_Host, Lan => True),
                                    Ip_Addr.Resolve (Rem_Port, Socket.Udp)));
      Logger.Log_Debug ("Bus " & Rbus.Name.Image & " initialializing");
      -- Check that this address is not already associated to a bus
      if Buses.Search_Match (Bus_Match_Name'Access, Rbus,
                             From => Bus_List_Mng.Current_Absolute) then
        Logger.Log_Debug ("Bus " & Rbus.Name.Image  & " already in use");
        raise Address_In_Use;
      end if;

      -- Set host address to match alias or LAN in config, or to local host
      Rbus.Host_If := Config.Get_Interface (Rbus.Name.Image);

      -- Now we set the socket to the proper interface for sending and
      --  receiving IPM
      if Rbus.Host_If = Socket.Local_Host_Id then
        Logger.Log_Debug ("Bus initialializing on default interface");
      else
        Logger.Log_Debug ("Bus initialializing on specific interface " &
               Ip_Addr.Image (Rbus.Host_If));
      end if;

      -- Open the IPM UDP socket and configure it
      Rbus.Adm.Open (Socket.Udp);
      Rbus.Adm.Set_Sending_Ipm_Interface (Rbus.Host_If);
      Rbus.Adm.Set_Reception_Interface (Rbus.Host_If);
      Socket_Util.Set_Destination (Rbus.Adm, Lan => True,
                                   Host => Rem_Host, Port => Rem_Port);
      Rbus.Host := Socket.Get_Destination_Host (Rbus.Adm);
      Rbus.Port := Socket.Get_Destination_Port (Rbus.Adm);
      Socket_Util.Link (Rbus.Adm, Rem_Port);
      Logger.Log_Debug ("IPM socket initialialized");
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
                       Rbus.Ttl,
                       Rbus.Passive_Factor);
    Rbus.Kind := Kind;
    Rbus.Sup_Cb := Sup_Cb;

    -- Set admin callback
    Ipm_Reception_Mng.Set_Callbacks (Rbus.Adm, Ipm_Reception_Cb'Access, null,
                                     Set_For_Reply => Rbus.Kind = Multicast);

    if Rbus.Kind = Multicast then
      -- Create the UDP Pt to Pt socket
      Rbus.Acc.Open (Socket.Udp);
      Rbus.Acc.Set_Reception_Interface (Rbus.Host_If);
      Rbus.Acc.Link_Dynamic;
      -- Set reception callback. The Set_For_Reply is needed to detect our
      --  own messages
      Ipm_Reception_Mng.Set_Callbacks (Rbus.Acc, Ipm_Reception_Cb'Access, null,
                                       Set_For_Reply => True);
      Logger.Log_Debug ("UDP socket initialialized");
      -- Unique identifier of this process
      Rbus.Addr := As.U.Tus (Image (Rbus.Host_If, Rbus.Acc.Get_Linked_To)
                           & Mode_Suffix (Multicast));
    else
      -- Create the TCP accepting socket, set accep callback
      Tcp_Util.Accept_From (Socket.Tcp_Header,
                            (Kind => Tcp_Util.Port_Dynamic_Spec),
                            Tcp_Accept_Cb'Access,
                            Rbus.Acc, Port_Num,
                            Rbus.Host_If);
      Logger.Log_Debug ("TCP socket initialialized");

      -- Unique identifier of this process
      Rbus.Addr := As.U.Tus (Image (Rbus.Host_If, Port_Num)
                           & Mode_Suffix (Reliable));
    end if;
    -- Also store the IP:Port
    Rbus.Ipaddr := Rbus.Addr;
    Rbus.Ipaddr.Trail (2);

    -- Set TTL on Admin and Accept (TCP or UDP) sockets
    Rbus.Adm.Set_Ttl (Rbus.Ttl);
    Rbus.Acc.Set_Ttl (Rbus.Ttl);

    -- Arm Bus related active timer and create passive timer
    Timeout.Delay_Seconds := 0.0;
    Timeout.Period := Rbus.Heartbeat_Period;
    Rbus.Heartbeat_Timer.Create (Timeout, Timer_Cb'Access);
    Logger.Log (All_Timers, "Started Bus active timer");
    Rbus.Passive_Timer := new Chronos.Passive_Timers.Passive_Timer;
    Timeout.Period := Rbus.Heartbeat_Period * Rbus.Passive_Factor;
    Rbus.Passive_Timer.Start (Timeout);
    Logger.Log_Debug ("Reliable timer created");
    Logger.Log (All_Timers, "Started Bus passive timer");

    -- Done: Insert in list, return access
    Buses.Rewind (Bus_List_Mng.Next, False);
    Buses.Insert (Rbus);
    Bus.Acc := Buses.Access_Current;

    -- Report own address in Multicast
    -- (report of reliable own address will occur after  local connection)
    if Rbus.Kind = Multicast then
      Notify_Sup (Buses.Access_Current, Rbus.Addr, Trilean.Other);
    end if;

    Logger.Log_Debug ("Bus " & Rbus.Name.Image
                    & " created "
                    & (case Rbus.Kind is
                        when Active  => "Active",
                        when Passive =>  "Passive",
                        when Multicast => "Multicast")
                    & " at " & Rbus.Addr.Image);
    if Rbus.Kind /= Multicast then
      Logger.Log_Debug (" with Period: "
         & Images.Dur_Image (Rbus.Heartbeat_Period, 1, False)
         & ", MaxMissed: " & Images.Integer_Image(Rbus.Heartbeat_Max_Missed)
         & ", Timeout: " &  Images.Dur_Image (Rbus.Timeout, 1, False)
         & ", TTL: " & Images.Integer_Image(Rbus.Ttl)
         & ", PassiveFactor " & Images.Integer_Image(Rbus.Passive_Factor));

      -- Wait a little bit (100ms) for "immediate" connections to establish
      Event_Mng.Pause (100);
    end if;

  end Init;

  -- Is a Bus initialised
  function Is_Init (Bus : Bus_Type) return Boolean is
  begin
    return Bus.Acc /= null;
  end Is_Init;

  -- Reset a Bus (make it re-usable)
  procedure Reset (Bus : in out Bus_Type) is
    Unused_Bus_Found : Boolean;
    Dummy_Moved : Boolean;
    use type Socket.Socket_Dscr;
  begin
    Check_In_Callback;
    -- Check that Bus is initialised
    if Bus.Acc = null then
      raise Status_Error;
    end if;
    Unused_Bus_Found := Buses.Search_Access (Bus.Acc);
    Logger.Log_Debug ("Bus.Reset " & Bus.Acc.Name.Image);

    -- Send Death info (if possible)
    if Bus.Acc.Kind /= Multicast then
      begin
        Send_Adm (Trilean.Other);
      exception
        when others =>
          null;
      end;
    end if;

    -- Close resources
    Ipm_Reception_Mng.Remove_Callbacks (Bus.Acc.Adm);
    if Bus.Acc.Kind = Multicast then
      Ipm_Reception_Mng.Remove_Callbacks (Bus.Acc.Acc);
    else
      Tcp_Util.Abort_Accept (Socket.Tcp_Header, Bus.Acc.Acc.Get_Linked_To);
      if Bus.Acc.Passive_Timer.Running then
        Bus.Acc.Passive_Timer.Stop;
        Logger.Log (All_Timers, "Stopped Bus passive timer");
      end if;
      Bus.Acc.Heartbeat_Timer.Delete;
      Logger.Log (All_Timers, "Stopped Bus active timer");
      Deallocate (Bus.Acc.Passive_Timer);
    end if;

    -- Remove all partners
    Remove_Partners (True);

    -- Remove all subscribers
    Bus.Acc.Subscribers.Rewind (Subscriber_List_Mng.Next, False);
    loop
      exit when Bus.Acc.Subscribers.Is_Empty;
      Remove_Current_Subscriber;
    end loop;


    -- Delete current Bus from list
    Buses.Delete (Moved => Dummy_Moved);
    Bus.Acc := null;
  end Reset;

  -- Send a Message on a Bus
  procedure Send (Bus : in out Bus_Type; Message : in String) is
    Msg : Tcp_Message_Str;
    Partner_Acc : Partner_Access;
    Moved : Boolean;
    Success : Boolean;
    Dummy : Boolean;
  begin
    -- Check that Bus is initialised
    if Bus.Acc = null then
      raise Status_Error;
    end if;

    -- Check message length
    Check_Message (Message);

    if Bus.Acc.Kind = Multicast then
      -- Multicast
      Socket.Set_Destination_Host_And_Port (
                                   Socket => Bus.Acc.Adm,
                                   Host => Bus.Acc.Host,
                                   Port => Bus.Acc.Port);
      Ipm_Send (Bus.Acc.Adm, "M/" & Pid_Image & '/' & Message,
                             Pid_Image'Length + 3 + Message'Length);
    end if;

    if Bus.Acc.Partners.Is_Empty then
      -- No partner
      return;
    end if;

    -- Prepare message and list of parners
    Msg(1 .. Message'Length) := Message;
    Bus.Acc.Partners.Rewind (Partner_Access_List_Mng.Next, False);

    -- Send message on each partner except on the shadow connection to ourself
    --  (so we send it to ourself only once).
    loop
      exit when Bus.Acc.Partners.Is_Empty;
      Bus.Acc.Partners.Read (Partner_Acc, Moved => Moved);
      begin
        if Talk_To (Partner_Acc.State) then
          Dummy := Tcp_Send (Partner_Acc.Sock, null, null,
                             Bus.Acc.Timeout, Msg, Message'Length);
        end if;
        Success := True;
      exception
        when Socket.Soc_Conn_Lost =>
          Logger.Log_Debug ("Lost connection to " & Partner_Acc.Addr.Image);
          Success := False;
        when Tcp_Util.Timeout_Error =>
          Logger.Log_Debug ("Timeout while sending to "
                          & Partner_Acc.Addr.Image);
          Success := False;
        when Except:others =>
          Log_Exception ("Tcp_Send", Ada.Exceptions.Exception_Name (Except),
                         "sending to " & Partner_Acc.Addr.Image);
          Success := False;
      end;
      if not Success then
        -- Remove partner with which error occurs
        Dummy := Partners.Search_Access (Partner_Acc);
        Remove_Current_Partner (True);
      end if;
      exit when not Moved;
    end loop;
  end Send;

  -- Reply to the message currently being received
  -- If not in receive
  procedure Reply (Bus : in out Bus_Type; Message : in String) is
    Msg : Tcp_Message_Str;
    Partner_Acc : Partner_Access;
    Success : Boolean;
    Dummy : Boolean;
  begin
    -- We must be in receive
    if not Calling_Receive then
      raise Not_In_Receive;
    end if;

    -- Check that Bus is initialised
    if Bus.Acc = null then
      raise Status_Error;
    end if;

    -- Check message length
    Check_Message (Message);

    if Bus.Acc.Kind = Multicast then
      -- Multicast bus, send on the socket (Set_For_Reply was set)
      Ipm_Send (Bus.Acc.Adm, "M/" & Pid_Image & '/' & Message,
                             Pid_Image'Length + 3 + Message'Length);
      return;
    end if;

    if Bus.Acc.Partners.Is_Empty then
      -- No partner
      return;
    end if;

    -- Prepare message and (current) partner
    Msg(1 .. Message'Length) := Message;
    Partner_Acc := Partners.Access_Current;
    if not Talk_To (Partner_Acc.State) then
      return;
    end if;

    -- Send
    begin
      Dummy := Tcp_Send (Partner_Acc.Sock, null, null,
                         Bus.Acc.Timeout, Msg, Message'Length);
      Success := True;
    exception
      when Socket.Soc_Conn_Lost =>
        Logger.Log_Debug ("Lost connection to " & Partner_Acc.Addr.Image);
        Success := False;
      when Tcp_Util.Timeout_Error =>
        Logger.Log_Debug ("Timeout while sending to "
                        & Partner_Acc.Addr.Image);
        Success := False;
      when Except:others =>
        Log_Exception ("Tcp_Send", Ada.Exceptions.Exception_Name (Except),
                       "sending to " & Partner_Acc.Addr.Image);
        Success := False;
    end;
    if not Success then
      -- Remove partner with which error occurs
      Remove_Current_Partner (True);
    end if;
  end Reply;

  -- Mode definition (to identify a destination) and image
  Mode_List_Image : constant array (Mode_List) of Character :=
    (Reliable => 'R', Multicast => 'M');
  -- Returns "/R" or "/M"
  function Mode_Suffix (Mode : in Mode_List) return String is
  begin
    return '/' & Mode_List_Image(Mode);
  end Mode_Suffix;

  -- Send a message to one process
  procedure Send_To (Bus : in out Bus_Type;
                     Host_Port_Mode : in String;
                     Message : in String) is
    Host : Tcp_Util.Remote_Host;
    Port : Tcp_Util.Remote_Port;
    Host_Id : Socket.Host_Id;
    Port_Num : Socket.Port_Num;
    Mode : Mode_List;

  begin
    -- Check that Bus is initialised
    if Bus.Acc = null then
      raise Status_Error;
    end if;

    -- Check, Parse and resolve into Host_Id and Port_Num
    if Host_Port_Mode'Length < 3
    or else Host_Port_Mode(Host_Port_Mode'Last-1) /= '/' then
      raise Invalid_Address;
    end if;
    -- Parse IP addr & port
    Ip_Addr.Parse (
        Host_Port_Mode(Host_Port_Mode'First .. Host_Port_Mode'Last-2),
        Host, Port);
    -- Parse mode
    if Host_Port_Mode(Host_Port_Mode'Last) = Mode_List_Image(Reliable) then
      Mode := Reliable;
    elsif Host_Port_Mode(Host_Port_Mode'Last) = Mode_List_Image(Multicast) then
      Mode := Multicast;
    else
      raise Invalid_Address;
    end if;
    -- resolve host and port
    Host_Id := Ip_Addr.Resolve (Host, False);
    Port_Num := Ip_Addr.Resolve (Port,
        (if Mode = Reliable then Socket.Tcp else Socket.Udp));

    -- Send
    Send_To (Bus, Host_Id, Port_Num, Mode, Message);
  exception
    when Ip_Addr.Name_Error =>
      raise Unknown_Destination;
    when Ip_Addr.Parse_Error =>
      raise Invalid_Address;
  end Send_To;

  procedure Send_To (Bus : in out Bus_Type;
                     Host : in Socket.Host_Id;
                     Port : in Socket.Port_Num;
                     Mode : in Mode_List;
                     Message : in String) is
    Msg : Tcp_Message_Str;
    Partner_Acc : Partner_Access;
    Partner : Partner_Rec;
    Success : Boolean;
    Dummy : Boolean;
  begin
    -- Check that Bus is initialised
    if Bus.Acc = null then
      raise Status_Error;
    end if;

    -- Check that Reliable Bus sends reliable messages
    if Bus.Acc.Kind /= Multicast and then Mode = Multicast then
      raise Uncompatible_Mode;
    end if;

    -- Check message length
    Check_Message (Message);

    -- Send point to point UDP
    if Mode = Multicast then
      Socket.Set_Destination_Host_And_Port (
                                   Socket => Bus.Acc.Acc,
                                   Host => Host,
                                   Port => Port);
      Ipm_Send (Bus.Acc.Acc, "M/" & Pid_Image & '/' & Message,
                             Pid_Image'Length + 3 + Message'Length);
      return;
    end if;

    -- Search partner
    Partner.Host := Host;
    Partner.Port := Port;
    Partner.Mode := Mode;
    if not Partners.Search_Match (Partner_Match_Hpm'Access, Partner,
                          From => Partner_List_Mng.Current_Absolute) then
      raise Unknown_Destination;
    end if;
    Partner_Acc := Partners.Access_Current;

    -- Prepare message and check state of partner
    Msg(1 .. Message'Length) := Message;
    if not Talk_To (Partner_Acc.State) then
      return;
    end if;

    -- Send
    begin
      Dummy := Tcp_Send (Partner_Acc.Sock, null, null,
                         Bus.Acc.Timeout, Msg, Message'Length);
      Success := True;
    exception
      when Socket.Soc_Conn_Lost =>
        Logger.Log_Debug ("Lost connection to " & Partner_Acc.Addr.Image);
        Success := False;
      when Tcp_Util.Timeout_Error =>
        Logger.Log_Debug ("Timeout while sending to "
                        & Partner_Acc.Addr.Image);
        Success := False;
      when Except:others =>
        Log_Exception ("Tcp_Send", Ada.Exceptions.Exception_Name (Except),
                       "sending to " & Partner_Acc.Addr.Image);
        Success := False;
    end;
    if not Success then
      -- Remove partner with which error occurs
      Dummy := Partners.Search_Access (Partner_Acc);
      Remove_Current_Partner (True);
    end if;

  end Send_To;

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
    Subs : Subscriber_Rec;
    Ok : Boolean;
    Position : Natural;
  begin
    Check_In_Callback;
    -- Check that this Bus is initialised
    if Bus = null then
      raise Status_Error;
    end if;
    if not Buses.Search_Access (Bus.Acc) then
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
      Regular_Expressions.Compile (Subs.Filter.all, Ok, Filter,
                                   Multi_Line => True, Dot_All => True);
      if not Ok then
        Logger.Log_Debug ("Subscriber.Init regexp error "
                        & Regular_Expressions.Error (Subs.Filter.all));
        Regular_Expressions.Free (Subs.Filter.all);
        Deallocate (Subs.Filter);
        raise Invalid_Filter;
      end if;
    end if;

    -- Store in Bus, save position in case we are dispatching
    Position := (if Bus.Acc.Subscribers.Is_Empty then 0
                 else Bus.Acc.Subscribers.Get_Position);
    Bus.Acc.Subscribers.Rewind (Subscriber_List_Mng.Next, False);
    Bus.Acc.Subscribers.Insert (Subs);
    Subscriber.Acc := Bus.Acc.Subscribers.Access_Current;
    if Position /= 0 then
      Bus.Acc.Subscribers.Move_At (Position);
    end if;

    Logger.Log_Debug ("Subscriber "
                    & (if Filter /= "" then Filter & " " else "")
                    & "init ok");
  end Init;

  -- Is a Subscriber initialised
  function Is_Init (Subscriber : Subscriber_Type) return Boolean is
  begin
    return Subscriber.Acc /= null;
  end Is_Init;

  -- Reset a Subscriber (make it re-usable)
  procedure Reset (Subscriber : in out Subscriber_Type) is
  begin
    Check_In_Callback;
    if Subscriber.Acc = null then
      raise Status_Error;
    end if;
    if Subscriber.Acc.Bus = null then
      raise Status_Error;
    end if;
    -- Find bus
    if not Buses.Search_Access (Subscriber.Acc.Bus) then
      raise Status_Error;
    end if;
    -- Find subscriber in bus list
    if not Subscriber.Acc.Bus.Subscribers.Search_Access (Subscriber.Acc) then
      Logger.Log_Debug ("Subscriber.Reset subscriber unknown by its bus!");
      raise Status_Error;
    end if;

    Logger.Log_Debug ("Subscriber.Reset " & Subscriber.Acc.Bus.Name.Image);
    Remove_Current_Subscriber;
    Subscriber.Acc := null;
  end Reset;

  -- Dispatch the message to the subscribers of the provided bus
  procedure Dispatch (Message : in String;
                      Bus     : in Bus_Access;
                      Local   : in Boolean) is
    Subs : Subscriber_Access;
    Match_Info : Regular_Expressions.One_Match_Array;
    N_Match : Natural;
    Ok : Boolean;
  begin

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
          -- See if message strictly matches this Filter
          Regular_Expressions.Exec (Subs.Filter.all, Message,
                                    N_Match, Match_Info);
          Ok := Regular_Expressions.Strict_Match (Message, Match_Info);
        end if;
      end if;
      if Ok then
        begin
          Calling_Receive := True;
          Subs.Observer.Receive (Subs.Client, Message);
          Calling_Receive := False;
        exception
          when Error:others =>
            Calling_Receive := False;
            Logger.Log_Debug ("Receive raised exception: "
                            & Ada.Exceptions.Exception_Name (Error));
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
  -- Other accesses (including filling the listst) are done by Access_Current
  procedure Set (To : out Bus_Rec; Val : in Bus_Rec) is
  begin
    -- Copy all fields except the lists (of Partners and Subscribers)
    To.Name := Val.Name;
    To.Addr := Val.Addr;
    To.Ipaddr := Val.Ipaddr;
    To.Adm := Val.Adm;
    To.Host := Val.Host;
    To.Port := Val.Port;
    To.Acc := Val.Acc;
    To.Host_If := Val.Host_If;
    To.Sup_Cb := Val.Sup_Cb;
    To.Kind := Val.Kind;
    To.Heartbeat_Period := Val.Heartbeat_Period;
    To.Heartbeat_Max_Missed := Val.Heartbeat_Max_Missed;
    To.Timeout := Val.Timeout;
    To.Ttl := Val.Ttl;
    To.Passive_Factor := Val.Passive_Factor;
    To.Heartbeat_Timer := Val.Heartbeat_Timer;
    To.Passive_Timer := Val.Passive_Timer;
    -- Lists must be empty because this is Bus creation
    if not Val.Partners.Is_Empty or else not Val.Subscribers.Is_Empty then
      raise Internal_Error;
    end if;
  end Set;

  -- Internal: Finalizations
  overriding procedure Finalize (Bus : in out Bus_Type) is
  begin
    Logger.Log (Finalizations, "Finalizing Bus");
    if Bus.Acc /= null then
      Reset (Bus);
    end if;
    Logger.Log (Finalizations, "Bus finalized");
  end Finalize;

  overriding procedure Finalize (Subscriber : in out Subscriber_Type) is
  begin
    Logger.Log (Finalizations, "Finalizing Subscriber");
    if Subscriber.Acc /= null then
      Reset (Subscriber);
    end if;
    Logger.Log (Finalizations, "Subscriber finalized");
  end Finalize;

end Autobus;

