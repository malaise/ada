with Ada.Unchecked_Deallocation, Ada.Exceptions;
with Basic_Proc, Environ, Int_Image, Ip_Addr, Socket_Util, Tcp_Util;
package body Autobus is

  --------------
  -- INTERNAL --
  --------------
  -- List of Buses and Partners
  Buses : Bus_List_Mng.List_Type;
  Partners : Partner_List_Mng.List_Type;

  -- Heartbeat (Todo: getenv for each Bus)
  Heartbeat_Period : constant Duration := 1.0;
  Heartbeat_Missed_Factor : constant := 3;

  -- Connection timeout (Todo: getenv for each Bus)
  Connection_Timeout : constant Duration := 0.5;

  -- Ipm message type
  -- 'A' or 'D' then '/' then IPM address then ":" then port num
  --  Ex: "A/123.123.123.123:65535"
  -- Message received shall not exceed 23 chars
  -- We read a message of 24 characters to check that it is shorter than 23
  Ipm_Message_Max_Length : constant := 23 + 1;
  subtype Ipm_Message_Str is String (1 .. Ipm_Message_Max_Length);

  -- Tcp message type
  Tcp_Message_Max_Length : constant := 1024 * 1024 + 1;
  subtype Tcp_Message_Str is String (1 .. Tcp_Message_Max_Length);

  -- Internal inconsistency
  Internal_Error : exception;

  -- Image of a port num
  function Port_Image is new Int_Image (Socket.Port_Num);

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
                        & " raised in " & Operation);
    if Message = "" then
      Basic_Proc.Put_Line_Error (".");
    else
      Basic_Proc.Put_Line_Error (", " & Message & '.');
    end if;
  end Log_Error;

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

  -- Remove current (in Partners list) Partner
  -- Remove its ref in its Bus list and remove it from Partners list
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
  procedure Start_Partner_Timer is
    Partner_Acc : Partner_Access;
    Timeout : Timers.Delay_Rec (Timers.Delay_Sec);
  begin
    Partner_Acc := Partner_Access(Partners.Access_Current);
    Timeout.Delay_Seconds := Heartbeat_Missed_Factor * Heartbeat_Period;
    Partner_Acc.Timer.Start (Timeout);
  end Start_Partner_Timer;

  -- Pending connection Cb
  procedure Tcp_Send is new Socket.Send (Tcp_Message_Str,
                                         Tcp_Message_Max_Length);
  procedure Tcp_Connection_Cb (Remote_Host_Id  : in Tcp_Util.Host_Id;
                               Remote_Port_Num : in Tcp_Util.Port_Num;
                               Connected       : in Boolean;
                               Dscr            : in Socket.Socket_Dscr) is
    Partner : Partner_Rec;
    Partner_Found : Boolean;
    Partner_Acc : Partner_Access;
    Message : Tcp_Message_Str;
    Message_Length : Natural;
  begin
    if not Connected then
      -- This should not occur because the number of connection retries
      --  is infinite
      Log_Error ("Tcp_Connection_Cb", "failure",
                 "shall not occur");
      return;
    end if;

    -- Set non blocking, find partner by Host and Port, update its Sock,
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
    Partner_Acc.Sock := Dscr;
    Partner_Acc.Sock.Set_Blocking (False);
    Debug ("Connection to partner " & Partner_Acc.Addr.Image);

    -- Send identification message
    Message_Length := Partner_Acc.Bus.Addr.Length + 1;
    Message(1 .. Message_Length) := 'I' & Partner_Acc.Bus.Addr.Image;
    Tcp_Send (Partner_Acc.Sock, Message, Message_Length);

    --  Create and start its timer
    Start_Partner_Timer;
  end Tcp_Connection_Cb;

  -- TCP disconnection Cb
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

  -- Dispatch a message to the Subscribers of current bus
  procedure Dispatch (Message : in String);

  -- TCP Reception Cb
  package Tcp_Reception_Mng is new Tcp_Util.Reception (Tcp_Message_Str);
  procedure Tcp_Reception_Cb (Dscr    : in Socket.Socket_Dscr;
                              Message : in Tcp_Message_Str;
                              Length  : in Natural) is
    Partner : Partner_Rec;
    Partner_Found : Boolean;
    Partner_Acc : Partner_Access;
  begin
    -- Find partner by Socket
    -- Find partner by socket
    Partner.Sock := Dscr;
    Partners.Search_Match (Partner_Found, Partner_Match_Sock'Access, Partner,
                           From => Partner_List_Mng.Absolute);
    if not Partner_Found then
      Log_Error ("Tcp_Reception_Cb", " partner not found",
                 "in partners list");
      return;
    end if;
    Partner_Acc := Partner_Access(Partners.Access_Current);

    -- Dispatch Data message
    if Length = 0 then
      Log_Error ("Tcp_Reception_Cb", " empty message ",
                 " from " & Partner_Acc.Addr.Image);
      return;
    end if;
    if Message(1) = 'D' then
      Debug ("Reception of Data from " & Partner_Acc.Addr.Image
           & " " & Message (2 .. Length));
      Dispatch (Message (2 .. Length));
      return;
    end if;

    -- Process Identification service message
    if Message(1) = 'I' then
      -- The parner (connecting to us) sends us its accept address
      Partner_Acc.Addr := As.U.Tus (Message (2 .. Length));
      Debug ("Reception of Identification from " & Partner_Acc.Addr.Image);
      return;
    else
      Log_Error ("Tcp_Reception_Cb", " unexpected service message ",
                 Message & " from " & Partner_Acc.Addr.Image);
      return;
    end if;

  end Tcp_Reception_Cb;

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
    Partner.Host := Remote_Host_Id;
    Partner.Port := Remote_Port_Num;
    Partner.Sock := New_Dscr;
    Partner.Timer := new Chronos.Passive_Timers.Passive_Timer;
    Partner.Bus := Bus_Access(Buses.Access_Current);
    Insert_Partner (Partner);
    Debug ("Acception of partner " & Partner.Addr.Image);
    -- Insert in Bus a reference to this partner
    Start_Partner_Timer;
    -- Set reception callback
    New_Dscr.Set_Blocking (False);
    Tcp_Reception_Mng.Set_Callbacks (New_Dscr,
                                     Tcp_Reception_Cb'Access,
                                     Tcp_Disconnection_Cb'Access);

  end Tcp_Accept_Cb;

  -- Send Alive message of current Bus
  procedure Ipm_Send is new Socket.Send (Ipm_Message_Str,
                                         Ipm_Message_Max_Length - 1);
  procedure Send_Alive is
    Message_Len : Natural;
    Message : Ipm_Message_Str;
    Bus_Acc : Bus_Access;
  begin
    Bus_Acc := Bus_Access(Buses.Access_Current);
    Message_Len := Bus_Acc.Addr.Length + 2;
    Message(1 .. Message_Len) := "A/" & Bus_Acc.Addr.Image;
    Ipm_Send (Bus_Acc.Admin, Message, Message_Len);
  end Send_Alive;

  -- IPM Reception Cb
  package Ipm_Reception_Mng is new Tcp_Util.Reception (Ipm_Message_Str);
  procedure Ipm_Reception_Cb (Dscr    : in Socket.Socket_Dscr;
                              Message : in Ipm_Message_Str;
                              Length  : in Natural) is
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
      return;
    end if;
    begin
      -- Check address IP part
      Ip_Addr.Parse (Address, Rem_Host, Rem_Port);
      if Rem_Host.Kind = Tcp_Util.Host_Name_Spec
      or else Rem_Port.Kind = Tcp_Util.Port_Name_Spec then
        -- Not an IP address or not a port num
        return;
      end if;
      -- Set partner address
      Partner.Addr := As.U.Tus (Address);
      Partner.Host := Rem_Host.Id;
      Partner.Port := Rem_Port.Num;
    exception
      when others => return;
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
        return;
      end if;
      -- Set partner bus
      Partner.Bus := Bus_Access(Buses.Access_Current);
      -- Discard our own admin message
      if Partner.Addr = Partner.Bus.Addr then
        return;
      end if;
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
      return;
    end if;

    -- Handle Alive
    if not Partner_Found then
      -- New (unknown yet) partner
      if Partner.Addr < Partner.Bus.Addr then
        -- Addr < own: then the partner will connect to us (and we will accept)
        --  and it will send its address
        Debug ("Ipm: Waiting for new partner to identify " & Partner.Addr.Image);
        Send_Alive;
        return;
      end if;
      -- Addr > own: add partner and start connect
      Debug ("Ipm: Connecting to new partner " & Partner.Addr.Image);
      Partner.Timer := new Chronos.Passive_Timers.Passive_Timer;
      Insert_Partner (Partner);
      -- The callback can be called synchronously
      Connected := Tcp_Util.Connect_To (Socket.Tcp_Header,
                                        Rem_Host, Rem_Port,
                                        Connection_Timeout, 0,
                                        Tcp_Connection_Cb'Access);
    else
      -- This partner is known, restart its keep alive timer
      Start_Partner_Timer;
    end if;
  end Ipm_Reception_Cb;

  -- Timer Cb
  function Timer_Cb (Id : in Timers.Timer_Id;
                     Data : in Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Data);
    Bus_Found : Boolean;
    Bus : Bus_Rec;
    Bus_Acc : Bus_Access;
    Partner_Acc : Partner_Access;
    Partner_Found : Boolean;
    Moved : Boolean;
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
    Send_Alive;

    -- Check partners keep alive timers
    Bus_Acc := Bus_Access(Buses.Access_Current);
    Bus_Acc.Partners.Rewind (False, Partner_Access_List_Mng.Next);
    loop
      exit when Bus_Acc.Partners.Is_Empty;
      Bus_Acc.Partners.Read (Partner_Acc, Moved => Moved);
      if Partner_Acc.Timer.Has_Expired then
        -- This partner is not alive (alive timeout has expired)
        Debug ("Alive timeout of partner " & Partner_Acc.Addr.Image);
        Partners.Search_Access (Partner_Found, Partner_Acc);
        if not Partner_Found  then
          Log_Error ("Timer_Cb", "partner not found", "in partners list");
        else
          Remove_Current_Partner (True);
        end if;
      end if;
      exit when not Moved;
    end loop;
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

    -- Set admin callback
    Ipm_Reception_Mng.Set_Callbacks (Rbus.Admin, Ipm_Reception_Cb'Access, null);

    -- Create Accep Tcp socket, set accep callback
    Tcp_Util.Accept_From (Socket.Tcp_Header,
                          (Kind => Tcp_Util.Port_Dynamic_Spec),
                          Tcp_Accept_Cb'Access,
                          Rbus.Accep, Port_Num);
    Rbus.Addr := As.U.Tus (Image (Socket.Local_Host_Id , Port_Num));

    -- Arm timer
    Timeout.Delay_Seconds := 0.0;
    Timeout.Period := Heartbeat_Period;
    Rbus.Timer.Create (Timeout, Timer_Cb'Access);

    -- Done: Insert in list, return access
    Buses.Rewind (False, Bus_List_Mng.Next);
    Buses.Insert (Rbus);
    Bus.Acc := Buses.Access_Current;
    Debug ("Bus " & Rbus.Name.Image & " created at " & Rbus.Addr.Image);
  end Init;

  -- Reset a Bus (make it re-usable)
  procedure Reset (Bus : in out Bus_Type) is
    use type Socket.Socket_Dscr;
  begin
    -- Check that Bus is initialised
    if Bus.Acc = null then
      raise Status_Error;
    end if;
    -- Abort connect on pending connections
  end Reset;

  -- Send a Message on a Bus
  procedure Send (Bus : in out Bus_Type; Message : in String) is
  begin
    null;
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
                  Bus : Bus_Access_Type;
                  Filter : in String;
                  Observer : access Observer_Type'Class) is
  begin
    null;
  end Init;

  -- Reset a Subscriber (make it re-usable)
  procedure Reset (Subscriber : in out Subscriber_Type) is
  begin
    null;
  end Reset;

  -- Dispatch the message to the subscribers of current bus
  procedure Dispatch (Message : in String) is
  begin
    -- Find bus
    -- Notify matching Subscribers
    -- @@@
    null;
  end Dispatch;

  ---------------
  -- INTERNAL --
  ---------------
  -- Internal: Copy a Bus_Rec, for dynamic list of Buses
  -- This is used only to store the bus in the Buses at creation
  -- Other accesses are done by Access_Current
  procedure Set (To : out Bus_Rec; Val : in Bus_Rec) is
  begin
    -- Caopy all fields except the lists (of Partners and Subscribers)
    To.Name := Val.Name;
    To.Addr := Val.Addr;
    To.Admin := Val.Admin;
    To.Accep := Val.Accep;
    To.Timer := Val.Timer;
    -- Lists are empty
    if not Val.Partners.Is_Empty or else not Val.Subscribers.Is_Empty then
      raise Internal_Error;
    end if;
  end Set;

  -- Internal: Finalizations
  procedure Deallocate is new Ada.Unchecked_Deallocation (Bus_Rec, Bus_Access);
  overriding procedure Finalize (List : in out Bus_Rec) is
  begin
    null;
  end Finalize;

  overriding procedure Finalize (List : in out Subscriber_Rec) is
  begin
    null;
  end Finalize;

  overriding procedure Finalize (List : in out Bus_Type) is
  begin
    null;
  end Finalize;

  overriding procedure Finalize (List : in out Subscriber_Type) is
  begin
    null;
  end Finalize;

end Autobus;

