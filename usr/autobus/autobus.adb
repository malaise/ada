with Ada.Unchecked_Deallocation, Ada.Exceptions;
with Basic_Proc, Int_Image, Ip_Addr, Socket_Util, Tcp_Util;
package body Autobus is

  --------------
  -- INTERNAL --
  --------------
  -- List of Buses and Partners
  Buses : Bus_List_Mng.List_Type;
  Partners : Partner_List_Mng.List_Type;

  -- Heartbeat
  Heartbeat_Period : constant Duration := 0.33;
  Heartbeat_Missed_Factor : constant := 3;

  -- Connection timeout
  Connection_Timeout : constant Duration := 0.5;

  -- Internal inconsistency
  Internal_Error : exception;

  -- Internal: Image of a port num
  function Port_Image is new Int_Image (Socket.Port_Num);

  -- Internal Image of a full address
  function Image (Host : Socket.Host_Id; Port : Socket.Port_Num)
                 return String is
  begin
    return Ip_Addr.Image (Socket.Id2Addr (Host)) & ":" & Port_Image (Port);
  end Image;

  -- Internal: Log an exception
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

  -- Internal: Log an error
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


  -- Internal: Search Partner
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
  -- By access
  function Partner_Match_Acc (Curr, Crit : Partner_Access) return Boolean is
  begin
    return Curr = Crit;
  end Partner_Match_Acc;
  -- By Host and port
  function Partner_Match_Hp (Curr, Crit : Partner_Rec) return Boolean is
    use type Socket.Host_Id, Socket.Port_Num;
  begin
    return Curr.Host = Crit.Host and then Curr.Port = Crit.Port;
  end Partner_Match_Hp;

  -- Internal: Search Bus
  -- By address
  function Bus_Match_Addr (Curr, Crit : Bus_Rec) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Curr.Addr = Crit.Addr;
  end Bus_Match_Addr;
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

  -- Internal: Remove current (in Partners list) Partner
  -- Remove its ref in its Bus list and remove it from Parrners list
  procedure Remove_Current_Partner is
    Partner_Acc : Partner_Access;
    Partner_Found : Boolean;
    Moved : Boolean;
  begin
    -- Find partner ref in Bus
    Partner_Acc := Partner_Access (Partners.Access_Current);
    Partner_Acc.Bus.Partners.Search_Match (
           Partner_Found, Partner_Match_Acc'Access, Partner_Acc,
           From => Partner_Access_List_Mng.Absolute);
    if not Partner_Found then
      Log_Error ("Remove_Current_Partner", "not found", "in bus list");
    end if;

    -- Delete this partner from bus list
    Partner_Acc.Bus.Partners.Delete (Moved => Moved);

    -- Close this partner resources
    if Partner_Acc.Sock.Is_Open then
      -- This partner is connected
      Partner_Acc.Sock.Close;
    else
      -- There is a pending connection attempt to this partner
      begin
        -- Connection was requested with host and port Ids
        Tcp_Util.Abort_Connect (
           Host => (Kind => Tcp_Util.Host_Id_Spec, Id => Partner_Acc.Host),
           Port => (Kind => Tcp_Util.Port_Num_Spec, Num => Partner_Acc.Port));
      exception
        when Tcp_Util.No_Such =>
          Log_Error ("Remove_Current_Partner", "no such",
                     "while aborting pending tcp connection");
      end;
    end if;

    -- Delete this partner from Partners
    Partners.Search_Access (Partner_Found, Partner_Acc);

  end Remove_Current_Partner;

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
  procedure Tcp_Connection_Cb (Remote_Host_Id  : in Tcp_Util.Host_Id;
                               Remote_Port_Num : in Tcp_Util.Port_Num;
                               Connected       : in Boolean;
                               Dscr            : in Socket.Socket_Dscr) is
    Partner : Partner_Rec;
    Partner_Found : Boolean;
    Partner_Acc : Partner_Access;
  begin
    if not Connected then
      -- This should not occur because the number of connection retries
      --  is infinite
      Log_Error ("Tcp_Connection_Cb", "failure",
                 "shall not occur");
      return;
    end if;

    -- Set non blocking, find partner by Host and Port, update its Sock,
    Dscr.Set_Blocking (False);
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

    --  Create and start its timer
    Start_Partner_Timer;
  end Tcp_Connection_Cb;

  -- TCP disconnection Cb
  procedure Tcp_Disconnection_Cb (Dscr : in Socket.Socket_Dscr) is
    Partner : Partner_Rec;
    Partner_Found : Boolean;
  begin
    -- Remove partner
    -- Set non blocking, find partner by Host and Port, update its Sock,
    Partner.Sock := Dscr;
    -- Find partner by socket
    Partners.Search_Match (Partner_Found, Partner_Match_Sock'Access, Partner,
                           From => Partner_List_Mng.Absolute);
    if not Partner_Found then
      Log_Error ("Tcp_Disconnection_Cb", " partner not found",
                 "in partners list");
      return;
    end if;
    Remove_Current_Partner;
  end Tcp_Disconnection_Cb;

  -- TCP Reception Cb
  Tcp_Message_Max_Length : constant := 1024 * 1024;
  subtype Tcp_Message_Str is String (1 .. Tcp_Message_Max_Length);
  package Tcp_Reception_Mng is new Tcp_Util.Reception (Tcp_Message_Str);
  procedure Tcp_Reception_Cb (Dscr    : in Socket.Socket_Dscr;
                              Message : in Tcp_Message_Str;
                              Length  : in Natural);

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
      Log_Error ("Accept_Cb", "bus not found", "in bus list");
      return;
    end if;
    Partner.Addr := As.U.Tus (Image (Remote_Host_Id, Remote_Port_Num));
    Partner.Host := Remote_Host_Id;
    Partner.Port := Remote_Port_Num;
    Partner.Sock := New_Dscr;
    Partner.Bus := Bus_Access(Buses.Access_Current);
    Partners.Rewind (False, Partner_List_Mng.Next);
    Partners.Insert (Partner);
    Start_Partner_Timer;
    -- Set reception callback
    Tcp_Reception_Mng.Set_Callbacks (New_Dscr,
                                     Tcp_Reception_Cb'Access,
                                     Tcp_Disconnection_Cb'Access);

  end Tcp_Accept_Cb;

  -- IPM Reception Cb
  -- IPM message is 'A' or 'D' then '/' then IP address then ":" then port num
  --  Ex: "A/123.123.123.123:65535"
  -- Message received shall not exceed 23 chars
  Ipm_Message_Max_Length : constant := 23 + 1;
  subtype Ipm_Message_Str is String (1 .. Ipm_Message_Max_Length);
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
        Log_Error ("Ipm_Reception_Cb", "bus not found", "in bus list");
        return;
      end if;
      -- Set partner bus
      Partner.Bus := Bus_Access(Buses.Access_Current);
      -- Discard our own admin message
      if Partner.Addr = Partner.Bus.Name then
        return;
      end if;
    end;

    -- Find partner by address
    Partners.Search_Match (Partner_Found, Partner_Match_Addr'Access, Partner,
                           From => Partner_List_Mng.Absolute);

    -- Handle Death
    if Message(1) = 'D' then
      if not Partner_Found then
        -- Death of an unknown partner
        return;
      else
        Remove_Current_Partner;
      end if;
    end if;

    -- Handle Alive
    if not Partner_Found then
      -- New (unknown yet) partner
      if Partner.Addr < Partner.Bus.Name then
        -- Addr < own: then the partner will connect to us (and we will accept)
        return;
      end if;
      -- Addr > own: add partner and start connect
      Partners.Rewind (False, Partner_List_Mng.Next);
      Partners.Insert (Partner);
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
  -- Send Alive and check partners keep alive timers
  -- @@@

  ------------
  -- PUBLIC --
  ------------

  -- Initialise a Bus
  procedure Init (Bus : in out Bus_Type;
                  Address : in String) is
    Rbus : Bus_Rec;
    Port_Num : Socket.Port_Num;
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
                       "Administration IPM socket creation");
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
    -- @@@

    -- Done: Insert in list, return access
    Buses.Rewind (False, Bus_List_Mng.Next);
    Buses.Insert (Rbus);
    Bus.Acc := Buses.Access_Current;
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

  procedure Tcp_Reception_Cb (Dscr    : in Socket.Socket_Dscr;
                              Message : in Tcp_Message_Str;
                              Length  : in Natural) is
  begin
    -- Find bus
    -- Notify matching Subscribers
    -- @@@
    null;
  end Tcp_Reception_Cb;

  ---------------
  -- INTERNAL --
  ---------------
  -- Internal: Copy a Bus_Rec, for dynamic list of Buses
  -- This is used only to store the bus in the Buses at creation
  -- Other accesses are done by Access_Current
  procedure Set (To : out Bus_Rec; Val : in Bus_Rec) is
  begin
    To.Admin := Val.Admin;
    To.Accep := Val.Accep;
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

