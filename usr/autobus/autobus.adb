with Ada.Unchecked_Deallocation, Ada.Exceptions;
with Basic_Proc, Ip_Addr, Socket_Util, Tcp_Util;
package body Autobus is

  -- List of Buses and Partners
  Buses : Bus_List_Mng.List_Type;
  Partners : Partner_List_Mng.List_Type;

  -- Pending connection Cb
  -- If OK, find bus in pending list and insert partner

  -- Accept connection Cb
  -- Find bus and insert partner, set reception callback

  -- Reception Cb
  -- Find bus
  -- On disconnection remove partner
  -- On message notify mathcing Subscribers

  -- Internal inconsistency
  Internal_Error : exception;

  -- Internal: Log an an exception
  procedure Log_Exception (Operation, Exception_Name, Message : in String) is
  begin
    Basic_Proc.Put_Error ("Autobus: Exception " & Exception_Name
                        & " raised in " & Operation);
    if Message = "" then
      Basic_Proc.Put_Line_Error (".");
    else
      Basic_Proc.Put_Line_Error (", " & Message);
    end if;
  end Log_Exception;

  -- Initialise a Bus
  procedure Init (Bus : in out Bus_Type;
                  Address : in String) is
    Rbus : Bus_Rec;
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

    -- Create Accep TCp socket, set accep callback

    -- Done: Append in list, return access
    Buses.Rewind (False, Bus_List_Mng.Prev);
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

  -------------
  -- Private --
  -------------

  -- Copy a Bus_Rec, for dynamic list of Buses
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

