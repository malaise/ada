-- Simple API for reliable message passing
with Ada.Finalization;
with Socket, Regular_Expressions, As.U,
     Dynamic_List, Limited_List,
     Timers, Chronos.Passive_Timers;
package Autobus is

  -- How to use:
  -- * First, create and initialise a Bus (provide an IPM address and port)
  --   You can tune this bus in the file indicated in the ENV variable
  --    AUTOBUS_CONFIG (see Autobus.dtd).
  --   Then you can send messages (strings) on it.
  -- * Second, create an observer (with a procedure Receive on it)
  --    and a Subscriber, and init the subscriber. The procedure Receive
  --    will be called with messages received on the Bus.

  -------------
  -- The Bus --
  -------------
  type Bus_Type is tagged limited private;
  type Bus_Access_Type is access all Bus_Type;

  -- Initialise a Bus, may raise:
  -- On incorrect format (not <lan>:<port>, invalid LAN or port num)
  Invalid_Address : exception;
  -- On LAN or port name not found (DNS, networks, services)
  Name_Error : exception;
  -- On error in the tuning configuration file (parsed at Init of first Bus)
  -- See Autobus.dtd for the format of this file
  Config_Error : exception;
  procedure Init (Bus : in out Bus_Type;
                  Address : in String);

  -- Is a Bus initialised
  function Is_Init (Bus : Bus_Type) return Boolean;

  -- Reset a Bus (make it re-usable)
  procedure Reset (Bus : in out Bus_Type);

  -- Send a Message on a Bus, may raise:
  -- On message longer than Message_Max_Length (1MB)
  Message_Max_Length : constant := 1024 * 1024;
  Message_Too_Long : exception;
  procedure Send (Bus : in out Bus_Type; Message : in String);

  -------------------
  -- The Subsciber --
  -------------------
  type Subscriber_Type is tagged limited private;
  type Subscriber_Access_Type is access all Subscriber_Type;

  -- The Observer is notified with the Messages (sent on the Bus)
  --  that pass the Filter
  -- Filter is a PCRE regular expression
  -- Empty filter lets all messages pass through
  -- Echo allows enabling observation of messages sent by own process
  -- In Receive it is forbidden to reset a Bus or a Subscriber
  -- Exceptions raised by Receive are caught and hidden
  type Observer_Type is limited interface;
  procedure Receive (Observer : in out Observer_Type;
                     Subscriber : in Subscriber_Access_Type;
                     Message : in String) is abstract;

  -- Initialise a Subscriber on a Bus, may raise:
  -- On incorrect filter expression
  Invalid_Filter : exception;
  procedure Init (Subscriber : in out Subscriber_Type;
                  Bus : in Bus_Access_Type;
                  Observer : access Observer_Type'Class;
                  Filter : in String := "";
                  Echo : in Boolean := False);

  -- Is a Subscriber initialised
  function Is_Init (Subscriber : Subscriber_Type) return Boolean;

  -- Reset a Subscriber (make it re-usable)
  procedure Reset (Subscriber : in out Subscriber_Type);

  ------------------------
  -- General Exceptions --
  ------------------------
  -- If initialising a Bus already initialised
  -- If resetting or sending on a Bus not initialised or reset
  -- If initialising a Subscriber already initialised
  -- If resetting a Subscriber not initialised or reset
  Status_Error : exception;

  -- If resetting a Bus or a Subscriber while in Receive
  Reset_In_Receive : exception;

  -- If any system error on any call
  System_Error : exception;

private

  -- List of partners
  type Bus_Rec;
  type Bus_Access is access all Bus_Rec;
  type Timer_Access is access all Chronos.Passive_Timers.Passive_Timer;
  type Partner_Rec is record
    -- Address of the TCP socket "www.xxx.yyy.zzz:portnum"
    Addr : As.U.Asu_Us;
    -- Low level address
    Host : Socket.Host_Id;
    Port : Socket.Port_Num;
    -- Socket
    Sock : Socket.Socket_Dscr;
    -- Timer of keep alive
    Timer : Timer_Access;
    -- Reference to the bus (when message received or disconnection)
    Bus : Bus_Access;
  end record;
  package Partner_Dyn_List_Mng is new Dynamic_List (Partner_Rec);
  package Partner_List_Mng renames Partner_Dyn_List_Mng.Dyn_List;
  type Partner_Access is access all Partner_Rec;
  package Partner_Access_Dyn_List_Mng is new Dynamic_List (Partner_Access);
  package Partner_Access_List_Mng renames Partner_Access_Dyn_List_Mng.Dyn_List;

  -- List of Subscribers
  type Filter_Access is access Regular_Expressions.Compiled_Pattern;
  type Observer_Access is access all Observer_Type'Class;
  type Subscriber_Rec is record
    -- Bus
    Bus : Bus_Access;
    -- Filter
    Filter : Filter_Access;
    Echo : Boolean;
    -- Observer
    Observer : Observer_Access;
    -- Client subscriber
    Client : Subscriber_Access_Type;
  end record;

  package Subscriber_Dyn_List_Mng is new Dynamic_List (Subscriber_Rec);
  package Subscriber_List_Mng renames Subscriber_Dyn_List_Mng.Dyn_List;

  -- List of Buses
  type Bus_Rec is limited new Ada.Finalization.Limited_Controlled with record
    -- Address of the IPM socket "www.xxx.yyy.zzz:portnum", for reporting
    Name : As.U.Asu_Us;
    -- Address of the TCP socket "www.xxx.yyy.zzz:portnum"
    Addr : As.U.Asu_Us;
    -- Administration IPM socket
    Admin : Socket.Socket_Dscr := Socket.No_Socket;
    -- TCP accept socket
    Accep : Socket.Socket_Dscr := Socket.No_Socket;
    -- Heartbeat period and Max missed number, Timeout on connect and send, TTL
    Heartbeat_Period : Duration := 1.0;
    Heartbeat_Max_Missed : Positive := 3;
    Timeout : Duration := 0.5;
    Ttl : Socket.Ttl_Range := 5;
    -- List of access to partners (TCP connections)
    Partners : Partner_Access_List_Mng.List_Type;
    -- List of subscribers
    Subscribers : Subscriber_List_Mng.List_Type;
    -- Timer
    Timer : Timers.Timer_Id;
  end record;
  procedure Set (To : out Bus_Rec; Val : in Bus_Rec);
  package Bus_List_Mng is new Limited_List (Bus_Rec, Set);

  -- Exported types
  type Subscriber_Type is new Ada.Finalization.Limited_Controlled with record
    Acc : access Subscriber_Rec;
  end record;
  overriding procedure Finalize (Subscriber : in out Subscriber_Type);
  type Bus_Type is new Ada.Finalization.Limited_Controlled with record
    Acc : access Bus_Rec;
  end record;
  overriding procedure Finalize (Bus : in out Bus_Type);

end Autobus;

