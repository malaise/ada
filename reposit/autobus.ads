-- Simple API for reliable message passing
with Ada.Finalization;
with Socket, Regular_Expressions, As.U,
     Dynamic_List, Limited_List,
     Timers, Chronos.Passive_Timers;
package Autobus is

  -- Concept
  -- Several processes connect to a Bus on which they can publish messages.
  -- Each message is dispatched to observers, according to filters on the
  --  content of the message.

  -- How to use:
  -- * First, create and initialise a Bus (provide an IPM address or LAN name,
  --    and a port num or name).
  --   Then you can send messages (strings) on the Bus.
  --   You can tune this Bus in the XML file indicated in the ENV variable
  --    AUTOBUS_CONFIG (see below).
  -- * Second, create an Observer (with a procedure Receive on it)
  --    and a Subscriber.
  --   Then init the Subscriber, possibly with a filter expression on the
  --     content of the expected messages.
  --    The procedure Receive will be called with the messages (received on the
  --     Bus) that match the filter.

  -- Implementation:
  -- Each Bus relies on a fixed IPM address and port, and a random TCP port.
  -- Each process sends periodically an IPM message with his host and TCP port.
  -- The other processes on the bus keep a list of known alive partners.
  -- When a new process starts it declares itself on the Bus and all the
  --  partners either connects to it or get connected to it.
  -- Delivering a message consists in sending it in TCP successively to the all
  --  the partners, each of them dispatching the message to the local observers.

  -- Tuning the Bus:
  -- A XML file allows the default tuning for all the Buses, and also a specific
  --  tuning of each Bus.
  -- For the default and also for each individual Bus:
  -- - Heartbeat_Period is the period in seconds at which each process on
  --    the Bus sends the alive message. It is used in combination with
  --    Heartbeat_Max_Missed. Default 1.
  -- - Heartbeat_Max_Missed, the number of missing alive messages after which
  --    the partners consider that a a process is dead (or at least unreachable)
  --    and discard it from the list of partners. Default 3.
  -- - Timeout for connecting and for sending each TCP message. When it fails
  --    the corresponding partner is discarded. Default 0.5.
  --    Note that the Timeout applies to each attempt of TCP connection. After
  --    each timeout there is a new attempt... likely until the partner is
  --    finally discarded because of alive timeout.
  -- - TTL for both IPM and TCP exchanges. Default 5.
  -- For each Bus, two ways to set a specific network interface for IPM and TCP:
  -- - Alias defines the IP address of the interface to be used by a given host.
  --    Ex: Name="telemaque" Address="192.168.0.5" means: if your local host
  --    name is telemaque then use the local interface with address 192.168.0.5
  -- - LAN defines the IP address and netmask of a LAN to use, if locally
  --    connected to the local host. Ex: Address="10.100.12.0"
  --    Netmask="255.255.255.0" means: if one of the local interfaces is
  --    10.100.12.*, then use this interface. A LAN directive will apply to all
  --    all the hosts on the LAN, except for those which are previously defined
  --    in an Alias.
  -- Aliases and LANs are tested in the order of declaration. If none matches
  --  then the bus listens to, and sends the IPM messages on the interface
  --  associated to the local host name.
  -- See the DTD Autobus.dtd for the format of the XML file.

  -- Note that the exclusion of a partner (either because it informs that it is
  --  dead, or because of a timeout of alive message, on connection or on
  --  sending) is not definitive. The partner is re-inserted when it is running
  --  and reachable again (i.e. when we receive an alive message from it). It
  --  only misses the applicative  messages that were sent meanwhile.

  -------------
  -- The Bus --
  -------------
  type Bus_Type is tagged limited private;
  type Bus_Access_Type is access all Bus_Type;

  -- Initialise a Bus, may raise:
  -- On incorrect format (not <lan>:<port>, invalid LAN or port)
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

  --------------------
  -- The Subscriber --
  --------------------
  type Subscriber_Type is tagged limited private;
  type Subscriber_Access_Type is access all Subscriber_Type;

  -- The Observer is notified with the messages (sent on the Bus)
  --  that pass the filter
  -- Filter is a PCRE regular expression
  -- Empty filter lets all messages pass through
  -- Echo allows enabling observation of messages sent by own process
  -- In Receive it is forbidden to initialise or reset a Bus or a Subscriber,
  --  this would raise the exception In_Receive
  -- Exceptions raised by Receive itself are caught and hidden
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

  -- If initialising or resetting a Bus or a Subscriber while in Receive
  In_Receive : exception;

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
    -- Host Id denoting the interface (for TCP and IPM)
    Host_If : Socket.Host_Id;
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

  -- Lists of buses ans partners
  -- Defined in spec in order to avoid that the finalization of the body
  --  (which may occur before the finalization of user data) deletes the list
  --  too early
  Buses : Bus_List_Mng.List_Type;
  Partners : Partner_List_Mng.List_Type;

end Autobus;

