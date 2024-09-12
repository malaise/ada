-- Simple API for exchanging messages (reliably or not) through channels
private with Ada.Finalization;
private with Chronos.Passive_Timers, Dynamic_List, Reg_Exp, Timers,
             Limited_List;
with Socket, As.U, Trilean;
package Autobus is

  -- Concept
  -- Several processes connect to a Bus on which they can publish messages.
  -- Each message is dispatched to observers, according to filters that they
  --  can set on the content of the messages.

  -- How to use:
  -- * First, create and initialise a Bus (provide an IPM address or LAN name,
  --    and a port num or name).
  --   If you are Active on the bus, then your presence will be checked
  --    periodically by the other partners.
  --   Then you can send messages (strings) on the Bus.
  --   You can tune this Bus in the XML file indicated in the ENV variable
  --    AUTOBUS_CONFIG (see below).
  -- * Second, create an Observer (with a procedure Receive on it)
  --    and a Subscriber.
  --   Then init the Subscriber, possibly with a filter (regular expression) on
  --     the content of the expected messages.
  --    The procedure Receive will be called with the messages (received on the
  --     Bus) that match the filter.

  -- Implementation:
  -- There are two kinds of Bus, Multicast and Reliable:
  -- A. Reliable
  -- Each Bus relies on a fixed IPM address and port, and a random TCP port.
  -- Each process sends periodically in IPM a live message containing
  --  its host and local TCP port.
  -- The other processes on the bus keep a list of known alive partners.
  -- When a new process starts it declares itself on the Bus and all the
  --  partners either connect to it or get connected to it.
  -- Delivering a message consists in sending it in TCP successively to the all
  --  the partners, each of them dispatching the message to the local observers.
  -- Each partner on a reliable bus can be
  --  * either active: it periodically sends a live message, so that the other
  --    partner can detect its death in any circumstance with a watchdog,
  --  * or passive: its sudden death can only be detected by the closure of the
  --    TCP connection.
  --  Active mode can be useful for servers, while passive mode might be more
  --  convenient for clients.
  -- B. Multicast
  -- Each message is sent in multicast (IPM) to the partners and
  --  dispatched to the observers. Some messages might be lost in the network.

  -- Bus kinds can be mixed. In one process, a Bus is either Reliable (either
  --  active or passive) or Multicast. But different processes can communicate
  --  through Buses of different kinds, providing of course that they have the
  --  same address.
  --  - A Reliable publisher sends in TCP while in Multicast it sends in IPM,
  --  - A Reliable receiver receives Reliable and Multicast messages,
  --  - A Multicast receiver receives the live messages of the Reliable
  --    publishers, connects with them, then it handles them as passive,
  --  - Point to point sending (operation Send_To) follows the same logic:
  --    * if either the local or remote Bus is Reliable then the Host-Port must
  --      denote the known Reliable patner
  --    * if both Buses are Multicast, then the Host-Port is used to send
  --      the message in point to point UDP to the Host,
  -- This way, any message sent through or to a Reliable bus is reliable, and
  --  any message sent through and to a Multicast bus is multicast.

  -- Tuning the Bus:
  -- A XML file allows the default tuning for all the Buses, and also a specific
  --  tuning for each Bus. The path of this file must be set in the environment
  --  variable AUTOBUS_CONFIG.
  -- For the default values and also for each individual reliable Bus:
  -- - Heartbeat_Period is the period in seconds at which each process on
  --    the (reliaable active) Bus sends the alive message. It is used in
  --    combination with Heartbeat_Max_Missed. Default 1.
  -- - Heartbeat_Max_Missed, the number of missing alive messages after which
  --    the partners consider that a a process is dead (or at least unreachable)
  --    and discard it from the list of partners. Default 3.
  -- - Timeout for connecting and for sending each TCP message. When it fails
  --    the corresponding partner is discarded. Default 0.5.
  --    Note that the Timeout applies to each attempt of TCP connection to
  --    a reliable active partner. After each timeout there is a new attempt...
  --    likely until the partner is finally discarded because of alive timeout.
  -- - TTL for both IPM and TCP exchanges. Default 5.
  -- - Passive_Factor for the number of Heartbeats after which passive partners
  --    send an alive message (in case the initial alive message would be lost.
  --    Default 10.
  -- For each Bus, two ways to set a specific network interface for IPM and TCP:
  -- - Alias defines the IP address of the interface to be used by a given host.
  --    Ex: Name="telemaque" Address="192.168.0.5" means: if your local host
  --    name is telemaque then use the local interface with address 192.168.0.5
  -- - LAN defines the IP address and netmask of a LAN to use, if locally
  --    connected to the local host. Ex: Address="10.100.12.0"
  --    Netmask="255.255.255.0" means: if one of the local interfaces is
  --    10.100.12.*, then use this interface. A LAN directive will apply to all
  --    the hosts on the LAN, except for those which are previously defined
  --    in an Alias.
  -- Aliases and LANs are tested in the order of declaration and the first that
  --  matches is selected. If none matches, then the Bus listens to, and sends
  --  the IPM messages on the interface associated to the local host name.
  -- See the DTD Autobus.dtd for the format of the XML file.

  -- Note that the exclusion of an active partner (either because it informs
  --  that it dyes, or because of a timeout of alive message, or on timeout
  --  while connecting or sending) is not definitive. The partner is re-inserted
  --  when it is reachable again (i.e. when we receive an alive message from
  --  it). It only misses the applicative messages that were sent meanwhile.

  -------------
  -- The Bus --
  -------------
  type Bus_Type is tagged limited private;
  type Bus_Access_Type is access all Bus_Type;

  -- Supervision callback on the Bus
  -- Report the insertion of a remote reliable partner (State=True),
  --  the death of a remote reliable parner (State=False) and our own address
  --  (State=Other) on a reliable or multicast bus
  -- Address has the form "host_addr:port_num/mode"
  -- In Sup_Callback it is forbidden to initialise or reset a Bus or a
  --  Subscriber, this would raise the exception In_Callback
  -- Exceptions raised by Sup_Callback are caught and hidden
  type Sup_Report is record
    Addr  : As.U.Asu_Us;
    State : Trilean.Trilean;
  end record;
  type Sup_Callback is access procedure (Report : in Sup_Report);

  -- Initialise a Bus
  -- We are either active on a reliable Bus, passive on a reliable Bus
  --  or the bus is multicast
  type Bus_Kind is (Active, Passive, Multicast);
  -- May raise:
  -- On incorrect format (not <lan>:<port>, invalid LAN or port)
  Invalid_Address : exception;
  -- On LAN or port name not found (DNS, networks, services)
  Name_Error : exception;
  -- If a bus is already init with this Address
  Address_In_Use : exception;
  -- On error in the tuning configuration file (parsed at Init of first Bus)
  -- See Autobus.dtd for the format of this file
  Config_Error : exception;
  -- The Sup_Cb does not report any Multicast bus address and state
  --  (neither partners nor ourself)
  procedure Init (Bus : in out Bus_Type;
                  Address : in String;
                  Kind : in Bus_Kind := Active;
                  Sup_Cb : in Sup_Callback := null);

  -- Is a Bus initialised
  function Is_Init (Bus : Bus_Type) return Boolean;

  -- Reset a Bus (make it re-usable)
  procedure Reset (Bus : in out Bus_Type);

  -- Send a Message on a Bus, may raise:
  -- On message longer than Message_Max_Length (1MB)
  Message_Max_Length : constant := 1024 * 1024;
  Message_Too_Long : exception;
  -- Empty message
  Empty_Message : exception;
  procedure Send (Bus : in out Bus_Type; Message : in String);

  -- Reply to the message currently being received
  -- Note that if two processes use the same multicast bus on the same node
  --  they both receive the reply to a message sent by one of them
  -- If not in receive
  Not_In_Receive : exception;
  -- May also raise Empty_Message
  procedure Reply (Bus : in out Bus_Type; Message : in String);

  -- Mode definition (to identify a destination) and image
  type Mode_List is (Reliable, Multicast);
  -- Returns "/R" or "/M"
  function Mode_Suffix (Mode : in Mode_List) return String;

  -- Send a message to one process
  -- Dest is designated by a string "host_addr:port_num/mode"
  --  or "host_name:port_name/mode" or any combination,where mode is 'R' for
  --  reliable or 'M' for multicast
  -- If names do not resolve or destination is not known
  Unknown_Destination : exception;
  -- If sending Muticast message through a Reliable Bus
  Uncompatible_Mode : exception;
  -- May also raise Empty_Message or Invalid_Address
  procedure Send_To (Bus : in out Bus_Type;
                     Host_Port_Mode : in String;
                     Message : in String);

  -- Dest is designated by a Host_Id, a Port_Num and a mode
  procedure Send_To (Bus : in out Bus_Type;
                     Host : in Socket.Host_Id;
                     Port : in Socket.Port_Num;
                     Mode : in Mode_List;
                     Message : in String);


  --------------------
  -- The Subscriber --
  --------------------
  type Subscriber_Type is tagged limited private;
  type Subscriber_Access_Type is access all Subscriber_Type;

  -- The Observer is notified with the messages (sent on the Bus)
  --  that pass the filter
  -- Filter is a PCRE multi-line regular expression
  -- Empty filter lets all messages pass through
  -- Echo allows enabling observation of messages sent by own process
  -- In Receive it is forbidden to initialise or reset a Bus or a Subscriber,
  --  this would raise the exception In_Callback
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
  --  or in Sup_Callback
  In_Callback : exception;

  -- On any unexpected system error on any call
  System_Error : exception;

private

  -- List of partners
  type Bus_Rec;
  type Bus_Access is access all Bus_Rec;
  type Timer_Access is access all Chronos.Passive_Timers.Passive_Timer;
  type Partner_State_List is (Init, Active, Passive, Multicast, Shadow);
  subtype Init_Sate_List is Partner_State_List range Active .. Passive;
  type Partner_Rec is record
    -- Address of the TCP socket "www.xxx.yyy.zzz:portnum/mode"
    Addr : As.U.Asu_Us;
    -- Low level address
    Host : Socket.Host_Id;
    Port : Socket.Port_Num;
    Mode : Mode_List;
    -- Socket
    Sock : Socket.Socket_Dscr;
    -- State of the partner while waiting for connection completion
    Init_State: Init_Sate_List;
    -- State of the connection
    State : Partner_State_List := Init;
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
  type Filter_Access is access Reg_Exp.Compiled_Pattern;
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
  --************************************************************************
  --* Don't forget to update the body of Set when adding fields in Bus_Rec *
  --************************************************************************
  type Bus_Rec is limited new Ada.Finalization.Limited_Controlled with record
    -- Address of the IPM socket "www.xxx.yyy.zzz:portnum", for reporting
    Name : As.U.Asu_Us;
    -- Address of the TCP (Acc) or the UDP socket on dynamic port
    --  "www.xxx.yyy.zzz:portnum/mode" (Mode is 'R' or 'M')
    -- Unique identifier
    Addr : As.U.Asu_Us;
    -- The IP and port part of Addr
    Ipaddr : As.U.Asu_Us;
    -- Administration or multicast IPM socket
    Adm : Socket.Socket_Dscr := Socket.No_Socket;
    Host : Socket.Host_Id;
    Port : Socket.Port_Num;
    -- TCP accept socket, dynamic port => Unique for this process/node
    --  (in multicast, it is bound to a dynamic UDP port)
    Acc : Socket.Socket_Dscr := Socket.No_Socket;
    -- Host Id denoting the interface (for TCP and IPM)
    Host_If : Socket.Host_Id;
    -- Supervision callback
    Sup_Cb : Sup_Callback;
    -- Are we active or passive or multicasting on this bus
    Kind : Bus_Kind := Active;
    -- Heartbeat period and Max missed number, Timeout on connect and send,
    --  TTL and passive_Factor
    Heartbeat_Period : Duration := 1.0;
    Heartbeat_Max_Missed : Positive := 3;
    Timeout : Duration := 0.5;
    Ttl : Socket.Ttl_Range := 5;
    Passive_Factor : Positive := 10;
    -- List of access to partners (TCP connections)
    Partners : Partner_Access_List_Mng.List_Type;
    -- List of subscribers
    Subscribers : Subscriber_List_Mng.List_Type;
    -- Timer for heartbeat sending and checking
    Heartbeat_Timer : Timers.Timer_Id;
    -- Passive timer for heartbeat sending when pasive
    Passive_Timer : Timer_Access;
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

