-- Simple API for reliable message passing
with Ada.Finalization;
with Socket, Regular_Expressions, As.U,
     Dynamic_List, Limited_List,
     Timers, Chronos.Passive_Timers;
package Autobus is

  -------------
  -- The Bus --
  -------------
  type Bus_Type is tagged limited private;
  type Bus_Access_Type is access all Bus_Type;

  -- Initialise a Bus, may raise:
  -- On incorrect format (not <lan>:<port>, invalud LAN or port num)
  Invalid_Address : exception;
  -- On LAN or port name not found (DNS, networks, services)
  Name_Error : exception;
  procedure Init (Bus : in out Bus_Type;
                  Address : in String);

  -- Reset a Bus (make it re-usable)
  procedure Reset (Bus : in out Bus_Type);

  -- Send a Message on a Bus
  -- On message longer than 1MB
  Message_Too_Long : exception;
  procedure Send (Bus : in out Bus_Type; Message : in String);

  -------------------
  -- The Subsciber --
  -------------------
  type Subscriber_Type is tagged limited private;

  -- The Observer is notified with the Messages (sent on the Bus)
  --  that pass the Filter
  -- Filter is a PCRE regular expression
  -- Empty filter lets all messages pass through
  type Observer_Type is limited interface;
  procedure Bus_Receive (Observer : in out Observer_Type;
                         Message : in String) is abstract;

  -- Initialise a Subscriber on a Bus, may raise
  -- On incorrect filter expression
  Invalid_Filter : exception;
  procedure Init (Subscriber : in out Subscriber_Type;
                  Bus : Bus_Access_Type;
                  Filter : in String;
                  Observer : access Observer_Type'Class);

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
    -- Reference to the bus (when message recieved or diconnection)
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
  type Subscriber_Access_Type is access all Subscriber_Type;
  type Subscriber_Rec is new Ada.Finalization.Controlled with record
    -- Bus
    Bus : Bus_Access;
    -- Filter
    Filter : Filter_Access;
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

