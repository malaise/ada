-- UDP, IPM and TCP socket binding
with System;
with Sys_Calls, C_Types;
package Socket is

  -- A socket descriptor
  type Socket_Dscr is tagged private;
  No_Socket : constant Socket_Dscr;

  -- Available protocols on socket, UDP then TCP
  -- Udp is UDP and IPM
  -- Tcp is flows of bytes
  -- Tcp_Header implements messages over TCP
  -- Afux is TCP connection within the same host
  type Protocol_List is (Udp, Tcp, Tcp_Header, Tcp_Afux, Tcp_Header_Afux);
  subtype Tcp_Protocol_List is Protocol_List range Tcp .. Tcp_Header_Afux;

  -- Note for Multicast IP (using Udp socket):
  -- For sending IPM, simply Set_Destination to a LAN name
  --  which is defined with a D class address, and a port.
  --  It is possible to specify a sending IPM interface.
  -- For receiving IPM, first Set_Destination to the LAN name and port,
  --  then Link to the same port as this destination.
  --  It is possible to link dynamically to port (then the destination port
  --  that was set is not used).
  --  It is possible to specify (before linking to port) the receiving network
  --  interface. This is more efficient because otherwise the multicast address
  --  is enabled on all the interfaces that are up and multicast-capable.

  -- A port
  type Port_Num is new C_Types.Uint16 range 0 .. C_Types.Uint16'Last;

  -- A host
  type Host_Id is private;
  Any_Host : constant Host_Id;

  -- The blocking / non-blocking modes
  --  Blocking emission and reception
  --  Blocking only connection and sending
  --  Non blocking connection, emission and reception
  type Blocking_List is (Full_Blocking, Blocking_Send, Non_Blocking);

  -- The TTL, for TCP non Afux (default 64) and UDP/IPM (default 32)
  subtype Ttl_Range is Natural range 0 .. 255;

  -- Exceptions for errors
  -- Errors are consequences of bad usage of the socket
  -- All the calls may raise Soc_Use_Err or Soc_Sys_Err
  Soc_Use_Err,   -- Socket should be open or not open
  Soc_Sys_Err,   -- System error (traced) and errno set (see Sys_Calls)
  Soc_Dest_Err,  -- Destination should be set or is already set
  Soc_Link_Err,  -- Socket should be linked or is already linked
  Soc_Conn_Err,  -- Socket should be connected or is already connected
                 --  or is already connecting
  Soc_Bcast_Err, -- Broadcast not allowed for this protocol
  Soc_Len_Err,   -- Length too short (for message or name)
  Soc_Reply_Err, -- Set_For_Reply must not be set
  Soc_Tail_Err,  -- Sent msg len is not 0 while prev send raised Soc_Woul_Block
  Soc_Proto_Err, -- Call not allowed for this protocol
  Soc_Fd_In_Use: -- Close while fd is used by x_select (see Get_Fd and
                 --  Event_Mng Add/Del_Fd_Callback)
                 exception;

  -- Exceptions for failures
  -- Failures are (transient) consequences of external factors
  Soc_Conn_Refused,   -- Connection refused (no dest process linked)
  Soc_Name_Not_Found, -- Host/lan/service name not found
  Soc_Would_Block,    -- Connection, send or receive would block
  Soc_Conn_Lost,      -- Connection has been lost
  Soc_Addr_In_Use,    -- Address in use, maybe in close-wait state
  Soc_Read_0,         -- Read returns 0, after a select => disconnection
  Soc_Reply_Iface:    -- Set_For_Reply could not set the IPM sending interface
                 exception;

  -- The following list describes which call may raise Soc_Would_Block on a
  --  non blocking socket and what the application should do it this case:
  -- Receive: wait for read on fd and re-call Read with the same arguments,
  -- Send: wait for write on fd and call Re_Send, until Ok,
  -- Set_Destination (connect tcp): wait for write on fd then check result
  --   by calling Is_Connected.
  -- Accept_Connection: go back to wait for read on fd

  ---------------------------------
  -- OPEN - CLOSE - SET BLOCKING --
  ---------------------------------

  -- Open a socket (in Full_Blocking mode)
  procedure Open (Socket : in out Socket_Dscr; Protocol : in Protocol_List);

  -- Close a socket
  -- May raise Soc_In_Use if Fd is used (in Event_Mng)
  procedure Close (Socket : in out Socket_Dscr);

  -- Is a socket open
  function Is_Open (Socket : in Socket_Dscr) return Boolean;

  -- Image of a socket (for hashing by client)
  -- May raise Soc_Use_Err if Socket is not open
  function Image (Socket : in Socket_Dscr) return String;

  -- Set the socket blocking or non blocking
  --  (for sending, receiving, connecting)
  -- Socket is Full_Blocking at creation (open/accept)
  procedure Set_Blocking (Socket : in Socket_Dscr; Blocking : in Blocking_List);

  -- Get the the socket blocking or non blocking mode
  function Get_Blocking (Socket : in Socket_Dscr) return Blocking_List;

  -- Is a socket in blocking mode in emission or reception
  function Is_Blocking (Socket : in Socket_Dscr; Emission : in Boolean)
                       return Boolean;

  -- Set the TTL of a socket
  -- May raise Soc_Proto_Err if socket is Tcp_(Header_)Afux
  procedure Set_Ttl (Socket : in Socket_Dscr; Ttl : in Ttl_Range);

  -- Get the TTL of a socket
  -- May raise Soc_Proto_Err if socket is Tcp_(Header_)Afux
  function Get_Ttl (Socket : Socket_Dscr) return Ttl_Range;

  -- Get the Fd of a socket (for use in Event_Mng. Add/Del _Callback)
  function Get_Fd (Socket : in Socket_Dscr) return Sys_Calls.File_Desc;

  -- Get the protocol of a socket
  function Get_Protocol (Socket : in Socket_Dscr) return Protocol_List;

  -----------------------------
  -- LINK - ACCEPT - RECEIVE --
  -----------------------------

  -- Set the interface on which to link.
  -- For Tcp not Afux and for Udp (including Ipm)
  -- To be set before linking.
  -- May raise Soc_Proto_Err if Afux
  procedure Set_Reception_Interface (Socket : in Socket_Dscr;
                                     Host   : in Host_Id);
  -- Bind for reception or connection accepting,
  --  On a port from services, on a port by num
  --  or a dynamical (ephemeral - attributed by the OS) port
  -- May raise Soc_Link_Err or Soc_Con_Err if tcp and already
  --  linked or connected
  -- May raise Soc_Name_Not_Found if Service not found
  -- May raise Soc_Addr_In_Use (socket may be in Close-Wait) wait a bit
  procedure Link_Service (Socket : in Socket_Dscr; Service  : in String);
  procedure Link_Port    (Socket : in Socket_Dscr; Port  : in Port_Num);
  -- Link_Dynamic may raise Soc_Proto_Err if socket is afux
  procedure Link_Dynamic (Socket : in Socket_Dscr);

  -- Get port num to which socket is linked
  -- May raise Soc_Link_Err if not linked
  function Get_Linked_To (Socket : in Socket_Dscr) return Port_Num;

  -- Accept a new Tcp connection
  -- The socket must be open, tcp and linked
  -- A new socket is open (tcp) with destination set, Full_Blocking
  -- May raise Soc_Proto_Err if socket is not tcp
  -- May raise Soc_Link_Err if socket is not linked
  -- May raise Soc_Would_Block if no more valid connection to accept
  procedure Accept_Connection (Socket : in Socket_Dscr;
                               New_Socket : in out Socket_Dscr);

  -- Receive a message, waiting for it
  -- The socket destination may be set for a reply if not tcp
  -- May raise Soc_Reply_Err if Set_For_Reply and tcp
  -- May raise Soc_Link_Err if socket is linked and tcp
  --                     or if socket is not linked and udp
  -- May raise Soc_Conn_err if socket is not connected and tcp
  -- May raise Soc_Conn_Lost if sender has closed (even in udp)
  -- May raise Soc_Read_0 if sender has closed
  -- May raise Soc_Would_Block if the full sent message was not
  --  received, in tcp_header and udp
  -- May raise Soc_Len_Err if message size is to short in tcp_header
  generic
    -- If Message_Size (in bits) is 0, the length is computed from
    --  Message_Type'Size, which may be wrong with indefinite type.
    type Message_Type (<>) is private;
    Message_Size : Natural := 0;
  procedure Receive (Socket        : in Socket_Dscr;
                     Message       : out Message_Type;
                     Length        : out Natural;
                     Set_For_Reply : in Boolean := False);

  ----------------------------------
  -- DESTINATION PORT/HOST - SEND --
  ----------------------------------

  -- Set the interface on which send mutlicast IP (udp_socket).
  -- To be set before setting destination.
  -- Set 0 to select back the "default" interface.
  -- Beware that setting the sending interface is not always supported
  --  and may require to be root (Set_Destination will raise Soc_Sys_Err).
  -- May raise Soc_Proto_Err if not udp
  procedure Set_Sending_Ipm_Interface (Socket : in Socket_Dscr;
                                       Host   : in Host_Id);

  -- Before setting the destination of a TCP inet socket,
  --  define the sending port
  -- May raise Soc_Proto_Error if socket is not TCP inet (Tcp or Tcp_Header)
  -- May raise Soc_Link_Err if socket is linked and tcp
  -- May raise Soc_Dest_Err if destination is already set
  -- May raise Soc_Bind_Err if socket is alreay bound
  -- May raise Soc_Name_Not_Found if Service is not found
  -- May raise Soc_Addr_In_Use if port is alreay used
  procedure Bind_Service (Socket  : in Socket_Dscr;
                          Service : in String);
  procedure Bind_Port (Socket  : in Socket_Dscr;
                       Port   : in Port_Num);
  -- Get bound port
  -- May raise Soc_Bind_Err if socket is not bound
  function Get_Bound_Port (Socket : Socket_Dscr) return Port_Num;

  -- Set destination (Host/Lan and port) for sending
  -- If Lan is true then Name is a LAN name to broadcast on
  -- Otherwise it is a host name
  -- Connects if tcp.
  -- May raise Soc_Conn_Refused, Soc_Would_Block, Soc_Conn_Err if tcp
  -- May raise Soc_Name_Not_Found if Name or Service is not found
  -- May raise Soc_Link_Err if socket is tcp and linked
  -- May raise Soc_Conn_Err if socket is tcp and already connected
  -- May raise Soc_Bcast_Err if socket is tcp and Lan
  procedure Set_Destination_Name_And_Service (
               Socket  : in Socket_Dscr;
               Lan     : in Boolean;
               Name    : in String;
               Service : in String);
  procedure Set_Destination_Host_And_Service (
               Socket  : in Socket_Dscr;
               Host    : in Host_Id;
               Service : in String);
  procedure Set_Destination_Name_And_Port (
               Socket : in Socket_Dscr;
               Lan    : in Boolean;
               Name   : in String;
               Port   : in Port_Num);
  procedure Set_Destination_Host_And_Port (
               Socket : in Socket_Dscr;
               Host   : in Host_Id;
               Port   : in Port_Num);

  -- Check the connection status, especially after an asynchronous
  --  connect (Set_Destination on a non Blocking Tcp socket)
  function Is_Connected (Socket : Socket_Dscr) return Boolean;

  -- Change destination Host/Lan or port
  -- Must be Udp and have been set by either
  -- * Set Destination
  -- * Receive (Set_To_Reply => True)
  -- May raise Soc_Proto_Err if socket is tcp
  -- May raise Soc_Dest_Err if destination is not already set
  -- May raise Soc_Name_Not_Found if Name or Service is not found
  procedure Change_Destination_Name (
               Socket : in Socket_Dscr;
               Lan    : in Boolean;
               Name   : in String);
  procedure Change_Destination_Host (
               Socket   : in Socket_Dscr;
               Host     : in Host_Id);

  procedure Change_Destination_Service (
               Socket : in Socket_Dscr;
               Service  : in String);
  procedure Change_Destination_Port (
               Socket : in Socket_Dscr;
               Port  : in Port_Num);

  -- Get current destination of a socket
  -- Must have been set by either
  -- * Set Destination
  -- * Receive (Set_To_Reply => True) in Udp
  -- * Accept_Connection in Tcp
  -- May raise Soc_Dest_Err is destination is not already set
  function Get_Destination_Host (Socket : Socket_Dscr) return Host_Id;
  function Get_Destination_Port (Socket : Socket_Dscr) return Port_Num;

  -- Send a message
  -- If Length is 0 then the full size of Message_Type is sent
  -- May raise Soc_Dest_Err if destination is not set
  -- May raise Soc_Conn_Err if tcp and not connected
  -- May raise Soc_Tail_Err if non blocking and previous send
  --   had raised Soc_Would_Block (Re_Send should have been used)
  -- May raise Soc_Conn_Lost if destination has closed
  -- May raise Soc_Would_Block if non blocking and overflow (call Re_Send)
  generic
    -- If Message_Size (in bits) is 0, the length (if 0) is computed from
    --  Message_Type'Size, which may be wrong with indefinite type.
    type Message_Type (<>) is private;
    Message_Size : Natural := 0;
  procedure Send (Socket  : in Socket_Dscr;
                  Message : in Message_Type;
                  Length  : in Natural := 0);

  -- Try to send remaining of message after a Soc_Would_Block
  --  on Send or Re_Send
  -- May raise Soc_Tail_Err if previous send or Re_Send succeeded
  -- May raise same exceptions as Send
  procedure Re_Send (Socket  : in Socket_Dscr);

  ---------------
  -- UTILITIES --
  ---------------

  -- Name <-> Num
  ---------------
  -- Convert Port_Num to Port_Name and reverse (for a given protocol)
  -- May raise Soc_Name_Not_Found if Name/Num is not found
  function Port_Name_Of (Port : Port_Num; Protocol : Protocol_List)
                        return String;
  function Port_Num_Of  (Name : String; Protocol : Protocol_List)
                        return Port_Num;

  -- Convert Id to Host Name and reverse (not for LANs)
  -- May raise Soc_Name_Not_Found if Name is not found
  function Host_Name_Of (Id : Host_Id) return String;
  function Host_Id_Of   (Name : String) return Host_Id;

  -- Convert Id to LAN Name and reverse (not for hosts)
  -- May raise Soc_Name_Not_Found if Name is not found
  function Lan_Name_Of (Id : Host_Id) return String;
  function Lan_Id_Of   (Name : String) return Host_Id;

  -- Host_Id <-> 4 bytes of Ip address
  subtype Byte is C_Types.Byte;
  type Ip_Address is record
    A, B, C, D : Byte; -- Network (natural) order
  end record with Size => 4 * System.Storage_Unit;

  function Id2Addr (Id : Host_Id) return Ip_Address;
  function Addr2Id (Addr : Ip_Address) return Host_Id;

  -- Local host interfaces
  ------------------------
  -- Get local Host name and corresponding id
  function Local_Host_Name return String;
  function Local_Host_Id return Host_Id;

  -- Get local LAN name (from local host) and corresponding id
  function Local_Lan_Name return String;
  function Local_Lan_Id return Host_Id;

  -- Get the broadcast address for a given local interface (designated by If_Id)
  function Local_Bcast_Of (If_Id : Host_Id) return Host_Id;

  -- Get the id of local Host on a given LAN and netmask
  function Local_Host_Id_For (Lan, Netmask : Host_Id) return Host_Id;
private

  type Socket_Dscr is tagged record
    Soc_Addr : System.Address := System.Null_Address;
  end record;

  type Host_Id is new C_Types.Uint32
    with Size => 4 * System.Storage_Unit; -- As Ip_Addr
  Any_Host : constant Host_Id := 0;

  No_Socket : constant Socket_Dscr := (Soc_Addr => System.Null_Address);
end Socket;

