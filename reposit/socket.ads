with System;
with X_Mng;
package Socket is

  -- The socket descriptor
  type Socket_Dscr is private;
  No_Socket : constant Socket_Dscr;

  -- Available protocols
  type Protocol_List is (Udp, Tcp, Tcp_Header);

  -- Any problem? Look at the traces
  Socket_Error : exception;

  -- A port
  type Port_Num is new Natural range 0 .. 65535;

  -- A host
  type Host_Id is private;
  No_Host : constant Host_Id;

  -- Exceptions for errors
  Soc_Use_Err,   -- Socket should be open or not open
  Soc_Sys_Err,   -- System error (traced) and errno set
  Soc_Dest_Err,  -- Destination should be set or not set
  Soc_Link_Err,  -- Socket should be linked or not linked
  Soc_Conn_Err,  -- Socket should be connected or not connected
                 --  or is already connecting
  Soc_Bcast_Err, -- Broadcast not allowed for this protocol
  Soc_Len_Err,   -- Lenght (message or name) too short
  Soc_Reply_Err, -- Set_for_reply must not be set
  Soc_Tail_Err,  -- Sent mesg len is not 0 while prev send raised Soc_Woul_Block
  Soc_Proto_Err: -- Call not allowed for this protocol
                 exception;

  -- Exceptions for failures
  Soc_Conn_Refused,   -- Connection refused (no dest process)
  Soc_Name_Not_Found, -- Host/service name not found
  Soc_Would_Block,    -- Connection, send, receive would block
  Soc_Conn_Lost:      -- Connection has been lost
                 exception;

  -- All calls may raise Soc_Use_Err or Soc_Sys_Err


  -- When a socket is non  blocking the calls which may raise Soc_Would_Block
  --   and what to do are:
  -- Receive, then wait for read on fd and re-Read with same arguments
  -- Send, then wait for write on fd and call Re_Send until OK
  -- Set_Destination on tcp (connect) then wait for write on fd
  --  then check result by calling Is_Connected
  ---------------------------------
  -- OPEN - CLOSE - SET BLOCKING --
  -- Open a socket (in blocking mode)
  ---------------------------------

  -- Open a socket
  procedure Open (Socket : in out Socket_Dscr;  Protocol : in Protocol_List);

  -- Close a socket
  procedure Close (Socket : in out Socket_Dscr);

  -- Is a socket open
  function Is_Open (Socket : in Socket_Dscr) return Boolean;

  -- Set a socket blocking or not
  procedure Set_Blocking (Socket : in Socket_Dscr);

  -- Get the Fd of a socket (for use in X_Mng. Add/Del _Callback) 
  function Fd_Of (Socket : in Socket_Dscr) return X_Mng.File_Desc;

  -------------------------------------
  -- RECEPTION PORT - FD - RECEPTION --
  -------------------------------------

  -- Bind for reception or connection accepting,
  --  On a port from services, on a port by num
  --  or a dynamical (ephemeral - attributed by the OS) port
  -- May raise Soc_Link_Err or Soc_Con_Err if tcp and already
  --  linked or connected
  -- May raise Soc_Name_Not_Found if Service not found
  procedure Link_Service (Socket : in Socket_Dscr; Service  : in String);
  procedure Link_Port    (Socket : in Socket_Dscr; Port  : in Port_Num);
  procedure Link_Dynamic (Socket : in Socket_Dscr);

  -- Get port num to which socket is linked
  -- May raise Soc_Link_Err if not linked
  function Get_Linked_To (Socket : in Socket_Dscr) return Port_Num;

  -- Accept a new Tcp connection
  -- The socket must be open, tcp and linked
  -- A new socket is open (tcp) with destination set
  -- May raise Soc_Proto_Err if socket is not tcp
  -- May raise Soc_Link_Err if socket is not linked
  procedure Accept_Connection (Socket : in Socket_Dscr;
                               New_socket : in out Socket_Dscr);

  -- Receive a message, waiting for it
  -- The socket destination may be set for a reply if not tcp
  -- May raise Soc_Reply_err if Set_For_Reply and tcp
  -- May raise Soc_Link_Err if socket is linked and tcp
  --                     or if socket is not linked and udp
  -- May raise Soc_Conn_err if socket is not connected and tcp
  -- May raise Soc_Conn_Lost if sender has closed (even in udp)
  -- May raise Soc_Would_Block if the full sent message was not
  --  received, in tcp_header and udp
  -- May raise Soc_Len_Err if message size is to short in tcp_header
  generic
    type Message_Type is private;
  procedure Receive (Socket        : in Socket_Dscr;
                     Message       : out Message_Type;
                     Length        : out Natural;
                     Set_For_Reply : in Boolean := False);


  -------------------------------------
  -- DESTINATION PORT/HOST - SENDING --
  -------------------------------------

  -- Set destination (Host/Lan and port) for sending
  -- If Lan is true then Name is a LAN name to broadcast on
  -- Otherwise it is a host name
  -- Connects if tcp.
  -- May raise: Soc_Conn_Refused, Soc_Would_Block, Soc_Conn_Err if tcp
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

  function Is_Connected (Socket : Socket_Dscr) return Boolean;

  -- Change destination Host/Lan or port
  -- Must be Udp and have been set by either
  -- * Set Destination
  -- * Receive (Set_To_Reply => True)
  -- May raise Soc_Proto_Err if socket is tcp
  -- May raise Soc_Dest_Err is destination is not already set
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
  -- * Accept in Tcp
  -- May raise Soc_Dest_Err is destination is not already set
  function Get_Destination_Host (Socket : Socket_Dscr) return Host_Id;
  function Get_Destination_Port (Socket : Socket_Dscr) return Port_Num;

  -- Convert Host_Id to Host_Name and reverse (not for LANs)
  -- May raise Soc_Name_Not_Found if Name is not found
  function Host_Name_Of (Id : Host_Id) return String;
  function Host_Id_Of   (Name : String) return Host_Id;
  
  -- Send a message
  -- If Length is 0 then the full size of Message_Type is sent
  -- May raise Soc_Dest_Err if destination is not set
  -- May raise Soc_Conn_Err if tcp and not connected
  -- May raise Soc_Tail_Err if non blocking and previous send
  --   had raised Soc_Would_Block (Re_Send should have been used)
  -- May send Soc_Conn_Lost if destination has closed
  -- May Soc_Would_Block if non blocking and overflow (call Re_Send)
  generic
    type Message_Type is private;
  procedure Send (Socket  : in Socket_Dscr;
                  Message : in Message_Type;
                  Length  : in Natural := 0);

  -- Try to send remaining of message after a Soc_Would_Block
  --  on Send or Re_Send
  -- May raise Soc_Tail_Err if previous send or Re_Send succeeded
  -- May raise same exceptions as Send
  procedure Re_Send (Socket  : in Socket_Dscr);

private

  type Socket_Dscr is record
    Soc_Addr : System.Address := System.Null_Address;
  end record;

  No_Socket : constant Socket_Dscr := (Soc_Addr => System.Null_Address);

  type Host_Id is new Natural;
  No_Host : constant Host_Id := 0;

end Socket;

