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


  -------------------------------------
  -- RECEPTION PORT - FD - RECEPTION --
  -------------------------------------

  -- Bind for reception or connection accepting,
  --  On a port from services, on a port by num
  --  or a dynamical (ephemeral - attributed by the OS) port
  procedure Link_Service (Socket : in Socket_Dscr; Service  : in String);
  procedure Link_Port    (Socket : in Socket_Dscr; Port  : in Port_Num);
  procedure Link_Dynamic (Socket : in Socket_Dscr);

  -- Get port num to which socket is linked
  function Get_Linked_To (Socket : in Socket_Dscr) return Port_Num;

  -- Get the Fd of a socket (for use in X_Mng. Add/Del _Callback) 
  function Fd_Of (Socket : in Socket_Dscr) return X_Mng.File_Desc;

  -- Accept a new Tcp connection.
  -- The socket must be open, tcp and linked.
  --  A new socket is created (tcp) with destination set
  procedure Accept_Connection (Socket : in Socket_Dscr;
                               New_socket : in out Socket_Dscr);

  -- Receive a message, waiting for it
  -- The socket must linked in udp and not linked in tcp
  -- The socket destination may be set for a reply
  -- Received not set means no message can be read (connection refused...)
  -- No set_for_reply if tcp
  generic
    type Message_Type is private;
  procedure Receive (Socket        : in Socket_Dscr;
                     Message       : out Message_Type;
                     Length        : out Natural;
                     Received      : out Boolean;
                     Set_For_Reply : in Boolean := False);


  -------------------------------------
  -- DESTINATION PORT/HOST - SENDING --
  -------------------------------------

  -- Set destination (Host/Lan and port) for sending
  -- If Lan is true then Name is a LAN name to broadcast on
  -- Otherwise it is a host name
  -- No Lan to bradcast if tcp. Connects if tcp.
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

  -- Change destination Host/Lan
  -- Must be Udp and have been set by either
  -- * Set Destination
  -- * Receive (Set_To_Reply => True)
  procedure Change_Destination_Name (
               Socket : in Socket_Dscr;
               Lan    : in Boolean;
               Name   : in String);
  procedure Change_Destination_Host (
               Socket   : in Socket_Dscr;
               Host     : in Host_Id);

  -- Change port
  -- Must be Udp and have been set by either
  -- * Set Destination
  -- * Receive (Set_To_Reply => True)
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
  function Get_Destination_Host (Socket : Socket_Dscr) return Host_Id;
  function Get_Destination_Port (Socket : Socket_Dscr) return Port_Num;

  -- Convert Host_Id to Host_Name and reverse (not for LANs)
  function Host_Name_Of (Id : Host_Id) return String;
  function Host_Id_Of   (Name : String) return Host_Id;
  
  -- Send a message
  -- If Length is 0 then the full size of Message_Type is sent
  generic
    type Message_Type is private;
  procedure Send (Socket  : in Socket_Dscr;
                  Message : in Message_Type;
                  Length  : in Natural := 0);

private

  type Socket_Dscr is record
    Soc_Addr : System.Address := System.Null_Address;
  end record;

  No_Socket : constant Socket_Dscr := (Soc_Addr => System.Null_Address);

  type Host_Id is new Natural;
  No_Host : constant Host_Id := 0;

end Socket;

