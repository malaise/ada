package body Socket is

  ----------------
  -- INTERFACES --
  ----------------
  Byte_Size : constant := 8;

  subtype Result is Integer;
  OK : constant Result := 0;

  type Boolean_For_C is new Boolean;
  for Boolean_For_C'Size use 4 * Byte_Size;

  type Word is new Integer range 0 .. Integer(Port_Num'Last);
  for Word'Size use 2 * Byte_Size;

  function C_Str (Str : String) return String is
  begin
    return Str & Ascii.Nul;
  end C_Str;

  type C_Protocol is new Protocol_List;
  for C_Protocol'Size use 32;

  function Soc_Open (S_Addr : System.Address;
                     Protocol : C_Protocol) return Result;
  pragma Import (C, Soc_Open, "soc_open");
  function Soc_Close (S_Addr : System.Address) return Result;
  pragma Import (C, Soc_Close, "soc_close");

  function Soc_Link_Service (S : System.Address;
                             Service : System.Address) return Result;
  pragma Import (C, Soc_Link_Service, "soc_link_service");
  function Soc_Link_Port (S : System.Address;
                          Port : Word) return Result;
  pragma Import (C, Soc_Link_Port, "soc_link_port");
  function Soc_Link_Dynamic (S : System.Address) return Result;
  pragma Import (C, Soc_Link_Dynamic, "soc_link_dynamic");
  function Soc_Get_Linked_Port (S : System.Address;
                                Port : System.Address) return Result;
  pragma Import (C, Soc_Get_Linked_Port, "soc_get_linked_port");
  function Soc_Get_Id (S : System.Address;
                       Fd : System.Address) return Result;
  pragma Import (C, Soc_Get_Id, "soc_get_id");
  function Soc_Accept (S : System.Address; N :  System.Address) return Result;
  pragma Import (C, Soc_Accept, "soc_accept");
  function Soc_Receive (S : System.Address;
                        P_Received    : System.Address;
                        Message       : System.Address;
                        Length        : System.Address;
                        Set_For_Reply : Boolean_For_C) return Result;
  pragma Import (C, Soc_Receive, "soc_receive");

  function Soc_Set_Dest_Service (S : System.Address;
                                 Host_Lan : System.Address;
                                 Lan      : Boolean_For_C;
                                 Service  : System.Address) return Result;
  pragma Import (C, Soc_Set_Dest_Service, "soc_set_dest_service");
  function Soc_Set_Dest_Port (S : System.Address;
                              Host_Lan : System.Address;
                              Lan      : Boolean_For_C;
                              Port     : Word) return Result;
  pragma Import (C, Soc_Set_Dest_Port, "soc_set_dest_port");
  function Soc_Set_Dest (S : System.Address;
                         Host : Host_Id;
                         Port : Word) return Result;
  pragma Import (C, Soc_Set_Dest, "soc_set_dest");

  function Soc_Is_Connected (S : System.Address;
                             P_Received : System.Address) return Result;
  pragma Import (C, Soc_Is_Connected, "soc_is_connected");

  function Soc_Change_Dest_Host (S : System.Address;
                                 Host_Lan : System.Address;
                                 Lan      : Boolean_For_C) return Result;
  pragma Import (C, Soc_Change_Dest_Host, "soc_change_dest_host");
  function Soc_Change_Dest_Service (S : System.Address;
                                    Service : System.Address) return Result;
  pragma Import (C, Soc_Change_Dest_Service, "soc_change_dest_service");
  function Soc_Change_Dest_Port (S : System.Address;
                                 Port : Word) return Result;
  pragma Import (C, Soc_Change_Dest_Port, "soc_change_dest_port");

  function Soc_Get_Dest_Host (S : System.Address;
                              Host : System.Address) return Result;
  pragma Import (C, Soc_Get_Dest_Host, "soc_get_dest_host");
  function Soc_Get_Dest_Port (S : System.Address;
                              Port : System.Address) return Result;
  pragma Import (C, Soc_Get_Dest_Port, "soc_get_dest_port");


  function Soc_Host_Name_Of (Host : Host_Id; Name : System.Address;
                             Len  : Natural) return Result;
  pragma Import (C, Soc_Host_Name_Of, "soc_host_name_of");
  function Soc_Host_Of (Name : System.Address;
                        Id   : System.Address) return Result;
  pragma Import (C, Soc_Host_Of, "soc_host_of");

  function Soc_Send (S : System.Address;
                     Message : System.Address;
                     Length  : Natural) return Result;
  pragma Import (C, Soc_Send, "soc_send");
                     
  --------------------
  -- IMPLEMENTATION --
  --------------------
  Res : Result;

  procedure Check_Ok is
  begin
    if Res /= OK then
      raise Socket_Error;
    end if;
  end Check_Ok;

  -- Open a socket
  procedure Open (Socket : in out Socket_Dscr;
                  Protocol : in Protocol_List) is
  begin
    Res := Soc_Open (Socket.Soc_Addr'Address,
                     C_Protocol(Protocol) );
    Check_Ok;
  end Open;

  -- Is a socket open
  function Is_Open (Socket : in Socket_Dscr) return Boolean is
    use type System.Address;
  begin
    return Socket.Soc_Addr /= System.Null_Address;
  end Is_Open;


  -- Close a socket
  procedure Close (Socket : in out Socket_Dscr) is
  begin
    Res := Soc_Close (Socket.Soc_Addr'Address);
    Check_Ok;
  end Close;


  -------------------------------------
  -- RECEPTION PORT - FD - RECEPTION --
  -------------------------------------

  -- Bind for reception on a port from services, on a port by num
  --  or a dynamical (ephemeral - attributed by the OS) port
  procedure Link_Service (Socket : in Socket_Dscr; Service  : in String) is
    C_Service : constant String := C_Str (Service);
  begin
    Res := Soc_Link_Service (Socket.Soc_Addr, C_Service'Address);
    Check_Ok;
  end Link_Service;

  procedure Link_Port (Socket : in Socket_Dscr; Port  : in Port_Num) is
  begin
    Res := Soc_Link_Port (Socket.Soc_Addr, Word(Port));
    Check_Ok;
  end Link_Port;

  procedure Link_Dynamic (Socket : in Socket_Dscr) is
  begin
    Res := Soc_Link_Dynamic (Socket.Soc_Addr);
    Check_Ok;
  end Link_Dynamic;

  -- Get port num to which socket is linked
  function Get_Linked_To (Socket : in Socket_Dscr) return Port_Num is
    Port : Word;
  begin
    Res := Soc_Get_Linked_Port (Socket.Soc_Addr, Port'Address);
    Check_Ok;
    return Port_Num(Port);
  end Get_Linked_To;

  procedure Accept_Connection (Socket : in Socket_Dscr;
                               New_Socket : in out Socket_Dscr) is
  begin
    Res := Soc_Accept (Socket.Soc_Addr, New_Socket.Soc_Addr'Address);
    Check_Ok;
  end Accept_Connection;


  -- Get the Fd of a socket (for use in X_Mng. Add/Del _Callback) 
  function Fd_Of (Socket : in Socket_Dscr) return X_Mng.File_Desc is
    Fd : Integer;
  begin
    Res := Soc_Get_Id (Socket.Soc_Addr, Fd'Address);
    Check_Ok;
    return X_Mng.File_Desc(Fd);
  end Fd_Of;

  -- Receive a message, waiting for it
  -- The socket destination may be set for a reply
  procedure Receive (Socket        : in Socket_Dscr;
                     Message       : out Message_Type;
                     Length        : out Natural;
                     Received      : out Boolean;
                     Set_For_Reply : in Boolean := False) is
    Rec_For_C : Boolean_For_C;
    Len : Natural  := Message_Type'Size / Byte_Size;
    SFR_For_C : Boolean_For_C := Boolean_For_C(Set_For_Reply);
  begin
    Res := Soc_Receive (Socket.Soc_Addr, Rec_For_C'Address, Message'Address,
           Len'Address, SFR_For_C);
    Check_Ok;
    Received := Boolean(Rec_For_C);
    Length := Len;
  exception
    when others =>
      raise Socket_Error; 
  end Receive;


  -------------------------------------
  -- DESTINATION PORT/HOST - SENDING --
  -------------------------------------

  -- Set destination (Host/Lan and port) for sending
  -- If Lan is true then Name is a LAN name to broadcast on
  -- Otherwise it is a host name
  procedure Set_Destination_Name_And_Service (
               Socket  : in Socket_Dscr;
               Lan     : in Boolean;
               Name    : in String;
               Service : in String) is
    Name_For_C : constant String := C_Str (Name);
    Service_For_C : constant String := C_Str (Service);
  begin
    Res := Soc_Set_Dest_Service (Socket.Soc_Addr,
                                 Name_For_C'Address, Boolean_For_C(Lan),
                                 Service_For_C'Address);
    Check_Ok;
  end Set_Destination_Name_And_Service;

  procedure Set_Destination_Host_And_Service (
               Socket  : in Socket_Dscr;
               Host    : in Host_Id;
               Service : in String) is
  begin
    -- Dummy port
    Set_Destination_Host_And_Port (Socket, Host, 0);
    Change_Destination_Service (Socket, Service);
  end Set_Destination_Host_And_Service;

  procedure Set_Destination_Name_And_Port (
               Socket : in Socket_Dscr;
               Lan    : in Boolean;
               Name   : in String;
               Port   : in Port_Num) is
    Name_For_C : constant String := C_Str (Name);
  begin
    Res := Soc_Set_Dest_Port (Socket.Soc_Addr,
                              Name_For_C'Address, Boolean_For_C(Lan),
                              Word(Port) );
    Check_Ok;
  end Set_Destination_Name_And_Port;

  procedure Set_Destination_Host_And_Port (
               Socket : in Socket_Dscr;
               Host   : in Host_Id;
               Port   : in Port_Num) is
  begin
    Res := Soc_Set_Dest (Socket.Soc_Addr, Host, Word(Port) );
    Check_Ok;
  end Set_Destination_Host_And_Port;

  -- Is a tcp socket connected
  function Is_Connected (Socket : Socket_Dscr) return Boolean is
    Con_For_C : Boolean_For_C;
  begin
    Res := Soc_Is_Connected (Socket.Soc_Addr, Con_For_C'Address);
    Check_Ok;
    return Boolean (Con_For_C);
  end Is_Connected;

  -- Change destination Host/Lan
  procedure Change_Destination_Name (
               Socket : in Socket_Dscr;
               Lan    : in Boolean;
               Name   : in String) is
    Name_For_C : constant String := C_Str (Name);
  begin
    res := Soc_Change_Dest_Host (Socket.Soc_Addr, Name_For_C'Address,
                                 Boolean_For_C(Lan));
    Check_Ok;
  end Change_Destination_Name;

  procedure Change_Destination_Host (
               Socket   : in Socket_Dscr;
               Host     : in host_Id) is
    Port : Port_Num;
  begin
    Port := Get_Destination_Port (Socket);
    Set_Destination_Host_And_Port (Socket, Host, Port);
  end Change_Destination_Host;

  -- Change service or port
  procedure Change_Destination_Service (
               Socket : in Socket_Dscr;
               Service  : in String) is
    Service_For_C : constant String := C_Str (Service);
  begin
    Res := Soc_Change_Dest_Service (Socket.Soc_Addr, Service_For_C'Address);
    Check_Ok;
  end Change_Destination_Service;

  procedure Change_Destination_Port (
               Socket : in Socket_Dscr;
               Port  : in Port_Num) is
  begin
    Res := Soc_Change_Dest_Port (Socket.Soc_Addr, Word(Port) );
    Check_Ok;
  end Change_Destination_Port;

  -- Get current destination of a socket
  function Get_Destination_Host (Socket : Socket_Dscr) return Host_Id is
    Host : Host_Id;
  begin
    Res := Soc_Get_Dest_Host (Socket.Soc_Addr, Host'Address);
    Check_Ok;
    return Host;
  end Get_Destination_Host;

  function Get_Destination_Port (Socket : Socket_Dscr) return Port_Num is
     Port : Word;
  begin
    Res := Soc_Get_Dest_Port (Socket.Soc_Addr, Port'Address);
    Check_Ok;
    return Port_Num(Port);
  end Get_Destination_Port;


  -- Convert Host_Id to Host_Name and reverse (not for LANs)
  function Host_Name_Of (Id : Host_Id) return String is
    Name : String (1 .. 1024);
  begin
    Res := Soc_Host_Name_Of (Id, Name'Address, Name'Length);
    Check_Ok;
    for I in Name'Range loop
      if Name(I) = Ascii.Nul then
        return Name(1 .. I-1);
      end if;
    end loop;
    raise Socket_Error;
  end Host_Name_Of;

  function Host_Id_Of (Name : String) return Host_Id is
    Name_For_C : constant String := C_Str (Name);
    Id : Host_Id;
  begin
    Res := Soc_Host_Of (Name_For_C'Address, Id'Address);
    Check_Ok;
    return Id;
  end  Host_Id_Of;
  
  -- Send a message
  -- If Length is 0 then the full size of Message_Type is sent
  procedure Send (Socket  : in Socket_Dscr;
                  Message : in Message_Type;
                  Length  : in Natural := 0) is
    Len : Natural;
  begin
    if Length = 0 then
      Len := Message_Type'Size / Byte_Size;
    else
      Len := Length;
    end if;
    Res := Soc_Send (Socket.Soc_Addr, Message'Address, Len);
    Check_Ok;
  end Send;

end Socket;

