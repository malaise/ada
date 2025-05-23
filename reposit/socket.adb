with Ada.Unchecked_Conversion;
with Aski, Address_Ops;
package body Socket is

  ----------------
  -- INTERFACES --
  ----------------
  Nul : Character renames Aski.Nul;
  Byte_Size : constant := C_Types.Uint8'Size;

  subtype Result is C_Types.Int;
  C_Soc_Ok : constant Result := 0;
  C_Soc_Use_Err   : constant Result :=  -1;
  C_Soc_Sys_Err   : constant Result :=  -2;
  C_Soc_Dest_Err  : constant Result :=  -3;
  C_Soc_Link_Err  : constant Result :=  -4;
  C_Soc_Conn_Err  : constant Result :=  -5;
  C_Soc_Bcast_Err : constant Result :=  -6;
  C_Soc_Len_Err   : constant Result :=  -7;
  C_Soc_Reply_Err : constant Result :=  -8;
  C_Soc_Tail_Err  : constant Result :=  -9;
  C_Soc_Proto_Err : constant Result := -10;
  C_Soc_Fd_In_Use : constant Result := -11;
  C_Soc_Fmt_Err   : constant Result := -12;

  C_Soc_Conn_Refused   : constant Result := -21;
  C_Soc_Name_Not_Found : constant Result := -22;
  C_Soc_Would_Block    : constant Result := -23;
  C_Soc_Conn_Lost      : constant Result := -24;
  C_Soc_Addr_In_Use    : constant Result := -25;
  C_Soc_Read_0         : constant Result := -26;
  C_Soc_Reply_Iface    : constant Result := -27;

  type Word is new C_Types.Uint16
    with Size => 16;

  function C_Str (Str : String) return String is (Str & Nul);

  function Strip (C_Str : String) return String is
  begin
    for I in C_Str'Range loop
      if C_Str(I) = Nul then
        return C_Str(C_Str'First .. I - C_Str'First);
      end if;
    end loop;
    raise Soc_Len_Err;
  end Strip;

  type C_Protocol is new Protocol_List
    with Size => 32;

  type C_Blocking_Mode is new Blocking_List
    with Size => 32;

  function Soc_Open (S_Addr : System.Address;
                     Protocol : C_Protocol) return Result
    with Import => True, Convention => C, External_Name => "soc_open";
  function Soc_Close (S_Addr : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_close";

  function Soc_Set_Blocking (S_Addr : System.Address;
                             Block  : C_Blocking_Mode) return Result
    with Import => True, Convention => C, External_Name => "soc_set_blocking";
  function Soc_Get_Blocking (S_Addr : System.Address;
                             Block  : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_get_blocking";
  function Soc_Get_Protocol (S_Addr : System.Address;
                             Protocol  : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_get_protocol";
  function Soc_Set_Ttl (S_Addr : System.Address; Ttl : Byte) return Result
    with Import => True, Convention => C, External_Name => "soc_set_ttl";
  function Soc_Get_Ttl (S_Addr : System.Address;
                        Ttl : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_get_ttl";

  function Soc_Set_Rece_Interface (S_Addr : System.Address;
                                   Host   : System.Address) return Result
    with Import => True, Convention => C,
         External_Name => "soc_set_rece_interface";
  function Soc_Link_Service (S : System.Address;
                             Service : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_link_service";
  function Soc_Link_Port (S : System.Address;
                          Port : Word) return Result
    with Import => True, Convention => C, External_Name => "soc_link_port";
  function Soc_Link_Dynamic (S : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_link_dynamic";
  function Soc_Get_Linked_Port (S : System.Address;
                                Port : System.Address) return Result
    with Import => True, Convention => C,
         External_Name => "soc_get_linked_port";
  function Soc_Get_Id (S : System.Address;
                       Fd : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_get_id";
  function Soc_Accept (S : System.Address; N :  System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_accept";
  function Soc_Receive (S : System.Address;
                        Message       : System.Address;
                        Length        : C_Types.Int;
                        Set_For_Reply : C_Types.Bool) return Result
    with Import => True, Convention => C, External_Name => "soc_receive";

  function Soc_Set_Send_Ipm_Interface (S_Addr : System.Address;
                                       Host   : System.Address) return Result
    with Import => True, Convention => C,

         External_Name => "soc_set_send_ipm_interface";
  function Soc_Bind_Service (S : System.Address;
                             Service : System.Address) return Result
    with Import => True, Convention => C,
         External_Name => "soc_bind_service";
  function Soc_Bind_Port (S : System.Address;
                          Port : Word) return Result
    with Import => True, Convention => C,
         External_Name => "soc_bind_port";
  function Soc_Get_Bound_Port (S : System.Address;
                               Port : System.Address) return Result
    with Import => True, Convention => C,
         External_Name => "soc_get_bound_port";

  function Soc_Set_Dest_Name_Service (S : System.Address;
                                      Host_Lan : System.Address;
                                      Lan      : C_Types.Bool;
                                      Service  : System.Address) return Result
    with Import => True, Convention => C,
         External_Name => "soc_set_dest_name_service";
  function Soc_Set_Dest_Name_Port (S : System.Address;
                                   Host_Lan : System.Address;
                                   Lan      : C_Types.Bool;
                                   Port     : Word) return Result
    with Import => True, Convention => C,
         External_Name => "soc_set_dest_name_port";
  function Soc_Set_Dest_Host_Service (S : System.Address;
                                      Host : System.Address;
                                      Service  : System.Address) return Result
    with Import => True, Convention => C,
         External_Name => "soc_set_dest_host_service";
  function Soc_Set_Dest_Host_Port (S : System.Address;
                                   Host : System.Address;
                                   Port : Word) return Result
    with Import => True, Convention => C,
         External_Name => "soc_set_dest_host_port";

  function Soc_Change_Dest_Name (S : System.Address;
                                 Host_Lan : System.Address;
                                 Lan      : C_Types.Bool) return Result
    with Import => True, Convention => C,
         External_Name => "soc_change_dest_name";
  function Soc_Change_Dest_Host (S : System.Address;
                                 Host : System.Address) return Result
    with Import => True, Convention => C,
         External_Name => "soc_change_dest_host";
  function Soc_Change_Dest_Service (S : System.Address;
                                    Service : System.Address) return Result
    with Import => True, Convention => C,
         External_Name => "soc_change_dest_service";
  function Soc_Change_Dest_Port (S : System.Address;
                                 Port : Word) return Result
    with Import => True, Convention => C,
         External_Name => "soc_change_dest_port";

  function Soc_Is_Connected (S : System.Address;
                             P_Connected : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_is_connected";

  function Soc_Get_Dest_Host (S : System.Address;
                              Host : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_get_dest_host";
  function Soc_Get_Dest_Port (S : System.Address;
                              Port : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_get_dest_port";

  function Soc_Port_Name_Of (Port : Word;
                             Protocol : C_Protocol;
                             Name : System.Address;
                             Len  : C_Types.Uint32) return Result
    with Import => True, Convention => C, External_Name => "soc_port_name_of";
  function Soc_Port_Of (Name : System.Address;
                        Protocol : C_Protocol;
                        Port : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_port_of";
  function Soc_Host_Name_Of (Host : System.Address; Name : System.Address;
                             Len  : C_Types.Uint32) return Result
    with Import => True, Convention => C, External_Name => "soc_host_name_of";
  function Soc_Host_Of (Name : System.Address;
                        Id   : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_host_of";
  function Soc_Lan_Name_Of (Lan : System.Address; Name : System.Address;
                            Len  : C_Types.Uint32) return Result
    with Import => True, Convention => C, External_Name => "soc_lan_name_of";
  function Soc_Lan_Of (Name : System.Address;
                       Id   : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_lan_of";
  function Soc_Get_Local_Host_Name (Name : System.Address;
                                    Host_Name_Len : C_Types.Uint32)
           return Result
    with Import => True, Convention => C,
         External_Name => "soc_get_local_host_name";
  function Soc_Get_Local_Host_Id(Id : System.Address) return Result
    with Import => True, Convention => C,
         External_Name => "soc_get_local_host_id";
  function Soc_Get_Local_Lan_Name (Name : System.Address;
                                   Lan_Name_Len : C_Types.Uint32) return Result
    with Import => True, Convention => C,
         External_Name => "soc_get_local_lan_name";
  function Soc_Get_Local_Lan_Id(Id : System.Address) return Result
    with Import => True, Convention => C,
         External_Name => "soc_get_local_lan_id";

  function Soc_Get_Bcast(If_Host, Bcast_Host : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_get_bcast";
  function Soc_Get_Host_Iface (Lan, Netmask, Id : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_get_host_iface";

  function Soc_Send (S : System.Address;
                     Message : System.Address;
                     Length  : C_Types.Uint32) return Result
    with Import => True, Convention => C, External_Name => "soc_send";
  function Soc_Resend (S : System.Address) return Result
    with Import => True, Convention => C, External_Name => "soc_resend";

  --------------------
  -- IMPLEMENTATION --
  --------------------
  Res : Result;

  -- Will be anonymous.
  Soc_Unknown_Error : exception;
  procedure Check_Ok is
  begin
    if Res = C_Soc_Ok then
      return;
    end if;
    case Res is
      when C_Soc_Use_Err   => raise Soc_Use_Err;
      when C_Soc_Sys_Err   => raise Soc_Sys_Err;
      when C_Soc_Dest_Err  => raise Soc_Dest_Err;
      when C_Soc_Link_Err  => raise Soc_Link_Err;
      when C_Soc_Conn_Err  => raise Soc_Conn_Err;
      when C_Soc_Bcast_Err => raise Soc_Bcast_Err;
      when C_Soc_Len_Err   => raise Soc_Len_Err;
      when C_Soc_Reply_Err => raise Soc_Reply_Err;
      when C_Soc_Tail_Err  => raise Soc_Tail_Err;
      when C_Soc_Proto_Err => raise Soc_Proto_Err;
      when C_Soc_Fd_In_Use => raise Soc_Fd_In_Use;
      when C_Soc_Fmt_Err   => raise Soc_Unknown_Error;

      when C_Soc_Conn_Refused   => raise Soc_Conn_Refused;
      when C_Soc_Name_Not_Found => raise Soc_Name_Not_Found;
      when C_Soc_Would_Block    => raise Soc_Would_Block;
      when C_Soc_Conn_Lost      => raise Soc_Conn_Lost;
      when C_Soc_Addr_In_Use    => raise Soc_Addr_In_Use;
      when C_Soc_Read_0         => raise Soc_Read_0;
      when C_Soc_Reply_Iface    => raise Soc_Reply_Iface;
      when others => raise Soc_Unknown_Error;
    end case;
  end Check_Ok;

  -- Image of a socket (for hashing by user)
  function Image (Socket: in Socket_Dscr) return String is
  begin
    if not Is_Open (Socket) then
      raise Soc_Use_Err;
    end if;
    return Address_Ops.Image (Socket.Soc_Addr);
  end Image;

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

  -- Set a socket blocking or not
  procedure Set_Blocking (Socket : in Socket_Dscr;
                          Blocking : in Blocking_List) is
  begin
    Res := Soc_Set_Blocking (Socket.Soc_Addr, C_Blocking_Mode(Blocking));
    Check_Ok;
  end Set_Blocking;

  -- Get a socket blocking mode
  function Get_Blocking (Socket : in Socket_Dscr) return Blocking_List is
    Mode : C_Blocking_Mode;
  begin
    Res := Soc_Get_Blocking (Socket.Soc_Addr, Mode'Address);
    Check_Ok;
    return Blocking_List(Mode);
  end Get_Blocking;

  -- Is a socket in blocking mode in emission or reception
  function Is_Blocking (Socket : in Socket_Dscr; Emission : in Boolean)
                       return Boolean is
    Mode : C_Blocking_Mode;
  begin
    Res := Soc_Get_Blocking (Socket.Soc_Addr, Mode'Address);
    Check_Ok;
    return  (Emission and then Mode /= Non_Blocking)
    or else (not Emission and then Mode = Full_Blocking);
  end Is_Blocking;

  -- Set the TTL of a socket
  procedure Set_Ttl (Socket : in Socket_Dscr; Ttl : in Ttl_Range) is
  begin
    Res := Soc_Set_Ttl (Socket.Soc_Addr, Byte(Ttl));
    Check_Ok;
  end Set_Ttl;

  -- Get the TTL of a socket
  function Get_Ttl (Socket : Socket_Dscr) return Ttl_Range is
    Ttl : Byte;
  begin
    Res := Soc_Get_Ttl (Socket.Soc_Addr, Ttl'Address);
    Check_Ok;
    return Ttl_Range (Ttl);
  end Get_Ttl;

  -- Get the Fd of a socket (for use in Event_Mng. Add/Del _Callback)
  function Get_Fd (Socket : in Socket_Dscr) return Sys_Calls.File_Desc is
    Fd : Integer;
  begin
    Res := Soc_Get_Id (Socket.Soc_Addr, Fd'Address);
    Check_Ok;
    return Sys_Calls.File_Desc(Fd);
  end Get_Fd;

  -- Get the protocol of a socket
  function Get_Protocol (Socket : in Socket_Dscr) return Protocol_List is
    Protocol : C_Protocol;
  begin
    Res := Soc_Get_Protocol (Socket.Soc_Addr, Protocol'Address);
    Check_Ok;
    return Protocol_List(Protocol);
  end Get_Protocol;


  -------------------------------------
  -- RECEPTION PORT - FD - RECEPTION --
  -------------------------------------

  -- Set the receiving interface
  procedure Set_Reception_Interface (Socket : in Socket_Dscr;
                                     Host   : in Host_Id) is
  begin
    Res := Soc_Set_Rece_Interface (Socket.Soc_Addr, Host'Address);
    Check_Ok;
  end Set_Reception_Interface;

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

  -- Receive a message, waiting for it
  -- The socket destination may be set for a reply
  procedure Receive (Socket        : in Socket_Dscr;
                     Message       : out Message_Type;
                     Length        : out Natural;
                     Set_For_Reply : in Boolean := False) is
    Len : Natural;
    Sfr_For_C : constant C_Types.Bool := C_Types.Bool(Set_For_Reply);
  begin
    if Message_Size = 0 then
      -- Size not provided at instantiation, guess it from
        -- provided type
      if Message'Size rem Byte_Size /= 0 then
        raise Soc_Len_Err;
      end if;
      Len := Message'Size / Byte_Size;
    else
      if Message_Size rem Byte_Size /= 0 then
        raise Soc_Len_Err;
      end if;
      Len := Message_Size / Byte_Size;
    end if;
    Res := Soc_Receive (Socket.Soc_Addr, Message'Address, Len, Sfr_For_C);
    if Res >= 0 then
      Length := Res;
      return;
    end if;
    Check_Ok;
  end Receive;


  -------------------------------------
  -- DESTINATION PORT/HOST - SENDING --
  -------------------------------------

  -- Set the sending IPM interface
  procedure Set_Sending_Ipm_Interface (Socket : in Socket_Dscr;
                                         Host   : in Host_Id) is
  begin
    Res := Soc_Set_Send_Ipm_Interface (Socket.Soc_Addr, Host'Address);
    Check_Ok;
  end Set_Sending_Ipm_Interface;

  -- Before setting the destination of a TCP socket, define the sending port
  procedure Bind_Service (Socket  : in Socket_Dscr;
                          Service : in String) is
    Service_For_C : constant String := C_Str (Service);
  begin
    Res := Soc_Bind_Service (Socket.Soc_Addr, Service_For_C'Address);
    Check_Ok;
  end Bind_Service;

  procedure Bind_Port (Socket  : in Socket_Dscr;
                       Port   : in Port_Num) is
  begin
    Res := Soc_Bind_Port (Socket.Soc_Addr, Word (Port));
    Check_Ok;
  end Bind_Port;

  -- Get bound port
  function Get_Bound_Port (Socket : Socket_Dscr) return Port_Num is
    Port : Word;
  begin
    Res := Soc_Get_Bound_Port (Socket.Soc_Addr, Port'Address);
    Check_Ok;
    return Port_Num(Port);
  end Get_Bound_Port;

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
    Res := Soc_Set_Dest_Name_Service (Socket.Soc_Addr,
                                 Name_For_C'Address, C_Types.Bool(Lan),
                                 Service_For_C'Address);
    Check_Ok;
  end Set_Destination_Name_And_Service;

  procedure Set_Destination_Host_And_Service (
               Socket  : in Socket_Dscr;
               Host    : in Host_Id;
               Service : in String) is
    Service_For_C : constant String := C_Str (Service);
  begin
    Res := Soc_Set_Dest_Host_Service (Socket.Soc_Addr, Host'Address,
                                      Service_For_C'Address);
    Check_Ok;
  end Set_Destination_Host_And_Service;

  procedure Set_Destination_Name_And_Port (
               Socket : in Socket_Dscr;
               Lan    : in Boolean;
               Name   : in String;
               Port   : in Port_Num) is
    Name_For_C : constant String := C_Str (Name);
  begin
    Res := Soc_Set_Dest_Name_Port (Socket.Soc_Addr,
                              Name_For_C'Address, C_Types.Bool(Lan),
                              Word(Port) );
    Check_Ok;
  end Set_Destination_Name_And_Port;

  procedure Set_Destination_Host_And_Port (
               Socket : in Socket_Dscr;
               Host   : in Host_Id;
               Port   : in Port_Num) is
  begin
    Res := Soc_Set_Dest_Host_Port (Socket.Soc_Addr, Host'Address, Word(Port) );
    Check_Ok;
  end Set_Destination_Host_And_Port;

  -- Is a tcp socket connected
  function Is_Connected (Socket : Socket_Dscr) return Boolean is
    Con_For_C : C_Types.Bool;
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
    Res := Soc_Change_Dest_Name (Socket.Soc_Addr, Name_For_C'Address,
                                 C_Types.Bool(Lan));
    Check_Ok;
  end Change_Destination_Name;

  procedure Change_Destination_Host (
               Socket   : in Socket_Dscr;
               Host     : in Host_Id) is
  begin
    Res := Soc_Change_Dest_Host (Socket.Soc_Addr, Host'Address);
    Check_Ok;
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

  -- Convert Port_Num to Port_Name and reverse (for a given protocol)
  function Port_Name_Of (Port : Port_Num; Protocol : Protocol_List)
                        return String is
    Name : String (1 .. 1024);
  begin
    Res := Soc_Port_Name_Of (Word (Port), C_Protocol(Protocol),
                             Name'Address, Name'Length);
    Check_Ok;
    return Strip (Name);
  end Port_Name_Of;

  function Port_Num_Of  (Name : String; Protocol : Protocol_List)
                        return Port_Num is
    Name_For_C : constant String := C_Str (Name);
    Port : Word;
  begin
    Res := Soc_Port_Of (Name_For_C'Address, C_Protocol(Protocol),
                        Port'Address);
    Check_Ok;
    return Port_Num(Port);
  end Port_Num_Of;

  -- Convert Host_Id to Host Name and reverse (not for LANs)
  function Host_Name_Of (Id : Host_Id) return String is
    Name : String (1 .. 1024);
  begin
    Res := Soc_Host_Name_Of (Id'Address, Name'Address, Name'Length);
    Check_Ok;
    return Strip (Name);
  end Host_Name_Of;

  function Host_Id_Of (Name : String) return Host_Id is
    Name_For_C : constant String := C_Str (Name);
    Id : Host_Id;
  begin
    Res := Soc_Host_Of (Name_For_C'Address, Id'Address);
    Check_Ok;
    return Id;
  end Host_Id_Of;

  -- Convert Host_Id to Lan Name and reverse (not for hosts)
  function Lan_Name_Of (Id : Host_Id) return String is
    Name : String (1 .. 1024);
  begin
    Res := Soc_Lan_Name_Of (Id'Address, Name'Address, Name'Length);
    Check_Ok;
    return Strip (Name);
  end Lan_Name_Of;

  function Lan_Id_Of (Name : String) return Host_Id is
    Name_For_C : constant String := C_Str (Name);
    Id : Host_Id;
  begin
    Res := Soc_Lan_Of (Name_For_C'Address, Id'Address);
    Check_Ok;
    return Id;
  end Lan_Id_Of;

  -- Get local Host name or id
  function Local_Host_Name return String is
    Name : String (1 .. 1024);
  begin
    Res := Soc_Get_Local_Host_Name (Name'Address, Name'Length);
    Check_Ok;
    return Strip (Name);
  end Local_Host_Name;

  function Local_Host_Id return Host_Id is
    Id : Host_Id;
  begin
    Res := Soc_Get_Local_Host_Id (Id'Address);
    Check_Ok;
    return Id;
  end Local_Host_Id;

  -- Get local LAN name or id
  function Local_Lan_Name return String is
    Name : String (1 .. 1024);
  begin
    Res := Soc_Get_Local_Lan_Name (Name'Address, Name'Length);
    Check_Ok;
    return Strip (Name);
  end Local_Lan_Name;

  function Local_Lan_Id return Host_Id is
    Id : Host_Id;
  begin
    Res := Soc_Get_Local_Lan_Id (Id'Address);
    Check_Ok;
    return Id;
  end Local_Lan_Id;

  -- Host_Id <-> 4 bytes of Ip address
  function L_Id2Addr is new Ada.Unchecked_Conversion (Host_Id, Ip_Address);
  function Id2Addr (Id : Host_Id) return Ip_Address is (L_Id2Addr(Id));

  function L_Addr2Id is new Ada.Unchecked_Conversion (Ip_Address, Host_Id);
  function Addr2Id (Addr : Ip_Address) return Host_Id is (L_Addr2Id(Addr));

   -- Broadcast address for a given local interface
  function Local_Bcast_Of (If_Id : Host_Id) return Host_Id is
    Ret_Id : Host_Id;
  begin
    Res := Soc_Get_Bcast (If_Id'Address, Ret_Id'Address);
    Check_Ok;
    return Ret_Id;
  end Local_Bcast_Of;

  -- Local host address on a given LAN and netmask
  function Local_Host_Id_For (Lan, Netmask : Host_Id) return Host_Id is
    Ret_Id : Host_Id;
  begin
    Res := Soc_Get_Host_Iface (Lan'Address, Netmask'Address, Ret_Id'Address);
    Check_Ok;
    return Ret_Id;
  end Local_Host_Id_For;

  -- Send a message
  -- If Length is 0 then the full size of Message_Type is sent
  procedure Send (Socket  : in Socket_Dscr;
                  Message : in Message_Type;
                  Length  : in Natural := 0) is
    Len : Natural;
  begin
    if Length = 0 then
      -- Size not provided at sending, use default
      if Message_Size = 0 then
        -- Size not provided at instantiation, guess it from
          -- provided type
        if Message'Size rem Byte_Size /= 0 then
          raise Soc_Len_Err;
        end if;
        Len := Message'Size / Byte_Size;
      else
        if Message_Size rem Byte_Size /= 0 then
          raise Soc_Len_Err;
        end if;
        Len := Message_Size / Byte_Size;
      end if;
      Len := Message'Size / Byte_Size;
    else
      Len := Length;
    end if;
    Res := Soc_Send (Socket.Soc_Addr, Message'Address, C_Types.Uint32(Len));
    Check_Ok;
  end Send;

  -- Try to send remaining of message after a Soc_Would_Block
  --  on Send or Re_Send
  procedure Re_Send (Socket  : in Socket_Dscr) is
  begin
    Res := Soc_Resend (Socket.Soc_Addr);
    Check_Ok;
  end Re_Send;

end Socket;

