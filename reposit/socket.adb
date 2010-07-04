with Ada.Characters.Latin_1, Ada.Unchecked_Conversion;
package body Socket is

  ----------------
  -- INTERFACES --
  ----------------
  Nul : Character renames Ada.Characters.Latin_1.Nul;
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

  type Word is new C_Types.Uint16;
  for Word'Size use 2 * Byte_Size;

  function C_Str (Str : String) return String is
  begin
    return Str & Nul;
  end C_Str;

  type C_Protocol is new Protocol_List;
  for C_Protocol'Size use 32;

  function Soc_Open (S_Addr : System.Address;
                     Protocol : C_Protocol) return Result;
  pragma Import (C, Soc_Open, "soc_open");
  function Soc_Close (S_Addr : System.Address) return Result;
  pragma Import (C, Soc_Close, "soc_close");
  function Soc_Set_Blocking (S_Addr : System.Address;
                             Block  : C_Types.Bool) return Result;
  pragma Import (C, Soc_Set_Blocking, "soc_set_blocking");
  function Soc_Is_Blocking (S_Addr : System.Address;
                            Block  : System.Address) return Result;
  pragma Import (C, Soc_Is_Blocking, "soc_is_blocking");

  function Soc_Set_Rece_Ipm_Interface (S_Addr : System.Address;
                                       Host   : System.Address) return Result;
  pragma Import (C, Soc_Set_Rece_Ipm_Interface, "soc_set_rece_ipm_interface");
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
                        Message       : System.Address;
                        Length        : C_Types.Int;
                        Set_For_Reply : C_Types.Bool;
                        Set_Ipm_Iface : C_Types.Bool) return Result;
  pragma Import (C, Soc_Receive, "soc_receive");

  function Soc_Set_Send_Ipm_Interface (S_Addr : System.Address;
                                       Host   : System.Address) return Result;
  pragma Import (C, Soc_Set_Send_Ipm_Interface, "soc_set_send_ipm_interface");
  function Soc_Set_Dest_Name_Service (S : System.Address;
                                      Host_Lan : System.Address;
                                      Lan      : C_Types.Bool;
                                      Service  : System.Address) return Result;
  pragma Import (C, Soc_Set_Dest_Name_Service, "soc_set_dest_name_service");
  function Soc_Set_Dest_Name_Port (S : System.Address;
                                   Host_Lan : System.Address;
                                   Lan      : C_Types.Bool;
                                   Port     : Word) return Result;
  pragma Import (C, Soc_Set_Dest_Name_Port, "soc_set_dest_name_port");
  function Soc_Set_Dest_Host_Service (S : System.Address;
                                      Host : System.Address;
                                      Service  : System.Address) return Result;
  pragma Import (C, Soc_Set_Dest_Host_Service, "soc_set_dest_host_service");
  function Soc_Set_Dest_Host_Port (S : System.Address;
                                   Host : System.Address;
                                   Port : Word) return Result;
  pragma Import (C, Soc_Set_Dest_Host_Port, "soc_set_dest_host_port");

  function Soc_Change_Dest_Name (S : System.Address;
                                 Host_Lan : System.Address;
                                 Lan      : C_Types.Bool) return Result;
  pragma Import (C, Soc_Change_Dest_Name, "soc_change_dest_name");
  function Soc_Change_Dest_Host (S : System.Address;
                                 Host : System.Address) return Result;
  pragma Import (C, Soc_Change_Dest_Host, "soc_change_dest_host");
  function Soc_Change_Dest_Service (S : System.Address;
                                    Service : System.Address) return Result;
  pragma Import (C, Soc_Change_Dest_Service, "soc_change_dest_service");
  function Soc_Change_Dest_Port (S : System.Address;
                                 Port : Word) return Result;
  pragma Import (C, Soc_Change_Dest_Port, "soc_change_dest_port");

  function Soc_Is_Connected (S : System.Address;
                             P_Connected : System.Address) return Result;
  pragma Import (C, Soc_Is_Connected, "soc_is_connected");

  function Soc_Get_Dest_Host (S : System.Address;
                              Host : System.Address) return Result;
  pragma Import (C, Soc_Get_Dest_Host, "soc_get_dest_host");
  function Soc_Get_Dest_Port (S : System.Address;
                              Port : System.Address) return Result;
  pragma Import (C, Soc_Get_Dest_Port, "soc_get_dest_port");

  function Soc_Port_Name_Of (Port : Word;
                             Protocol : C_Protocol;
                             Name : System.Address;
                             Len  : C_Types.Uint32) return Result;
  pragma Import (C, Soc_Port_Name_Of, "soc_port_name_of");
  function Soc_Port_Of (Name : System.Address;
                        Protocol : C_Protocol;
                        Port : System.Address) return Result;
  pragma Import (C, Soc_Port_Of, "soc_port_of");
  function Soc_Host_Name_Of (Host : System.Address; Name : System.Address;
                             Len  : C_Types.Uint32) return Result;
  pragma Import (C, Soc_Host_Name_Of, "soc_host_name_of");
  function Soc_Host_Of (Name : System.Address;
                        Id   : System.Address) return Result;
  pragma Import (C, Soc_Host_Of, "soc_host_of");
  function Soc_Get_Local_Host_Name (Name : System.Address;
                                    Host_Name_Len : C_Types.Uint32)
           return Result;
  pragma Import (C, Soc_Get_Local_Host_Name, "soc_get_local_host_name");
  function Soc_Get_Local_Host_Id(Id : System.Address) return Result;
  pragma Import (C, Soc_Get_Local_Host_Id, "soc_get_local_host_id");


  function Soc_Send (S : System.Address;
                     Message : System.Address;
                     Length  : C_Types.Uint32) return Result;
  pragma Import (C, Soc_Send, "soc_send");
  function Soc_Resend (S : System.Address) return Result;
  pragma Import (C, Soc_Resend, "soc_resend");

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
  procedure Set_Blocking (Socket : in Socket_Dscr; Blocking : in Boolean) is
  begin
    Res := Soc_Set_Blocking (Socket.Soc_Addr, C_Types.Bool(Blocking));
    Check_Ok;
  end Set_Blocking;

  -- Is a socket in blocking mode
  function Is_Blocking (Socket : in Socket_Dscr) return Boolean is
    Bool : C_Types.Bool;
  begin
    Res := Soc_Is_Blocking (Socket.Soc_Addr, Bool'Address);
    Check_Ok;
    return Boolean(Bool);
  end Is_Blocking;

  -- Get the Fd of a socket (for use in X_Mng. Add/Del _Callback)
  function Fd_Of (Socket : in Socket_Dscr) return Sys_Calls.File_Desc is
    Fd : Integer;
  begin
    Res := Soc_Get_Id (Socket.Soc_Addr, Fd'Address);
    Check_Ok;
    return Sys_Calls.File_Desc(Fd);
  end Fd_Of;



  -------------------------------------
  -- RECEPTION PORT - FD - RECEPTION --
  -------------------------------------

  -- Set the receiving IPM interface
  procedure Set_Reception_Ipm_Interface (Socket : in Socket_Dscr;
                                         Host   : in Host_Id) is
  begin
    Res := Soc_Set_Rece_Ipm_Interface (Socket.Soc_Addr, Host'Address);
    Check_Ok;
  end Set_Reception_Ipm_Interface;

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
                     Set_For_Reply : in Boolean := False;
                     Set_Ipm_Iface : in Boolean := False) is
    Len : Natural;
    Sfr_For_C : constant C_Types.Bool := C_Types.Bool(Set_For_Reply);
    Sif_For_C : constant C_Types.Bool := C_Types.Bool(Set_Ipm_Iface);
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
    Res := Soc_Receive (Socket.Soc_Addr, Message'Address, Len,
                        Sfr_For_C, Sif_For_C);
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
    for I in Name'Range loop
      if Name(I) = Nul then
        return Name(1 .. I-1);
      end if;
    end loop;
    raise Soc_Len_Err;
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

  -- Convert Host_Id to Host_Name and reverse (not for LANs)
  function Host_Name_Of (Id : Host_Id) return String is
    Name : String (1 .. 1024);
  begin
    Res := Soc_Host_Name_Of (Id'Address, Name'Address, Name'Length);
    Check_Ok;
    for I in Name'Range loop
      if Name(I) = Nul then
        return Name(1 .. I-1);
      end if;
    end loop;
    raise Soc_Len_Err;
  end Host_Name_Of;

  function Host_Id_Of (Name : String) return Host_Id is
    Name_For_C : constant String := C_Str (Name);
    Id : Host_Id;
  begin
    Res := Soc_Host_Of (Name_For_C'Address, Id'Address);
    Check_Ok;
    return Id;
  end Host_Id_Of;

  -- Get local Host name or id
  function Local_Host_Name return String is
    Name : String (1 .. 1024);
  begin
    Res := Soc_Get_Local_Host_Name (Name'Address, Name'Length);
    Check_Ok;
    for I in Name'Range loop
      if Name(I) = Nul then
        return Name(1 .. I-1);
      end if;
    end loop;
    raise Soc_Len_Err;
  end Local_Host_Name;

  function Local_Host_Id return Host_Id is
    Id : Host_Id;
  begin
    Res := Soc_Get_Local_Host_Id (Id'Address);
    Check_Ok;
    return Id;
  end Local_Host_Id;

  -- Host_Id <-> 4 bytes of Ip address
  function L_Id2Addr is new Ada.Unchecked_Conversion (Host_Id, Ip_Address);
  function Id2Addr (Id : Host_Id) return Ip_Address is
  begin
    return L_Id2Addr(Id);
  end Id2Addr;

  function L_Addr2Id is new Ada.Unchecked_Conversion (Ip_Address, Host_Id);
  function Addr2Id (Addr : Ip_Address) return Host_Id is
  begin
    return L_Addr2Id(Addr);
  end Addr2Id;

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

