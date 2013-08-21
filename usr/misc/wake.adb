with Ada.Exceptions;
with As.U.Utils, Basic_Proc, Argument, Str_Util.Regex, Hexa_Utils,
     Socket, Socket_Util, Tcp_Util, Ip_Addr;
-- Send "Wake On LAN" magic packet: a UDP message containing
--  6 times 'FF' then 16 times the MAC address (6 bytes each).
-- By default, broadcasts on local LAN, but if a host name or IP address
--  is provided then send to this host, by default on port 9 (but a specific
--  port name or num can be supplied).

procedure Wake is

  -- Socket stuff
  Host : Tcp_Util.Remote_Host;
  Lan_Id : Socket.Host_Id;
  Port : Tcp_Util.Remote_Port;
  Soc : Socket.Socket_Dscr;
  Default_Port : constant Socket.Port_Num := 9;

  type Byte_Array is array (Positive range <>) of Socket.Byte;

  -- Mac stuff
  subtype Mac_Type is Byte_Array (1 .. 6);
  Mac_Addr : Mac_Type;
  -- 6 bytes at FF then 16 times the 6 bytes of MAC address
  subtype Message_Type is Byte_Array (1 .. 102);
  Message : Message_Type := (others => 16#FF#);

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
      & " <mac_address> [<host>][:<port>]");
    Basic_Proc.Put_Line_Output (
      " <mac_address> ::= XX:XX:XX:XX:XX:XX    -- MAC address of host to wake up");
    Basic_Proc.Put_Line_Output (
      " <host> ::= <host_name>|<host_ip>       -- Default: broadcast on local LAN");
    Basic_Proc.Put_Line_Output (
      " <port> ::= <port_name>|<port_num>      -- Default: "
    & Ip_Addr.Image (Default_Port));
  end Usage;

  Exit_On_Error : exception;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    raise Exit_On_Error;
  end Error;

  function Image (Mac_Addr : Mac_Type) return String is
    Res : As.U.Asu_Us;
  begin
    for I in Mac_Addr'Range loop
      Res.Append (Hexa_Utils.Image (Natural (Mac_Addr(I))));
      if I /= Mac_Addr'Last then
        Res.Append (":");
      end if;
    end loop;
    return Res.Image;
  end Image;

  procedure My_Send is new Socket.Send (Message_Type);

  use type Tcp_Util.Remote_Host_List, Tcp_Util.Remote_Port_List;
begin
  ---------------------
  -- PARSE ARGUMENTS --
  ---------------------

  -- Help
  if Argument.Get_Nbre_Arg = 1
  and then (Argument.Get_Parameter = "-h"
    or else Argument.Get_Parameter = "--help") then
    Usage;
    return;
  end if;

  if Argument.Get_Nbre_Arg /= 1 and then Argument.Get_Nbre_Arg /= 2 then
    Error ("Invalid arguments");
  end if;

  -- Parse Mac address: 6 fields separated by ':'
  declare
    Mac_Str : constant As.U.Utils.Asu_Array
            := Str_Util.Regex.Split_Sep (Argument.Get_Parameter, ":");
    Mac_Error :exception;
  begin
    if Mac_Str'Length /= Mac_Addr'Length then
      raise Mac_Error;
    end if;
    for I in Mac_Addr'Range loop
      if Mac_Str(I).Length /= 2 then
        raise Mac_Error;
      end if;
      Mac_Addr(I) := Socket.Byte (Integer'(Hexa_Utils.Value (
                       Mac_Str(I).Image)));
    end loop;
  exception
    when others =>
      Error ("Invalid MAC address");
  end;

  -- Parse Host:Port
  if Argument.Get_Nbre_Arg = 2 then
    begin
      Ip_Addr.Parse (Argument.Get_Parameter (2), Host, Port);
    exception
      when others =>
        Error ("Invalid host and port");
    end;
  end if;
  -- Use default Host : Port
  if Host.Kind = Tcp_Util.Host_Name_Spec and then Host.Name.Is_Null then
    -- Default host is broadcast on LAN
    Lan_Id := Socket.Bcast_Of (Socket.Local_Host_Id);
    Host := (Tcp_Util.Host_Id_Spec, Lan_Id);
  end if;
  if Port.Kind = Tcp_Util.Port_Name_Spec and then Port.Name.Is_Null then
    -- Default port
    Port := (Tcp_Util.Port_Num_Spec, Default_Port);
  end if;

  -------------------------
  -- SET PACKET AND DEST --
  -------------------------
  for I in 1 .. 16 loop
    Message (7 + (I - 1) * 6 .. 6 + I * 6) := Mac_Addr;
  end loop;

  -- Create socket
  Soc.Open (Socket.Udp);

  -- Send dest to Host/Lan
  begin
    Socket_Util.Set_Destination (Soc, False, Host, Port);
  exception
    when Socket.Soc_Name_Not_Found =>
      Error ("Unknown destination "
            & Argument.Get_Parameter (Occurence => 2));
  end;

  -----------------------
  -- SEND MAGIC PACKET --
  -----------------------

  -- Send message
  My_Send (Soc, Message);


  -- Done
  Basic_Proc.Put_Line_Output (
      "Sent WOL packet for host " & Image (Mac_Addr)
    & " on " & Ip_Addr.Image (Soc.Get_Destination_Host,
                              Soc.Get_Destination_Port));
  -- Close socket
  Soc.Close;

exception
  when Exit_On_Error =>
    null;
  when Except : others =>
    Error ("Exception: " & Ada.Exceptions.Exception_Name (Except) & " raised");
end Wake;

