with Ada.Exceptions;
with As.U.Utils, Basic_Proc, Argument, String_Mng.Regex,
     Socket, Socket_Util, Tcp_Util, Char_To_Hexa, Ip_Addr;

procedure Wake is

  -- Socket stuff
  Host : Tcp_Util.Remote_Host;
  Lan_Id : Socket.Host_Id;
  Lan_Addr : Socket.Ip_Address;
  Port : Tcp_Util.Remote_Port;
  Soc : Socket.Socket_Dscr;
  Default_Port : constant Socket.Port_Num := 9;

  type Byte_Array is array (Positive range <>) of Socket.Byte;

  -- Mac stuff
  Mac_Addr : Byte_Array (1 .. 6);
  -- 6 bytes at FF then 16 times the 6 bytes of MAC address
  subtype Message_Type is Byte_Array (1 .. 102);
  Message : Message_Type := (others => 16#FF#);

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
      & " <mac_address> [ <host>:<port> ]");
    Basic_Proc.Put_Line_Output (" <mac_address> ::= XX:XX:XX:XX:XX:XX");
    Basic_Proc.Put_Line_Output (" <host> ::= <host_name>|<host_ip>");
    Basic_Proc.Put_Line_Output (" <port> ::= <port_name>|<port_num>");
  end Usage;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end Error;

  procedure My_Send is new Socket.Send (Message_Type);

  use type Tcp_Util.Remote_Host_List;
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
            := String_Mng.Regex.Split_Sep (Argument.Get_Parameter, ":");
    Mac_Error :exception;
  begin
    if Mac_Str'Length /= Mac_Addr'Length then
      raise Mac_Error;
    end if;
    for I in Mac_Addr'Range loop
      if Mac_Str(I).Length /= 2 then
        raise Mac_Error;
      end if;
      Mac_Addr(I) := Socket.Byte (Char_To_Hexa (Mac_Str(I).Element(1)) * 16
                                + Char_To_Hexa (Mac_Str(I).Element(2)));
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

  -------------------------
  -- SET PACKET AND DEST --
  -------------------------
  for I in 1 .. 16 loop
    Message (7 + (I - 1) * 6 .. 6 + I * 6) := Mac_Addr;
  end loop;

  -- Create socket
  Soc.Open (Socket.Udp);

  if  Argument.Get_Nbre_Arg = 1 then
    Lan_Addr := Socket.Id2Addr (Socket.Local_Host_Id);
    Lan_Addr.D := 16#FF#;
    Lan_Id := Socket.Addr2Id (Lan_Addr);
    Soc.Set_Destination_Host_And_Port (Lan_Id, Default_Port);
  else
    begin
      Socket_Util.Set_Destination (Soc, False, Host, Port);
    exception
      when Socket.Soc_Name_Not_Found =>
        Error ("Unknown destination "
              & Argument.Get_Parameter (Occurence => 2));
    end;
  end if;

  -----------------------
  -- SEND MAGIC PACKET --
  -----------------------

  -- Send message
  My_Send (Soc, Message);

  -- Close socket
  Soc.Close;

exception
  when Except : others =>
    Error ("Exception: " & Ada.Exceptions.Exception_Name (Except) & " raised");
end Wake;

