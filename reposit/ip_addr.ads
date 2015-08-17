with Socket, Tcp_Util;
package Ip_Addr is
  -- Separator between addr and port (for parsing an image)
  Sep : constant String := ":";

  -- If Addr is "xxx.yyy.zzz.ttt" where each is between 0 and 255 then
  --   return the Tcp_Util.Remote_Host (Tcp_Util.Host_Id_Spec)
  -- Elsif Addr is made of only numbers and dots, raise Parse_Error
  -- Else
  --   return the Tcp_Util.Remote_Host (Tcp_Util.Host_Name_Spec)
  -- End if
  function Parse (Addr : String) return Tcp_Util.Remote_Host;

  -- Image of an Ip address: "xxx.yyy.zzz.ttt"
  function Image  (Addr : Socket.Ip_Address) return String;

  -- Image of a host Id
  function Image  (Host : Socket.Host_Id) return String;

  -- If Port is a num between 0 and 65535 then
  --   return the Tcp_Util.Remote_Port (Tcp_Util.Port_Num_Spec)
  -- Else
  --   return the Tcp_Util.Remote_Port (Tcp_Util.Port_Name_Spec)
  -- End if
  function Parse (Port : String) return Tcp_Util.Remote_Port;

  -- Same for a Tcp_Util.Local_Port
  function Parse (Port : String) return Tcp_Util.Local_Port;

  -- Image of a port
  function Image (Port : Socket.Port_Num) return String;

  -- Parse a string at format <addr>:<port> where <addr> and <port>
  --  are processed as in both Parse functions above
  -- <addr>: is supported (and leads to empty port name)
  -- :<port> is supported (and leads to empty host name)
  procedure Parse (Addr_Port : in String;
                   Host : out Tcp_Util.Remote_Host;
                   Port : out Tcp_Util.Remote_Port);

  Parse_Error : exception;

  -- Image <addr>:<port> (xxx.yyy.zzz.ttt:pppp) of an Addr and Port
  function Image  (Addr : Socket.Ip_Address;
                   Port : Socket.Port_Num) return String;
  function Image  (Host : Socket.Host_Id;
                   Port : Socket.Port_Num) return String;

  -- Resolve a remote Host or LAN
  -- If the Host is already a Host_Id_Spec then simply extract the Host_Id
  --  otherwise use Socket.Host_Id_Of or Socket.Lan_Id_Of, which may raise
  --  Name_Error
  function Resolve (Host : Tcp_Util.Remote_Host;
                    Lan  : Boolean) return Socket.Host_Id;
  -- Resolve a remote Port
  -- If the Port is already a Port_Num_Spec then simply extract the Port_Num
  --  otherwise use Socket.Port_Num_Of, which may raise Name_Error
  function Resolve (Port : Tcp_Util.Remote_Port;
                    Protocol : Socket.Protocol_List) return Socket.Port_Num;
  -- Same on local port
  -- Raises Constraint_Error on a Port_Dynamic_Spec
  function Resolve (Port : Tcp_Util.Local_Port;
                    Protocol : Socket.Protocol_List) return Socket.Port_Num;

  Name_Error : exception renames Socket.Soc_Name_Not_Found;

end Ip_Addr;

