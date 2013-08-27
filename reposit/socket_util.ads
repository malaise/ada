-- Use Tcp_Util addresses on socket
with Socket, Tcp_Util;
package Socket_Util is

  subtype Socket_Dscr is Socket.Socket_Dscr;

  -- Set destination host/lan name or Ip address, and port name or num
  -- Lan is significant only of Host.Kind is Tcp_Util.Host_Name_Spec
  procedure Set_Destination (Soc  : in Socket_Dscr;
                             Lan  : in Boolean;
                             Host : in Tcp_Util.Remote_Host;
                             Port : in Tcp_Util.Remote_Port);

  -- Change destination host/lan name or Ip address
  -- Lan is significant only of Host.Kind is Tcp_Util.Host_Name_Spec
  procedure Change_Destination (Soc  : in Socket_Dscr;
                                Lan  : in Boolean;
                                Host : in Tcp_Util.Remote_Host);

  -- Change destination port name or num
  procedure Change_Destination (Soc  : in Socket_Dscr;
                                Port : in Tcp_Util.Remote_Port);

  -- Link to a port name or num
  procedure Link (Soc  : in Socket_Dscr;
                  Port : in Tcp_Util.Local_Port);
  procedure Link (Soc  : in Socket_Dscr;
                  Port : in Tcp_Util.Remote_Port);

end Socket_Util;

