with Socket, Sys_Calls;
package Client_Fd is

  procedure Add_Client (Client : in Socket.Socket_Dscr);

  -- Unhook fd, close socket and de record
  procedure Del_Client (Client : in Socket.Socket_Dscr);
  procedure Del_All;

  function Socket_Of (Fd : Sys_Calls.File_Desc) return Socket.Socket_Dscr;

  Client_Error : exception;

end Client_Fd;

