with Socket, Event_Mng;
package Client_Fd is

  procedure Add_Client (Client : in Socket.Socket_Dscr);

  -- Unhook fd, close socket and del record
  procedure Del_Client (Client : in Socket.Socket_Dscr);
  procedure Del_All;

  function Socket_Of (Fd : Event_Mng.File_Desc) return Socket.Socket_Dscr;

  Client_Error : exception;

end Client_Fd;

