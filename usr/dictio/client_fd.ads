with Socket, Event_Mng;
package Client_Fd is

  procedure Add_Client (Client : in Socket.Socket_Dscr);

  -- Unhook fd, close socket and del record
  procedure Del_Client (Client : in Socket.Socket_Dscr);
  procedure Del_All;

  -- Read first/next client
  -- Socket.No_Socket when no more
  procedure Read_First (Client : out Socket.Socket_Dscr);
  procedure Read_Next  (Client : out Socket.Socket_Dscr);

  function Socket_Of (Fd : Event_Mng.File_Desc) return Socket.Socket_Dscr;

  Client_Error : exception;

end Client_Fd;

