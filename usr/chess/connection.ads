with Tcp_Util;
with Space, Players;
package Connection is

  -- Initialise connection
  -- If server name is empty, we are server
  procedure Init (Server_Name : in String;
                  Port : Tcp_Util.Remote_Port;
                  Color : in Space.Color_List);

  -- Wait until iitialisation completed
  procedure Wait_Ready;

  -- Send an action
  procedure Send (Action : in Players.Action_Rec);

  -- Has an action been received and get it
  function Action_Received return Boolean;
  function Receive return Players.Action_Rec;

  -- Close connection;
  procedure Close;



  -- Error on connection or transfer
  Connection_Error : exception; 

  -- Server has rejected message
  Protocol_Error : exception;

  -- Color is same as server
  Color_Error : exception;

  -- Server is busy
  Busy_Error : exception;

  -- Receive while not Action_Received
  No_Action : exception;

end Connection;

