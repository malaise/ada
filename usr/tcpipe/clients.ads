with Common;
package Clients is

  -- Accept on a local port
  -- When a client connects stop accepting until it disconnects (then restart
  --  accepting)
  -- When a client is connected relay data from/to it
  Invalid_Port : exception;
  procedure Accept_Client (Port : in String);

  -- Set target
  Invalid_Target : exception;
  procedure Set_Target (Target : in String);
  function Target_Set return Boolean;

  -- Connect to a port on target
  -- If Ok then relay data from/to it
  -- Else send a Disconnect to partner
  -- May raise Invalid_Target if Target is not set
  procedure Connect_Client (Port : in Common.Port_Num);

  -- Disconnect from a client
  -- If local then start accepting again
  procedure Disconnect (Port : in Common.Port_Num);

  -- Disconnect all clients
  -- If local then start accepting again
  procedure Disconnect_All;

  -- Send data to a client
  -- If error then disconnect and send a Disconnect to partner
  procedure Send (Port : in Common.Port_Num;
                  Len : in Natural;
                  Data : in Common.Data_Type);

end Clients;

