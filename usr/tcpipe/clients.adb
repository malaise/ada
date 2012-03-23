package body Clients is

  -- Descriptor of a client connection
  -- type Client_Rec is record
    -- The port associted to the client
  --   Port : Common.Port_Num;
    -- The definition is local (option -p) or comes from remote tcpipe
  --   Local : Boolean;
    -- When local, is it connected of not
  --   Connected : Boolean;
    -- When local not connected: the accept Fd, when connected (local or remote)
    --  the connection fd
  --   Fd : Sys_Calls.File_Desc;
  -- end record;

  -- Accept on a local port
  -- When a client connects stop accepting until it disconnects (then restart
  --  accepting)
  -- When a client is connected relay data from/to it
  -- Invalid_Port : exception;

  procedure Accept_Client (Port : in String) is
  begin
    null;
  end Accept_Client;

  -- Set target
  -- Invalid_Target : exception;

  procedure Set_Target (Target : in String) is
  begin
    null;
  end Set_Target;

  function Target_Set return Boolean is
  begin
    return Target_Set;
  end Target_Set;

  -- Connect to a port on target
  -- If Ok then relay data from/to it
  -- Else send a Disconnect to partner
  -- May raise Invalid_Target if Target is not set
  procedure Connect_Client (Port : in Common.Port_Num) is
  begin
    null;
  end Connect_Client;

  -- Disconnect from a client
  -- If local then start accepting again
  procedure Disconnect (Port : in Common.Port_Num) is
  begin
    null;
  end Disconnect;

  -- Send data to a client
  -- If error then disconnect and send a Disconnect to partner
  procedure Send (Port : in Common.Port_Num;
                  Len : in Natural;
                  Data : in Common.Data_Type) is
  begin
    null;
  end Send;

end Clients;

