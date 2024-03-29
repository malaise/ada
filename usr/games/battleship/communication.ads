package Communication is

  -- Has a signal been received
  function Sig_Received return Boolean;

  -- Initialise connection with server
  -- When client, send a "C" (client) each second
  -- A Fd_Event will be generated when connection is completed
  Init_Error : exception;
  procedure Connect (Addr : in String; Server : in Boolean);
  -- Are we connected? if yes, stop connecting
  function Is_Connected return Boolean;

  -- Set reception callback (if not null)
  -- De-activate previous callback
  type Reception_Cb is access procedure (Msg : in String);
  procedure Set_Callback (Receive : Reception_Cb := null);

  -- Send "E" (end) to partner
  procedure Send_End;

  -- Send a message to partner
  procedure Send (Msg : in String);

  -- Close communications
  procedure Close;

end Communication;

