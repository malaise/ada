package Communication is

  -- Has a signal been received
  function Sig_Received return Boolean;

  -- Wait until a partner is connected
  -- When client, send a "C" (client) each second
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

  -- Close communications
  procedure Close;

end Communication;


