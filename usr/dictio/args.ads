package Args is

  procedure Init;
  procedure Usage;

  -- The Bus address
  function Get_Bus return String;

  -- The port for clients
  function Get_Client_Port return String;

  -- The prio
  subtype Prio_Str is String (1 .. 3);
  function Get_Prio return Prio_Str;

end Args;

