package Args is

  procedure Init;
  procedure Usage;

  -- Mode is either Channel (tcp) or Bus (Ipm)
  type Channel_Mode_List is (Channel, Bus);
  function Get_Mode return Channel_Mode_List;

  -- The channel file of dests if Channel, the bus lan if Bus
  function Get_Dest return String;

  -- The port
  function Get_Name return String;

  -- The port for clients
  function Get_Client_Port return String;

  -- The prio
  subtype Prio_Str is String (1 .. 3);
  function Get_Prio return Prio_Str;

end Args;

