package Args is

  procedure Init;
  procedure Usage;

  function Get_Channel_Name return String;
  function Get_Dest_File return String;
  function Get_Client_Port return String;

end Args;

