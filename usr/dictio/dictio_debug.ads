package Debug is

  type Level_List is (Status, Intra, Fight, Online,
                      Client, Client_Data, Client_Notify,
                      Sync, Lib);
  Level_Array : array (Level_List) of Boolean;

  procedure Init;

  procedure Put (Str : in String);

  procedure Put_Error (Str : in String);

end Debug;

