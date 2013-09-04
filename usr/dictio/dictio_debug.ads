with Trace;
package Dictio_Debug is

  type Level_List is (Status, Intra, Fight, Online,
                      Client, Client_Data, Client_Notify, Client_Alias,
                      Sync, Lib);
  Loggers : array (Level_List) of Trace.Logger;

  -- Init all the loggers
  procedure Init;

  -- Put debug
  procedure Put (Level : in Level_List; Msg : in String);

  -- Put info, warning, error or fatal
  procedure Put_Info (Level : in Level_List; Msg : in String);
  procedure Put_Warning (Level : in Level_List; Msg : in String);
  procedure Put_Error (Level : in Level_List; Msg : in String);
  procedure Put_Fatal (Level : in Level_List; Msg : in String);

end Dictio_Debug;

