package Debug is

  type Level_List is (Status, Intra, Fight, Online, Client, Lib);
  Level_Array : array (Level_List) of Boolean;

  procedure Init;

  procedure Put (Str : in String);

end Debug;

