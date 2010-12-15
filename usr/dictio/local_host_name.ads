package Local_Host_Name is

  Max_Host_Name_Length : constant := 64;
  subtype Host_Name is String (1 .. Max_Host_Name_Length);

  procedure Set (Name : in String);

  function Get return String;

  procedure Get (Name : out Host_Name);

end Local_Host_Name;

