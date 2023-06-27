package Setup is

  -- Wait for connection, return True if success
  function Init (Addr : String; Server : Boolean) return Boolean;

  -- Let the user define the fleet
  procedure Define;

end Setup;
