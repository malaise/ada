package Setup is

  -- Connect, return True if success
  function Init (Addr : String; Server : Boolean) return Boolean;

  -- Define fleet
  procedure Define;

end Setup;
