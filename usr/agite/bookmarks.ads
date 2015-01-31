package Bookmarks is

  -- Init
  procedure Init;

  -- Set an internal variable
  procedure Set_Var (Name, Value : in String);

  -- Handle Bookmarks screen
  -- Returns new dir to change to, or "" if unchanged
  function Handle return String;

end Bookmarks;

