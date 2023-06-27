package Battle is

  -- Handle a game. If Start the current player starts the game
  -- Return true as long as play a new game
  function Play (Server, Start : Boolean) return Boolean;

  -- Error in protocol (shoot request / reply)
  Protocol_Error : exception;

end Battle;

