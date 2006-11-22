package Game is

  -- Result of a game
  type Result_List is (Landed, Lost, Aborted);

  -- Play one game, init a new environment
  -- Return game result
  function Play_One (New_Game : in Boolean) return Result_List;

end Game;

