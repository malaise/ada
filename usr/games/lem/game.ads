package Game is

  -- Result of a game
  type Result_List is (Landed, Lost, Aborted);

  -- Play one game, start as paused, init a new environment or reuse previous
  -- Return game result
  function Play_One (Paused, New_Game : in Boolean) return Result_List;

end Game;

