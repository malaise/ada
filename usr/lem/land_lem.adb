with Rnd;
with Game;
procedure Land_Lem is
  -- Result of a game
  -- Trigger a new environment at startup (as if prev game succeeded)
  Result : Game.Result_List := Game.Landed;
  use type Game.Result_List;
begin
  Rnd.Gen.Randomize;
  -- Do one game until end
  loop
    Result := Game.Play_One (Result = Game.Landed);
    exit when Result = Game.Aborted;
  end loop;
end Land_Lem;
