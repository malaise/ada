with Common, Screen, Compute;
procedure Nimmari is

  -- Result of computation
  Row    : Common.Row_Range;
  Remove : Common.Bar_Status_Array;
  Result : Common.Result_List;

  -- Change game after end of a game
  Change_Game : Boolean;

  use type Common.Result_List;
begin
  Compute.Init;

  -- Select game: Nim or Marienbad
  Common.Set_Game_Kind (Screen.Intro);

  One_Game:
  loop
    -- Init screen (with game kind and scores)
    Screen.Reset (Common.Get_Game_Kind, Common.Get_Scores);

    One_Go:
    loop
      -- Compute game, check end
      Compute.Play(Row, Remove, Result);

      -- Update bars
      Common.Remove_Bars (Row, Remove);
      Screen.Update (Row, Remove);

      -- exit when end
      exit One_Go when Result /= Common.Played;

      -- User plays
      Screen.Play (Row, Remove);
      Common.Remove_Bars (Row, Remove);

    end loop One_Go;

    -- Update score
    if Result = Common.Won or else Result = Common.Played_And_Won then
      -- Machine wins
      Common.Add_Win (Common.Machine);
    elsif Result = Common.Lost or else Result = Common.Played_And_Lost then
      -- Machine looses
      Common.Add_Win (Common.Human);
    end if;
    Screen.Reset (Common.Get_Game_Kind, Common.Get_Scores);

    -- Show end of game, change game?
    Screen.End_Game (Result, Change_Game);
    if Change_Game then
      Common.Switch_Game_Kind;
    end if;

  end loop One_Game;

exception
  when Screen.Exit_Requested =>
    null;
end Nimmari;

