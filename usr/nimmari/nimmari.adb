with Common, Screen, Compute;
procedure Nimmari is
  Game : Common.Game_List;
  Row : Common.Row_Range;
  Bars : Common.Full_Bar_Range;
  Human, Machine : Natural := 0;
  Result : Compute.Result_List;
  Change_Game : Boolean;

  use Common, Compute;
begin
  Compute.Init;
  Game := Screen.Intro;

  One_Game:
  loop
    Screen.Reset (Game);
    Screen.Score (Human, Machine);
    One_Go:
    loop
      -- Compute game, check end
      Compute.Play (Game, Result, Row, Bars);

      -- Update score
      if Result = Compute.Won or else Result = Compute.Played_And_Won then
        Machine := Machine + 1;
        Screen.Score (Human, Machine);
      elsif Result = Compute.Lost or else Result = Compute.Played_And_Lost then
        Human := Human + 1;
        Screen.Score (Human, Machine);
      end if;

      -- Display result
      Screen.Update (Row, Bars, Result, Change_Game);

      -- exit when end
      exit One_Go when Result /= Compute.Played;

      -- User plays
      Screen.Play;

    end loop One_Go;

    if Change_Game then
      if Game = Common.Nim then
        Game := Common.Marienbad;
      else
        Game := Common.Nim;
      end if;
    end if;

  end loop One_Game;

exception
  when Screen.Exit_Requested =>
    null;
end Nimmari;

