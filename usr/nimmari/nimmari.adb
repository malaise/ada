with Ada.Text_Io;
with Common, Screen, Compute;
procedure Nimmari is
  Debug : constant Boolean := False;

  -- Result of computation
  Row    : Common.Row_Range;
  Remove : Common.Bar_Status_Array;
  Result : Common.Result_List;

  -- Change game after end of a game
  Change_Game : Boolean;

  procedure Put (Bars : Common.Bar_Status_Array) is
  begin
    for I in Bars'Range loop
      if Bars(I) then
        Ada.Text_Io.Put ("I ");
      else
        Ada.Text_Io.Put ("- ");
      end if;
    end loop;
  end Put;

  use type Common.Result_List;
begin
  Compute.Init;

  -- Select game: Nim or Marienbad
  Common.Set_Game_Kind (Screen.Intro);

  One_Game:
  loop
    Common.Reset_Bars;
    -- Init screen (with game kind and scores)
    Screen.Reset;

    One_Go:
    loop
      -- Compute game, check end
      Compute.Play(Row, Remove, Result);
      if Debug then
        Ada.Text_Io.Put ("Machine played: Row" & Row'Img & " ");
        Put (Remove);
        Ada.Text_Io.Put_Line (" Result -> " & Result'Img);
      end if;

      if Result in Common.Played_Result_List then
        -- Update bars
        Screen.Update (Row, Remove);
        Common.Remove_Bars (Row, Remove);
      end if;

      -- Exit when end
      exit One_Go when Result /= Common.Played;

      -- User plays
      Screen.Play (Row, Remove);
      if Debug then
        Ada.Text_Io.Put ("Human played: Row" & Row'Img & " ");
        Put (Remove);
        Ada.Text_Io.New_Line;
      end if;
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
    -- Update scores
    Screen.Reset;

    -- Show end of game, change game?
    Screen.End_Game (Result, Change_Game);
    if Change_Game then
      -- Update game kind
      Common.Switch_Game_Kind;
      Screen.Reset;
    end if;

  end loop One_Game;

exception
  when Screen.Exit_Requested =>
    null;
end Nimmari;

