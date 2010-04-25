with Ada.Text_Io;
with Argument;
with Common, Screen, Text, Compute;
procedure Nimmari is
  Debug : constant Boolean := False;
  Graphic_Mode : Boolean := False;

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

  -- Parse arguments
  if Argument.Get_Parameter (1) = "--text" then
    Graphic_Mode := False;
  end if;

  -- Init then select game: Nim or Marienbad
  Compute.Init;
  if Graphic_Mode then
    Common.Set_Game_Kind (Screen.Intro);
  else
    Common.Set_Game_Kind (Text.Intro);
  end if;

  One_Game:
  loop
    Common.Reset_Bars;
    -- Init screen (with game kind and scores)
    if Graphic_Mode then
      Screen.Reset;
    end if;

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
        if Graphic_Mode then
          Screen.Update (Row, Remove);
        else
          Text.Update (Row, Remove);
        end if;
        Common.Remove_Bars (Row, Remove);
      end if;

      -- Exit when end
      exit One_Go when Result /= Common.Played;

      -- User plays
      if Graphic_Mode then
        Screen.Play (Row, Remove);
      else
        Text.Play (Row, Remove);
      end if;
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
    if Graphic_Mode then
      Screen.Reset;
    end if;

    -- Show end of game, change game?
    if Graphic_Mode then
      Screen.End_Game (Result, Change_Game);
    else
      Text.End_Game (Result, Change_Game);
    end if;
    if Change_Game then
      -- Update game kind
      Common.Switch_Game_Kind;
      if Graphic_Mode then
        Screen.Reset;
      end if;
    end if;

  end loop One_Game;

exception
  when Screen.Exit_Requested =>
    null;
end Nimmari;

