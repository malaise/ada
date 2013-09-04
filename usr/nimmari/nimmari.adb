-- The games of NIM and Marienbad, graphical and text version.
-- Remove sticks alternatively with the computer
-- Remove the last stick (in NIM) or oblige the computer to do so (Marienbad)
with As.U, Argument, Argument_Parser, Basic_Proc;
with Common, Screen, Text, Compute, Trace;
procedure Nimmari is
  Logger : Trace.Logger;
  Graphic_Mode : Boolean := False;

  -- Result of computation
  Row    : Common.Row_Range;
  Remove : Common.Bar_Status_Array;
  Result : Common.Result_List;

  -- Change game after end of a game
  Change_Game : Boolean;

  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => (False, 'h', As.U.Tus ("help"), False),
   02 => (False, 'n', As.U.Tus ("nim"), False),
   03 => (False, 'm', As.U.Tus ("marienbad"), False),
   04 => (False, 't', As.U.Tus ("text"), False));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        & " [ " & Argument_Parser.Image(Keys(4)) & " ] [ <nim> | <marienbad>]");
    Basic_Proc.Put_Line_Error ("   or: " & Argument.Get_Program_Name
        & Argument_Parser.Image(Keys(1)));
    Basic_Proc.Put_Line_Error (" <nim>       ::= "
        & Argument_Parser.Image(Keys(2)));
    Basic_Proc.Put_Line_Error (" <marienbad> ::= "
        & Argument_Parser.Image(Keys(3)));
  end Usage;

  procedure Put (Bars : Common.Bar_Status_Array) is
    Txt : As.U.Asu_Us;
  begin
    if not Logger.Debug_On then
      return;
    end if;
    for I in Bars'Range loop
      if Bars(I) then
        Txt.Append ("I ");
      else
        Txt.Append ("- ");
      end if;
    end loop;
    Logger.Log_Debug (Txt.Image);
  end Put;

  use type Common.Result_List;
begin
  Logger.Init;
  -- Parse arguments
  Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": Syntax ERROR. "
      & Arg_Dscr.Get_Error & ".");
    Usage;
    return;
  end if;

  if Arg_Dscr.Is_Set (1) then
    -- Help
    Usage;
    return;
  end if;

  if Arg_Dscr.Is_Set (2) and then Arg_Dscr.Is_Set (3) then
     Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": Syntax ERROR.");
    Usage;
    return;
  end if;

  -- Text mode
  Graphic_Mode := not Arg_Dscr.Is_Set (4);

  -- Init then select game: Nim or Marienbad
  Compute.Init;
  if Arg_Dscr.Is_Set (2) then
    -- Nim
    Common.Set_Game_Kind (Common.Nim);
  elsif Arg_Dscr.Is_Set (3) then
    -- Marienbad
    Common.Set_Game_Kind (Common.Marienbad);
  elsif Graphic_Mode then
    -- Graphic choice
    Common.Set_Game_Kind (Screen.Intro);
  else
    -- Text choice
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
      Logger.Log_Debug ("Machine played: Row" & Row'Img & " ");
      Put (Remove);
      Logger.Log_Debug (" Result -> " & Result'Img);

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
      Logger.Log_Debug ("Human played: Row" & Row'Img & " ");
      Put (Remove);
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
  when Common.Exit_Requested =>
    null;
end Nimmari;

