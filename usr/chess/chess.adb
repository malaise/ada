with Ada.Text_Io;
With Normal, Lower_Str, Argument;

with Pieces, Space.Board, Players, Screen, Game, Debug;

procedure Chess is

  Invalid_Argument : Exception;

  Action : Players.Action_Rec;
  Color : Space.Color_List;

  procedure Parse_Args is
    procedure Parse_Debug (Flag : in String; Option : in Debug.Debug_List) is
      Debug_Key : constant String := "D";
    begin
      if Argument.Get_Parameter (1, Debug_Key & Flag) = "" then
        Debug.Set (Option, True);
      else
        raise Invalid_Argument;
      end if;
    exception
      when Argument.Argument_Not_Found =>
        null;
    end Parse_Debug;
  begin
    Parse_Debug ("M", Debug.Moves);
    Parse_Debug ("T", Debug.Think);
    Parse_Debug ("N", Debug.No_Check);
  end Parse_Args;

begin
  Parse_Args;

  -- Init game: Set board, think and display
  Space.Board.Init;
  Game.Init;

  Color := Space.White;

  One_Game:
  loop

    if Debug.Get (Debug.Moves) then
      -- Dump Actions
      Players.Rewind_Actions (Color);
      loop
        Action := Players.Next_Action (Color);
        exit when not Action.Valid;
        Debug.Put (Action);
        Ada.Text_Io.New_Line;
      end loop;
      Ada.Text_Io.New_Line;
    end if;

    Get_One:
    loop
      declare
        Result : Game.Move_Status_List;
        use type Game.Move_Status_List;
      begin
        Action := Screen.Get(Color);
        -- User exit?
        exit One_Game when not Action.Valid;
        if Debug.Some then
          Ada.Text_Io.Put (">> Playing: ");
          Debug.Put(Action);
          Ada.Text_Io.New_Line;
        end if;
        Result := Game.Do_Move (Action);
        case Result is
          when Game.Nok =>
            --  Our king would be in check
            Screen.Put (Color, "King would be in check.");
          when Game.Ok =>
            exit Get_One;
          when Game.Check =>
            Screen.Put (Color, "Check!");
            exit Get_One;
          when Game.Stalemate =>
            Screen.Put(Color, "Stalemate??", True);
            exit One_Game;
          when Game.Checkmate =>
            Screen.Put(Color, "Checkmate!!", True);
            exit One_Game;
        end case;
      end;
    end loop Get_One;

    -- New player
    Color := Space.Opponent (Color);
  end loop One_Game;

  Screen.Close;

end Chess;

