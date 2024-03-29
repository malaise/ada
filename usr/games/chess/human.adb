with Basic_Proc;
with Pieces, Space.Board, Screen, Game, Debug, Connection, File, Players,
     Set_Up;

package body Human is

  Default_File_Name : constant String := "Default.chs";

  Mode : Play_Mode;
  Color : Space.Color_List;

  Move_Color : Space.Color_List;
  The_End : Boolean := False;

  procedure Do_Play;
  procedure Do_Wait;

  procedure Load_Moves (Wait : in Boolean);
  procedure Save_If_Server (Action : in Game.Valid_Action_Rec;
                            Result : in Game.Move_Status_List);

  procedure Play (Mode   : in Play_Mode;
                  Color  : in Space.Color_List;
                  Name   : in String;
                  Port   : in Socket_Util.Remote_Port;
                  Setup  : in String;
                  Wait   : in Boolean) is
    use type Space.Color_List;
  begin
    Human.Mode := Mode;
    Human.Color := Color;
    if Debug.Get (Debug.Human) then
      Basic_Proc.Put_Line_Output ("Human start in mode " & Play_Mode'Image(Mode)
         & " with color " & Space.Color_List'Image(Color));
    end if;

    -- File
    if Mode /= Client then
      if Name = "" then
        -- No file name specified, overwrite default
        File.Delete (Default_File_Name);
        File.Open (Default_File_Name);
      else
        -- Load and append to specified file
        File.Open (Name);
      end if;
    end if;

    -- Init
    Space.Board.Init;
    Move_Color := Space.White;
    if Mode /= Client and then Setup /= "" then
      Space.Board.Erase;
      begin
        Move_Color := Set_Up.Load (Setup);
      exception
        when Set_Up.Load_Error =>
          raise Load_Error;
      end;
    end if;

    -- Client/Server
    if Mode /= Both then
      if Mode = Server then
        Connection.Init ("", Port, Color);
      else
        Connection.Init (Name, Port, Color);
      end if;
      Connection.Wait_Ready;
    end if;

    -- Init
    begin
      Game.Init (Color);
    exception
      when others =>
        if Mode /= Client and then Setup /= "" then
          raise Load_Error;
        else
          raise;
        end if;
    end;

    Load_Moves (Wait);

    -- Black move first
    if Setup /= "" and then Move_Color = Space.Black then
      File.Write ((Valid => False), Game.Nok);
    end if;

    -- Play loop
    while not The_End loop
      if Mode = Both or else Move_Color = Color then
        Do_Play;
      else
        Do_Wait;
      end if;
      Move_Color := Space.Opponent (Move_Color);
    end loop;

    -- The End
    if Mode /= Client then
      File.Close;
    end if;
    if Mode /= Both then
      Connection.Close;
      Screen.Wait (0.2);
    end if;
    Screen.Close;
  end Play;

  -- Put action status. Ok means Exit
  procedure Put (Result : in Game.Move_Status_List) is
    Ink_Color : Space.Color_List;
  begin
    if Mode = Both then
      Ink_Color := Move_Color;
    else
      Ink_Color := Color;
    end if;
    case Result is
      when Game.Nok =>
        -- Our king would be in check
        Screen.Put (Color, Ink_Color,
                    "Your King would be in check.");
      when Game.Ok =>
        Screen.Put (Color, Ink_Color, "Exit", True);
      when Game.Check =>
        Screen.Put (Color, Ink_Color, "Check!");
      when Game.Stalemate =>
        Screen.Put (Color, Ink_Color, "Stalemate??", True);
      when Game.Checkmate =>
        Screen.Put (Color, Ink_Color, "Checkmate!!", True);
    end case;
  end Put;


  procedure Do_Play is
    Action : Players.Action_Rec;
    Result : Game.Move_Status_List;
    use type Game.Move_Status_List;
  begin
    Screen.Reset_Time;

    if Debug.Get (Debug.Moves) then
      -- Dump Actions
      Players.Rewind_Actions (Move_Color);
      loop
        Action := Players.Next_Action (Move_Color);
        exit when not Action.Valid;
        Debug.Put (Action);
        Basic_Proc.New_Line_Output;
      end loop;
      Basic_Proc.New_Line_Output;
    end if;


    Get_One:
    loop
      Action := Screen.Get(Color, Move_Color);
      -- User exit?
      if not Action.Valid then
        The_End := True;
        exit Get_One;
      end if;
      if Debug.Some_Is then
        Basic_Proc.Put_Output (">> " & Space.Color_List'Image(Move_Color)
                       & " Playing: ");
        Debug.Put(Action);
        Basic_Proc.New_Line_Output;
      end if;
      Result := Game.Do_Move (Action);
      exit Get_One when Result /= Game.Nok;
      -- Invalid move
      Put (Result);
    end loop Get_One;

    -- Send valid action
    if Mode /= Both then
      Connection.Send (Action);
    end if;
    -- Put any significant result
    if not The_End then
      Screen.Put_Move (Move_Color, Action, Result);
      Save_If_Server (Action, Result);
    end if;
    if Result /= Game.Ok and then not The_End then
      Put (Result);
    end if;
    -- Exit or Check/Stale mate
    The_End := The_End
               or else Result = Game.Stalemate
               or else Result = Game.Checkmate;
    if Debug.Get (Debug.Human) then
      Basic_Proc.Put_Line_Output ("Human end of play. The_End is "
                          & Boolean'Image (The_End));
    end if;
  end Do_Play;

  procedure Do_Wait is
    Action : Players.Action_Rec;
    Result : Game.Move_Status_List;
    use type Game.Move_Status_List;

  begin
    Screen.Reset_Time;

    if Debug.Get (Debug.Human) then
      Basic_Proc.Put_Line_Output ("Human waiting");
    end if;
    -- Wait until opponent move
    loop
      Screen.Wait_Event (Color, Move_Color);
      exit when Connection.Action_Received;
    end loop;
    Action := Connection.Receive;
    if Debug.Get (Debug.Human) then
      Basic_Proc.Put_Line_Output ("Human received move");
    end if;

    if not Action.Valid then
      -- Opponent exits
      Result := Game.Ok;
      Put (Result);
      The_End := True;
    else
      Result := Game.Do_Move (Action);
      Screen.Put_Move (Space.Opponent (Color), Action, Result);
      Save_If_Server (Action, Result);
      if Result /= Game.Ok then
        Put (Result);
      end if;
      The_End := Result = Game.Stalemate or else Result = Game.Checkmate;
    end if;
    if Debug.Get (Debug.Human) then
      Basic_Proc.Put_Line_Output ("Human end of wait. The_End is "
                          & Boolean'Image (The_End));
    end if;

  end Do_Wait;

  procedure Load_Moves (Wait : in Boolean) is
    Action : Players.Action_Rec;
    Result : Game.Move_Status_List;
    Piece  : Pieces.Piece_Access;
    use type Pieces.Piece_Kind_List, Pieces.Piece_Access;
    use type Game.Move_Status_List;
  begin
    loop
      if Mode /= Client then
        Action := File.Read;
        if Mode = Server then
          Connection.Send (Action);
        end if;
      else
        loop
          Screen.Wait_Event (Color, Move_Color);
          exit when Connection.Action_Received;
        end loop;
        Action := Connection.Receive;
      end if;
      if Action.Valid then
        -- Check piece and movement
        Piece := Space.Board.Piece_At (Action.From);
        if not Players.Action_Exists (Move_Color, Action)
        or else Piece = null
        or else Pieces.Id_Of (Piece.all).Kind /=  Action.Piece then
          Screen.Put (Color, Move_Color, "Invalid action", True);
          if Debug.Get (Debug.Human) then
            Basic_Proc.Put_Output ("Loading invalid action ");
            Debug.Put (Action);
            Basic_Proc.New_Line_Output;
          end if;
          raise Load_Error;
        end if;
      end if;

      if Debug.Get (Debug.Human) then
        Basic_Proc.Put_Output ("Loading ");
        Debug.Put (Action);
        Basic_Proc.New_Line_Output;
      end if;
      exit when not Action.Valid;

      Result := Game.Do_Move (Action);
      -- Put any significant result
      Screen.Put_Move (Move_Color, Action, Result);
      if Result /= Game.Ok and then Result /= Game.Check then
        Put (Result);
      end if;
      if Result = Game.Nok then
        raise Load_Error;
      end if;
      -- Exit or Check/Stale mate
      The_End := The_End
                 or else Result = Game.Stalemate
                 or else Result = Game.Checkmate;
      exit when The_End;
      if Mode /= Client and then Wait then
        Screen.Put (Color, Move_Color, "Next", True);
      end if;
      Move_Color := Space.Opponent (Move_Color);
    end loop;
    if Debug.Get (Debug.Human) then
      Basic_Proc.Put_Line_Output ("End of loading. Move is "
          & Space.Color_List'Image(Move_Color));
    end if;
  exception
    when Load_Error =>
      -- Send Exit
      if Mode /= Both then
        Connection.Send ((Valid => False));
        Connection.Send ((Valid => False));
      end if;
      raise;
  end Load_Moves;

  procedure Save_If_Server (Action : in Game.Valid_Action_Rec;
                            Result : in Game.Move_Status_List) is
  begin
    if Mode /= Client then
      File.Write (Action, Result);
    end if;
  end Save_If_Server;

end Human;

