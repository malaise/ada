with Ada.Text_Io;
With Normal, Lower_Str, Argument;

with Screen, Game, Debug, Connection;

package body Human is

  Color : Space.Color_List;
  The_End : Boolean := False;

  procedure Do_Play;
  procedure Do_Wait;

  procedure Play (Server_Name : in String; Color  : in Space.Color_List) is
    use type Space.Color_List; 
  begin
    Human.Color := Color;
    Connection.Init (Server_Name, Color);
    Connection.Wait_Ready;

    Game.Init (Color);
    
    if Color = Space.White then
      Do_Play;
    end if;
    if not The_End then
      loop
        Do_Wait;
        exit when The_End;
        Do_Play;
        exit when The_End;
      end loop;
    end if;

    Connection.Close;
    Screen.Close;
  end Play;

  -- Put action status. Ok means Exit
  procedure Put (Result : in Game.Move_Status_List) is
  begin
    case Result is
      when Game.Nok =>
        -- Our king would be in check
        Screen.Put (Color, "King would be in check.");
      when Game.Ok =>
        Screen.Put (Color, "Exit", True);
      when Game.Check =>
        Screen.Put (Color, "Check!");
      when Game.Stalemate =>
        Screen.Put(Color, "Stalemate??", True);
      when Game.Checkmate =>
        Screen.Put(Color, "Checkmate!!", True);
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
      Action := Screen.Get(Color);
      -- User exit?
      if not Action.Valid then
        The_End := True;
        exit Get_One;
      end if;
      if Debug.Some then
        Ada.Text_Io.Put (">> Playing: ");
        Debug.Put(Action);
        Ada.Text_Io.New_Line;
      end if;
      Result := Game.Do_Move (Action);
      exit Get_One when Result /= Game.Nok;
      -- Invalid move
      Put (Result);
    end loop Get_One;

    -- Send valid action
    Connection.Send (Action);
    -- Put any significant result
    if not The_End then
      Screen.Put_Move (Color, Action, Result);
    end if;
    if Result /= Game.Ok and then not The_End then
      Put (Result);
    end if;
    -- Exit or Check/Stale mate
    The_End := The_End
               or else Result = Game.Stalemate
               or else Result = Game.Checkmate;
    if Debug.Get (Debug.Human) then
      Ada.Text_Io.Put_Line ("Human end of play. The_End is "
                          & Boolean'Image (The_End));
    end if;
  end Do_Play;

  procedure Do_Wait is
    Result : Game.Move_Status_List;
    use type Game.Move_Status_List;

    Action : Players.Action_Rec;
  begin
    Screen.Reset_Time;

    if Debug.Get (Debug.Human) then
      Ada.Text_Io.Put_Line ("Human waiting");
    end if;
    -- Wait until opponent move
    loop
      exit when Connection.Action_Received;
      Screen.Wait (Color, 100);
    end loop;
    Action := Connection.Receive;
    if Debug.Get (Debug.Human) then
      Ada.Text_Io.Put_Line ("Human received move");
    end if;

    if not Action.Valid then
      -- Opponent exits
      Result := Game.Ok;
      Put (Result);
      The_End := True;
    else
      Result := Game.Do_Move (Action);
      Screen.Put_Move (Space.Opponent (Color), Action, Result);
      if Result /= Game.Ok then
        Put (Result);
      end if;
      The_End := Result = Game.Stalemate or else Result = Game.Checkmate;
    end if;
    if Debug.Get (Debug.Human) then
      Ada.Text_Io.Put_Line ("Human end of wait. The_End is "
                          & Boolean'Image (The_End));
    end if;
        
  end Do_Wait;


end Human;

