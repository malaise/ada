with Ada.Text_Io;
With Normal, Lower_Str, Argument;

with Screen, Game, Debug, Connection, File, Image;

package body Human is

  Default_File_Name : constant String := "Default.chs";

  Color : Space.Color_List;
  Move_Color : Space.Color_List;
  The_End : Boolean := False;
  Server : Boolean;

  procedure Do_Play;
  procedure Do_Wait;

  procedure Load_Moves;
  procedure Save_If_Server (Action : in Game.Valid_Action_Rec;
                            Result : in Game.Move_Status_List);

  procedure Play (Server_Name : in String;
                  File_Name   : in String;
                  Color  : in Space.Color_List) is
    use type Space.Color_List; 
  begin
    Server := Server_Name = "";
    Human.Color := Color;
    if Debug.Get (Debug.Human) then
      Ada.Text_Io.Put_Line ("Human start. Color is " & Space.Color_List'Image(Human.Color)
         & " and server is " & Boolean'Image(Server));
    end if;
    if Server then
      if File_Name = "" then
        -- No file name specified, overwrite default
        File.Delete (Default_File_Name);
        File.Open (Default_File_Name);
      else
        -- Load and append to specified file
        File.Open (File_Name);
      end if;
    end if;
    Connection.Init (Server_Name, Color);
    Connection.Wait_Ready;

    Game.Init (Color);

    Load_Moves;

    if Color = Move_Color and then not The_End then
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

    if Server then
      File.Close;
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
        Screen.Put (Color, "Your King would be in check.", True);
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
      Screen.Wait (Color);
      exit when Connection.Action_Received;
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
      Save_If_Server (Action, Result);
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

  procedure Load_Moves is
    Action : Players.Action_Rec;
    Result : Game.Move_Status_List;
    use type Game.Move_Status_List;
  begin
    Move_Color := Space.White;
    loop
      if Server then
        Action := File.Read;
        Connection.Send (Action);
      else
        loop
          Screen.Wait (Color);
          exit when Connection.Action_Received;
        end loop;
        Action := Connection.Receive;
      end if;
      if Action.Valid and then not Players.Action_Exists (Move_Color, Action) then
        Screen.Put (Color, "Invalid action", True);
        if Debug.Get (Debug.Human) then
          Ada.Text_Io.Put ("Loading invalid action ");
          Debug.Put (Action);
          Ada.Text_Io.New_Line;
        end if;
        raise Load_Error;
      end if;
 
      if Debug.Get (Debug.Human) then
        Ada.Text_Io.Put ("Loading ");
        Debug.Put (Action);
        Ada.Text_Io.New_Line;
      end if;
      exit when not Action.Valid;

      Result := Game.Do_Move (Action);
      -- Put any significant result
      Screen.Put_Move (Move_Color, Action, Result);
      if Result /= Game.Ok then
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
      Move_Color := Space.Opponent (Move_Color);
    end loop;
    if Debug.Get (Debug.Human) then
      Ada.Text_Io.Put_Line ("End of loading. Move is " & Space.Color_List'Image(Move_Color));
    end if;
  exception
    when Load_Error => 
      -- Send Exit
      Connection.Send ((Valid => False));
      Connection.Send ((Valid => False));
      raise;
  end Load_Moves;

  procedure Save_If_Server (Action : in Game.Valid_Action_Rec;
                            Result : in Game.Move_Status_List) is
  begin
    if Server then
      File.Write (Action, Result);
    end if;
  end Save_If_Server;

end Human;

