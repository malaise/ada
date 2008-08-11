with Ada.Text_Io;
with Event_Mng, Chronos;
with Space, Screen, Flight, Moon, Lem, Debug;
package body Game is

  -- Lem initial position (re-used when prev game lost)
  Init_Position : Space.Position_Rec := Flight.Get_Init_Position;
  Init_Speed : Lem.Speed_Rec;

  function Play_One (New_Game : in Boolean) return Result_List is
    -- Flight (Lem) status
    Flight_Status : Flight.Status_Rec;
    -- Key reading result
    Get_Status : Screen.Got_List;
    use type Lem.Thrust_Range, Lem.Mass_Range;
    -- Tuning
    X_Thrust_Increment : constant Lem.X_Thrust_Range
                       := Lem.Max_X_Thrust / 20;
    Y_Thrust_Increment : constant Lem.Y_Thrust_Range
                       := Lem.Max_Y_Thrust / 15;
    -- Current Y thrust
    Y_Thrust : Lem.Y_Thrust_Range;
    -- Chronometer
    Chrono : Chronos.Chrono_Type;
    -- Landing status
    Land_Status : Flight.Status_List;
    -- Worst landing satus
    Worst_Landing : Flight.Status_List;
    use type Flight.Status_List;
    function Is_Landed (Status : Flight.Status_List) return Boolean is
    begin
      return Status = Flight.Landed or else Status = Flight.Safe_Landed;
    end Is_Landed;
    function Is_Failed (Status : Flight.Status_List) return Boolean is
    begin
      return Status = Flight.Crashed or else Status = Flight.Lost;
    end Is_Failed;
  begin
    -- Start (new) game
    if New_Game then
      if Debug.Set_Game then
        Ada.Text_Io.Put_Line ("GAME: Starting new");
      end if;
      -- Init Moon ground
      Moon.Init;
      -- Get a new random Lem position
      Init_Position := Flight.Get_Init_Position;
      -- Get a new random Lem vertical speed
      Init_Speed := Lem.Get_Init_Speed;
    end if;
    -- Init Lem and start chrono
    Lem.Init (Init_Position, Init_Speed);
    Chronos.Start (Chrono);

    -- Worst of all landings
    Worst_Landing := Flight.Safe_Landed;

    -- Init screen
    Screen.Init;
    Flight_Status := Flight.Get_Status;
    Screen.Update (Flight_Status, Chronos.Read (Chrono), True);
    if Debug.Set_Game then
      Ada.Text_Io.Put_Line ("GAME: Init done");
    end if;

    -- Play
    loop
      -- Get flying status
      Flight_Status := Flight.Get_Status;
      if Debug.Set_Game then
        Ada.Text_Io.Put_Line ("GAME: Status is " & Flight_Status.Status'Img);
        Ada.Text_Io.Put_Line ("GAME: Pos is " & Flight_Status.Pos.X_Pos'Img
                                        & "/" & Flight_Status.Pos.Y_Pos'Img);
        Ada.Text_Io.Put_Line ("GAME: Speed is "
                        & Flight_Status.Speed.X_Speed'Img
                  & "/" & Flight_Status.Speed.Y_Speed'Img);
      end if;

      -- Fly while flying or landed but still Y thrust
      if Is_Failed (Flight_Status.Status) then
        -- Crashed or lost
        exit;
      end if;

      -- Check if landed and no Y thrust
      Y_Thrust := Lem.Get_Y_Thrust;
      if Is_Landed (Flight_Status.Status) then
        -- Landed but game maybe not finished
        if not Lem.Is_Landed then
          -- Lem was not landed yet, tell it
          Lem.Set_Landed_Position (Flight_Status.Pos);
          -- Initial ground contact, save status
          --  (because further status will be safe_land)
          Land_Status := Flight_Status.Status;
          if Land_Status = Flight.Landed
          and then Worst_Landing = Flight.Safe_Landed then
            -- Previous landing (if any) was safe but this one is not
            Worst_Landing := Flight.Landed;
          end if;
          -- landing is safe if at least 10% fuel remaining
          if Lem.Get_Fuel < Lem.Max_Fuel / 10.0 then
            Worst_Landing := Flight.Landed;
          end if;
        end if;
        if Y_Thrust < Lem.Max_Y_Thrust / 20 then
          -- Landed and Ythrust off => end game
          -- Overall landing result is the worst of all
          Flight_Status.Status := Worst_Landing;
          exit;
        end if;
      end if;

      -- Get Lem characteristics and put
      Screen.Update (Flight_Status, Chronos.Read (Chrono), False);

      -- Get a key or wait a bit
      Get_Status := Screen.Get_Key (0.1);
      -- Handle key
      case Get_Status is
        when Screen.Right_Key =>
          -- Push right
          Lem.Set_X_Thrust (-X_Thrust_Increment);
        when Screen.Left_Key =>
          -- Push left
          Lem.Set_X_Thrust (X_Thrust_Increment);
        when Screen.Super_Right_Key =>
          -- Super push right
          Lem.Set_X_Thrust (- X_Thrust_Increment * 5);
        when Screen.Super_Left_Key =>
          -- Super push left
          Lem.Set_X_Thrust (X_Thrust_Increment * 5);
        when Screen.Up_Key =>
          -- Push less, down to 0
          if Y_Thrust > Y_Thrust_Increment then
            Y_Thrust := Y_Thrust - Y_Thrust_Increment;
          else
            Y_Thrust := 0;
          end if;
          Lem.Set_Y_Thrust (Y_Thrust);
        when Screen.Down_Key =>
          -- Push more, up to max
          Y_Thrust := Lem.Get_Y_Thrust;
          if Y_Thrust < Lem.Max_Y_Thrust - Y_Thrust_Increment then
            Y_Thrust := Y_Thrust + Y_Thrust_Increment;
          else
            Y_Thrust := Lem.Max_Y_Thrust;
          end if;
          Lem.Set_Y_Thrust (Y_Thrust);
        when Screen.Super_Up_Key =>
          -- Y thrust to 0
          Y_Thrust := 0;
          Lem.Set_Y_Thrust (Y_Thrust);
        when Screen.Super_Down_Key =>
          -- Y thrust to Max
          Y_Thrust := Lem.Max_Y_Thrust;
          Lem.Set_Y_Thrust (Y_Thrust);
        when Screen.Other_Key =>
          -- Ignore any other key
          null;
        when Screen.Break =>
          -- Abort
          Screen.Close;
          return Aborted;
        when Screen.Timeout =>
          -- Re-loop
          null;
        when Screen.Refresh =>
          -- Refresh screen
          Screen.Refresh;
      end case;
    end loop;

    -- Game is ended
    -- Stop the LEM and chrono
    Lem.Stop;
    Chronos.Stop (Chrono);

    -- Last display and get key
    loop
      if Flight_Status.Status = Flight.Lost then
        -- Lem lost: hide it
        Screen.Delete (Flight_Status, Chronos.Read (Chrono));
      else
        -- Landed or crashed: show it
        Screen.Update (Flight_Status, Chronos.Read (Chrono), True);
      end if;
      Screen.Put_End (Flight_Status.Status);

      Get_Status := Screen.Get_Key (-1.0);
      case Get_Status is
        when Screen.Break =>
          -- Abort
          Screen.Close;
          return Aborted;
        when Screen.Refresh =>
          -- Refresh screen
          Screen.Refresh;
        when Screen.Timeout =>
          -- Should not occure
          null;
        when Screen.Other_Key | Screen.Left_Key | Screen.Right_Key =>
          -- Any other key: go on and return game status
          if Is_Landed (Flight_Status.Status) then
            return Landed;
          else
            return Lost;
          end if;
        when others =>
          -- Arrows (remaining events): ignore
          null;
      end case;
    end loop;

  end Play_One;

end Game;

