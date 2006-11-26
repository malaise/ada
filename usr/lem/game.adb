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
    use type Lem.Thrust_Range;
    X_Thrust_Increment : constant Lem.X_Thrust_Range
                       := Lem.Max_X_Thrust / 7;
    Y_Thrust_Increment : constant Lem.Y_Thrust_Range
                       := Lem.Max_Y_Thrust / 10;
    -- Current Y thrust
    Y_Thrust : Lem.Y_Thrust_Range;
    -- Chronometer
    Chrono : Chronos.Chrono_Type;
    use type Flight.Status_List;
  begin
    -- Start (new) game
    if New_Game then
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

    -- Init screen
    Screen.Init;
    Flight_Status := Flight.Get_Status;
    Screen.Update (Flight_Status, Chronos.Read (Chrono), True);

    -- Play
    loop
      -- Get flying status
      Flight_Status := Flight.Get_Status;

      -- Fly while flying or landed but still Y thrust
      if Flight_Status.Status /= Flight.Flying
      and then Flight_Status.Status /= Flight.Approaching
      and then Flight_Status.Status /= Flight.Landed then
        -- Crashed or lost
        exit;
      end if;

      -- Check if landed and no Y thrust
      Y_Thrust := Lem.Get_Y_Thrust;
      if Flight_Status.Status = Flight.Landed then
        if Y_Thrust = 0 then
          -- Landed and Ythrust off
          exit;
        end if;
        -- Landed but game not finished
        if not Lem.Is_Landed then
          -- Lem was not landed yet, tell it
          Lem.Set_Landed_Position (Flight_Status.Pos);
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
        when Screen.Other_Key =>
          -- Any other key: go on and return game status
          if Flight_Status.Status = Flight.Landed then
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

