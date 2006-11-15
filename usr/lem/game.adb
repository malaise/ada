with Ada.Text_Io;
with Event_Mng;
with Screen, Flight, Moon, Lem, Debug;
package body Game is

  function Play_One return Boolean is
    Flight_Status : Flight.Status_Rec;
    Get_Status : Screen.Got_List;
    use type Lem.Thrust_Range;
    X_Thrust_Increment : constant Lem.X_Thrust_Range
                       := Lem.Max_X_Thrust / 2;
    Y_Thrust_Increment : constant Lem.Y_Thrust_Range
                       := Lem.Max_Y_Thrust / 10;
    Y_Thrust : Lem.Y_Thrust_Range;
    use type Flight.Status_List;
  begin
    -- Init Moon ground
    Moon.Init;
    -- Init Lem at a valid position
    Lem.Init (Flight.Get_Init_Position);

    -- Init screen
    Screen.Init;
    Screen.Update (Lem.Get_Position, Lem.Get_Speed);

    -- Play
    loop
      -- Get flying status
      Flight_Status := Flight.Get_Status;
      exit when Flight_Status.Status /= Flight.Flying;
      -- Get Lem characteristics and put
      Screen.Update (Flight_Status.Pos, Flight_Status.Speed);
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
          Y_Thrust := Lem.Get_Y_Thrust;
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
          return False;
        when Screen.Timeout =>
          -- Re-loop
          null;
        when Screen.Refresh =>
          -- Refresh screen
          Screen.Refresh;
      end case;
    end loop;

    -- Game is ended
    -- Stop the LEM
    Lem.Stop;

    -- Last display and get key
    loop
      if Flight_Status.Status = Flight.Lost then
        -- Lem lost: hide it
        Screen.Delete (Flight_Status.Speed);
      else
        -- Landed or crashed: show it
        Screen.Update (Flight_Status.Pos, Flight_Status.Speed);
      end if;
      Screen.Put_End (Flight_Status.Status);

      Get_Status := Screen.Get_Key (-1.0);
      case Get_Status is
        when Screen.Break =>
          -- Abort
          Screen.Close;
          return False;
        when Screen.Refresh =>
          -- Refresh screen
          Screen.Refresh;
        when Screen.Timeout =>
          -- Should not occure
          null;
        when Screen.Other_Key =>
          -- Any other key: go on
          return True;
        when others =>
          -- Arrows (remaining events): ignore
          null;
      end case;
    end loop;

  end Play_One;

end Game;

