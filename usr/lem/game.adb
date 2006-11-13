with Screen, Flight, Moon, Lem;
package body Game is

  function Play_One return Boolean is
    Flight_Status : Flight.Status_Rec;
    Prev_Pos, New_Pos : Screen.Lem_Position;
    use type Flight.Status_List;
  begin
    -- Init Moon ground
    Moon.Init;
    -- Init Lem at a valid position
    Prev_Pos := Screen.No_Position;
    New_Pos := (Set => True, Pos => Flight.Get_Init_Position);
    Lem.Init (New_Pos.Pos);
    -- Init screen
    Screen.Init (Moon.Get_Ground);
    Screen.Update (Prev_Pos, New_Pos, Lem.Get_Speed, Lem.Get_Y_Thrust);
    Prev_Pos := New_Pos;
    loop
      -- Get Lem chracteristics and put
      New_Pos.Pos := Lem.Get_Position;
      Screen.Update (New_Pos, Prev_Pos, Lem.Get_Speed, Lem.Get_Y_Thrust);
      Prev_Pos := New_Pos;
      -- Get flying status
      Flight_Status := Flight.Get_Status;
      exit when Flight_Status.Status /= Flight.Flying;
    end loop;
    -- Stop all
    Lem.Stop;
    New_Pos := Screen.No_Position;
    Screen.Update (New_Pos, Prev_Pos, Speed => (0.0, 0.0), Y_Thrust => 0);
    return True;
  end Play_One;

end Game;

