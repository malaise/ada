with Space, Moon, Flight, Lem;
package Screen is

  -- Reset screen. Display titles and moon ground
  procedure Init (Ground : in Moon.Ground_Array);

  -- Lem position
  type Lem_Position (Set : Boolean := True) is record
    case Set is
      when True => Pos : Space.Position_Rec;
      when False => null;
    end case;
  end record;
  No_Position : constant Lem_Position := (Set => False);

  -- Update lem and its speed and Y_thrust
  procedure Update (Prev_Pos, New_Pos : in Lem_Position;
                    Speed : in Lem.Speed_Rec;
                    Y_Thrust : in Lem.Y_Thrust_Range);

  -- Put game end
  subtype End_Reason_List is Flight.Status_List
                             range Flight.Landed .. Flight.Lost;
  procedure Put_End (Reason : in End_Reason_List);

end Screen;

