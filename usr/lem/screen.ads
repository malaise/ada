with Con_Io, Chronos;
with Space, Flight, Lem;
package Screen is

  -- Reset screen. Display titles and moon ground
  procedure Init;

  -- Close (definitively)
  procedure Close;

  -- Redisplay moon ground, frame...
  procedure Refresh;

  -- Update lem and show the gauges and time
  procedure Update (Flight_Status : in Flight.Status_Rec;
                    Elapsed_Time  : in Chronos.Date_Rec;
                    Update_Gauges : in Boolean);

  -- Delete lem and show the gauges and time
  procedure Delete (Flight_Status : in Flight.Status_Rec;
                    Elapsed_Time  : in Chronos.Date_Rec);

  -- Put game end
  subtype End_Reason_List is Flight.Status_List
                             range Flight.Landed .. Flight.Lost;
  procedure Put_End (Reason : in End_Reason_List);

  -- Put message when paused
  procedure Put_Pause;

  -- Get an event
  type Evt_Kind_List is (Move_Key, Move_Click, Break, Next, Prev,
                         Timeout, Refresh, Pause);
  type Mvt_Kind_List is (Right_Key, Left_Key, Super_Right_Key, Super_Left_Key,
                         Up_Key, Down_Key, Super_Up_Key, Super_Down_Key);
  type Evt_Rec (Evt : Evt_Kind_List := Refresh) is record
    case Evt is
      when Move_Key | Move_Click =>
        Mvt : Mvt_Kind_List;
      when others =>
        null;
    end case;
  end record;

  function Get_Event (Wait : in Duration) return Evt_Rec;

  -- Check if two heights are the same on screen
  --  (to be used as a "flat" ground criteria)
  function Same_Height (A, B : Space.Position_Range) return Boolean;

end Screen;

