with Con_Io;
with Space, Flight, Lem;
package Screen is

  -- Reset screen. Display titles and moon ground
  procedure Init;

  -- Close (definitively)
  procedure Close;

  -- Redisplay moon ground, frame...
  procedure Refresh;

  -- Update lem and the gauges
  procedure Update (Flight_Status : in Flight.Status_Rec;
                    Update_Gauges : in Boolean);

  -- Delete lem and show the gauges
  procedure Delete (Flight_Status : in Flight.Status_Rec);

  -- Put game end
  subtype End_Reason_List is Flight.Status_List
                             range Flight.Landed .. Flight.Lost;
  procedure Put_End (Reason : in End_Reason_List);

  -- Get a key
  type Got_List is (Right_Key, Left_Key, Up_Key, Down_Key, Break,
                    Other_Key, Timeout, Refresh);
  function Get_Key (Wait : in Duration) return Got_List;

  -- Check if two heights are the same on screen
  --  (to be used as a "flat" ground criteria)
  function Same_Height (A, B : Space.Position_Range) return Boolean;

end Screen;

