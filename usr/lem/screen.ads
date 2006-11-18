with Con_Io;
with Flight, Lem;
package Screen is

  -- Reset screen. Display titles and moon ground
  procedure Init;

  -- Close (definitively)
  procedure Close;

  -- Redisplay moon ground, frame...
  procedure Refresh;

  -- Update lem and the gauges
  procedure Update (New_Pos : in Lem.Position_Rec;
                    New_Speed : in Lem.Speed_Rec;
                    Update_Gauges : in Boolean);

  -- Delete lem and show the gauges
  procedure Delete (New_Speed : in Lem.Speed_Rec);

  -- Put game end
  subtype End_Reason_List is Flight.Status_List
                             range Flight.Landed .. Flight.Lost;
  procedure Put_End (Reason : in End_Reason_List);

  -- Get a key
  type Got_List is (Right_Key, Left_Key, Up_Key, Down_Key, Break,
                    Other_Key, Timeout, Refresh);
  function Get_Key (Wait : in Duration) return Got_List;

end Screen;

