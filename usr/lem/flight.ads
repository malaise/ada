with Space, Lem;
package Flight is

  -- Max horizontal and vertical speed
  Max_Horiz_Speed : constant Lem.Speed_Range := 1.0;
  Max_Verti_Speed : constant Lem.Speed_Range := 5.0;

  -- Get a valid init position for the LEM
  function Get_Init_Position return Space.Position_Rec;

  -- Kind of LEM flight status
  type Status_List is (Flying, Landed, Crashed, Lost);
  -- LEM status, postion and speed
  type Status_Rec is record
      Status : Status_List;
      Pos    : Space.Position_Rec;
      Speed  : Lem.Speed_Rec;
  end record;

  -- Get current LEM status
  function Get_Status return Status_Rec;

end Flight;

