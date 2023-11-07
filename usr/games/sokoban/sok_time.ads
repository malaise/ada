with Ada.Calendar;
package Sok_Time is

  -- Just start and stop the clock
  procedure Start_Time;
  procedure Stop_Time;

  procedure Disp_Time;

  -- Resets or sets the clock
  procedure Reset_Time;
  procedure Set_Time (Day : Natural; Dur : Ada.Calendar.Day_Duration);
  -- Get current clock value
  procedure Get_Time (Day : out Natural; Dur : out Ada.Calendar.Day_Duration);

end Sok_Time;
