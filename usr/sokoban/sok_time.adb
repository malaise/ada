with Calendar;
use Calendar;
with Sok_Display;
package body Sok_Time is

  Running : Boolean := False;
  Dur : Day_Duration := 0.0;
  Day : Natural := 0;
  Start_Clock : Calendar.Time;

  procedure Start_Time is
  begin
    Start_Clock := Calendar.Clock - Dur;
    Running := True;
  end Start_Time;

  procedure Stop_Time is
  begin
    Running := False;
  end Stop_Time;

  procedure Disp_Time is
  begin
    if Running then
      -- elapsed time
      declare 
        Current_Time : constant Calendar.Time := Calendar.Clock;
      begin
        if Current_Time > Start_Clock + Day_Duration'Last then
          Day := Day + 1;
          Dur := Current_Time - (Start_Clock + Day_Duration'Last);
          Start_Clock := Calendar.Clock;
        else
          Dur := Current_Time - Start_Clock;
        end if;
      end;
    end if;
    Sok_Display.Put_Time (Day, Dur);
  end Disp_Time;

  procedure Reset_Time is
  begin
    Start_Clock := Calendar.Clock;
    Dur := 0.0;
    Day := 0;
  end Reset_Time;

  procedure Set_Time (Day : Natural; Dur : Duration) is
  begin
    Start_Clock := Calendar.Clock - Dur;
    Sok_Time.Dur := Dur;
    Sok_Time.Day := Day;
  end Set_Time;

  procedure Get_Time (Day : out Natural; Dur : out Duration) is
  begin
    Day := Sok_Time.Day;
    Dur := Sok_Time.Dur;
  end Get_Time;

end Sok_Time;

