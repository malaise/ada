with Ada.Calendar;
with Event_Mng;
separate (Mcd_Mng)
function  Do_Delay (The_Delay : Duration) return Delay_Status_List is
  Expiration : Ada.Calendar.Time;
  Timeout_Ms : Integer;
  use type Ada.Calendar.Time, Event_Mng.Out_Event_List;

  procedure Compute_Timeout is
  begin
    Timeout_Ms := Integer ((Expiration - Ada.Calendar.Clock) * 1000);
    if Timeout_Ms < 0 then
      Timeout_Ms := 0;
    end if;
  end Compute_Timeout;

begin
  Expiration := Ada.Calendar.Clock + The_Delay;
  Compute_Timeout;

  loop
    if Event_Mng.Wait (Timeout_Ms) = Event_Mng.Signal_Event then
      return Exit_Break;
    end if;
    Compute_Timeout;
    exit when Timeout_Ms = 0;
  end loop;
  return Continue;
end Do_Delay;

