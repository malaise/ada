with Ada.Finalization, Ada.Calendar;
package Passive_Timers is

  type Passive_Timer is tagged limited private;

  -- Arm a passive timer with a given period
  -- Overwrites any previous setting on this timer
  -- Raises Invalid_Period if Period is <= 0.0
  procedure Arm (Timer : in out Passive_Timer;
                 Period : in Duration);

  -- Checks if timer expiration time (Prev_Exp + Period) is reached
  -- If yes, add Period to expiration time
  function Has_Expired (Timer : in Passive_Timer) return Boolean;

  Invalid_Period : exception;

private
  Not_Started : constant Duration := 0.0;
  type Timer_Rec is record
    Period : Duration := Not_Started;
    Next_Expiration : Ada.Calendar.Time;
  end record;
  type Timer_Access is access Timer_Rec;

  -- So timer can be In for function Has_Expired
  type Passive_Timer is new Ada.Finalization.Limited_Controlled with record
    Acc : Timer_Access := new Timer_Rec;
  end record;

  procedure Finalize (Timer : in out Passive_Timer);

end Passive_Timers;

