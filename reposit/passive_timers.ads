with Ada.Finalization;
with Virtual_Time, Chronos;
package Passive_Timers is

  type Passive_Timer is tagged limited private;

  -- Arm a passive timer with a given period
  -- Overwrites any previous setting on this timer
  -- Raises Invalid_Period if Period is <= 0.0
  procedure Arm (Timer  : in out Passive_Timer;
                 Period : in Duration;
                 Clock  : in Virtual_Time.Clock_Access := null);

  -- Stop a timer, which becomes unusable until re-armed
  Timer_Stopped : exception;
  procedure Stop (Timer : in out Passive_Timer);

  -- Suspend a timer: pending expirations can still be retrieved
  -- No action is timer is alread syspended
  procedure Suspend (Timer : in out Passive_Timer);

  -- Resume a suspended a timer: new expirations are resumed
  -- No action is timer is not syspended
  procedure Resume (Timer : in out Passive_Timer);

  -- Checks if timer expiration time (Prev_Exp + Period) is reached
  -- If yes, add Period to expiration time
  function Has_Expired (Timer : in Passive_Timer) return Boolean;

  Invalid_Period : exception;

private
  type Timer_Rec is record
    Period : Duration;
    Next_Expiration : Chronos.Time_Rec;
    Chrono : Chronos.Chrono_Type;
  end record;
  type Timer_Access is access Timer_Rec;

  -- So timer can be In for function Has_Expired
  type Passive_Timer is new Ada.Finalization.Limited_Controlled with record
    Acc : Timer_Access;
  end record;

  procedure Finalize (Timer : in out Passive_Timer);

end Passive_Timers;

