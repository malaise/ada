with Ada.Finalization;
with Chronos, Timers;
package Chronos.Passive_Timers is

  type Passive_Timer is tagged limited private;

  -- Timer status, independant from the associated clock status
  subtype Timer_Status is Timers.Timer_Status;

  function Status (Timer : Passive_Timer) return Timer_Status;
  -- True if timer is not Deleted
  function Exists (Timer : Passive_Timer) return Boolean;

  -- Arm a passive timer with a given period
  -- Overwrites any previous setting on this timer
  -- May raise Invalid_Delay if Delay_Seconds is < 0
  Invalid_Delay : exception;
  procedure Start (Timer      : in out Passive_Timer;
                   Delay_Spec : in Timers.Delay_Rec);

  -- Stop a timer, which becomes unusable (deleted) until re-started
  Timer_Stopped : exception;
  procedure Stop (Timer : in out Passive_Timer);

  -- Suspend a timer: pending expirations can still be retrieved
  -- No action if timer is alread syspended
  procedure Suspend (Timer : in out Passive_Timer);

  -- Resume a suspended a timer: new expirations are resumed
  -- No action if timer is not syspended
  procedure Resume (Timer : in out Passive_Timer);

  -- Checks if timer expiration time (Prev_Exp + Period) is reached
  -- If yes, add Period to expiration time
  function Has_Expired (Timer : Passive_Timer) return Boolean;

  -- When a timer has expired once and has no period (0.0) it is not re-armed
  -- Calling Suspend, Resume or Has_Expired again on it will raise:
  Timer_Expired : exception;

private
  type Timer_Rec is record
    Period : Timers.Period_Range;
    Next_Expiration : Chronos.Time_Rec;
    Chrono : Chronos.Chrono_Type;
    Expired : Boolean;
  end record;
  type Timer_Access is access Timer_Rec;

  -- So timer can be In for function Has_Expired
  type Passive_Timer is new Ada.Finalization.Limited_Controlled with record
    Acc : Timer_Access;
  end record;

  overriding procedure Finalize (Timer : in out Passive_Timer);

end Chronos.Passive_Timers;

