with Chronos, Timers;
package Chronos.Passive_Timers is

  type Passive_Timer is tagged limited private;

  -- Timer status, independant from the associated clock status
  subtype Timer_Status is Timers.Timer_Status;
  Stopped : constant Timer_Status := Timers.Timer_Status'(Timers.Deleted);

  function Status (Timer : Passive_Timer) return Timer_Status;
  -- True if timer is not Deleted (not Stopped)
  function Running (Timer : Passive_Timer) return Boolean;

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
  -- No action if timer is not suspended
  procedure Resume (Timer : in out Passive_Timer);

  -- Checks if timer expiration time (Prev_Exp + Period) is reached
  -- If yes, and periodical, add Period to expiration time
  -- If yes and single shot timer, set it to raise Timer_Expired
  function Has_Expired (Timer : in out Passive_Timer) return Boolean;

  -- When a timer has expired once and has no period (0.0) it is not re-armed
  -- Calling Suspend, Resume or Has_Expired again on it will raise:
  Timer_Expired : exception;

private
  type Passive_Timer is tagged limited record
    Running : Boolean := False;
    Period : Timers.Period_Range;
    Next_Expiration : Chronos.Time_Rec;
    Chrono : Chronos.Chrono_Type;
    Expired : Boolean;
  end record;

end Chronos.Passive_Timers;

