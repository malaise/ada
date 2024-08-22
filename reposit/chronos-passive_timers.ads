-- A passive timer is a kind of chorono that allows to know if a
--  timeout as expired (instead of reading time)
with Timers;
package Chronos.Passive_Timers is

  -- A passive timer is a kind of chorono that allows to know if a
  --  timeout as expired (instead of reading time)
  -- Timeout can be periodic or single shot (no period), see Timers
  type Passive_Timer is tagged limited private;

  -- Expiration appointments  of a timer
  subtype Delay_List is Timers.Delay_List;
  Delay_Sec : constant Delay_List := Timers.Delay_Sec;
  Delay_Del : constant Delay_List := Timers.Delay_Del;
  Delay_Exp : constant Delay_List := Timers.Delay_Exp;
  subtype Period_Range is Timers.Period_Range;
  No_Period : constant Period_Range := Timers.No_Period;
  Default_Timeout : constant Duration := Timers.Default_Timeout;
  Default_Delta : constant Timers.Delta_Rec := Timers.Default_Delta;
  subtype Delay_Rec is Timers.Delay_Rec;
  Infinite_Seconds : constant Duration := Timers. Infinite_Seconds;
  Infinite_Delay : constant Delay_Rec := Timers.Infinite_Delay;

  -- Timer status, independant from the associated clock status
  subtype Timer_Status is Timers.Timer_Status;
  Stopped : constant Timer_Status := Timers.Timer_Status'(Timers.Deleted);

  function Status (Timer : Passive_Timer) return Timer_Status;
  -- True if timer is not Deleted (not Stopped)
  function Running (Timer : Passive_Timer) return Boolean;

  -- Arm a passive timer, possibly with a given period
  -- Timers with a period 0.0 are single shot
  -- Overwrites any previous setting on this timer
  -- May raise Invalid_Delay if Delay_Seconds is < 0
  Invalid_Delay : exception;
  procedure Start (Timer      : in out Passive_Timer;
                   Delay_Spec : in Delay_Rec);

  -- Stop a timer, which becomes unusable (deleted) until re-started
  Timer_Stopped : exception;
  procedure Stop (Timer : in out Passive_Timer);

  -- Suspend a timer: pending expirations can still be retrieved
  -- No action if timer is alread syspended
  procedure Suspend (Timer : in out Passive_Timer);

  -- Resume a suspended a timer: new expirations are resumed
  -- No action if timer is not suspended
  procedure Resume (Timer : in out Passive_Timer);

  -- Check if timer expiration time (Prev_Exp + Period) is reached
  -- If yes, and periodical, add Period to expiration time
  -- If yes and single shot, set it to raise Timer_Expired
  function Has_Expired (Timer : in out Passive_Timer) return Boolean;

  -- When a single shot timer has expired and Has_Expired has returned True
  --  then, as long as it is not re-started, calling Suspend, Resume
  --  or Has_Expired again on it will raise:
  Timer_Expired : exception;

private
  type Passive_Timer is tagged limited record
    -- State and characteristics of the timer
    Running : Boolean := False;
    Period : Period_Range;
    Next_Expiration : Chronos.Time_Rec;
    -- Underlying chrono
    Chrono : Chronos.Chrono_Type;
    -- Single shot timer has expired and Has_Expired has already reported True
    Expired : Boolean;
  end record;

end Chronos.Passive_Timers;

