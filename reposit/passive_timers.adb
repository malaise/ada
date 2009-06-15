with Ada.Unchecked_Deallocation;
with Perpet;
package body Passive_Timers is

  procedure Free is new Ada.Unchecked_Deallocation (Timer_Rec, Timer_Access);

  -- Arm a passive timer with a given period
  -- Overwrites any previous setting on this timer
  -- Raises Invalid_Period if Period is <= 0.0
  procedure Arm (Timer  : in out Passive_Timer;
                 Period : in Duration;
                 Clock  : in Virtual_Time.Clock_Access := null) is
    use type Virtual_Time.Time, Perpet.Delta_Rec;
  begin
    if Period <= 0.0 then
      raise Invalid_Period;
    end if;
    if Timer.Acc = null then
      Timer.Acc := new Timer_Rec;
    end if;
    -- (Re) initialize chrono
    Timer.Acc.Chrono.Stop;
    Timer.Acc.Chrono.Attach (Clock);
    Timer.Acc.Chrono.Start;
    -- Initialise timer
    Timer.Acc.Next_Expiration := Perpet.To_Delta_Rec (Period);
    Timer.Acc.Period := Period;
  end Arm;

  -- Stop a timer, which becomes unusable until re-armed
  -- Timer_Stopped : exception;
  procedure Stop (Timer : in out Passive_Timer) is
  begin
    if Timer.Acc = null then
      raise Timer_Stopped;
    end if;
    -- Deallocate (chrono un-registers...)
    Free (Timer.Acc);
  end Stop;

  -- Suspend a timer: pending expirations can still be retrieved
  -- No action is timer is alread syspended
  procedure Suspend (Timer : in out Passive_Timer) is
  begin
    if Timer.Acc = null then
      raise Timer_Stopped;
    end if;
    Timer.Acc.Chrono.Stop;
  end Suspend;

  -- Resume a suspended a timer: new expirations are resumed
  -- No action is timer is not syspended
  procedure Resume (Timer : in out Passive_Timer) is
  begin
    if Timer.Acc = null then
      raise Timer_Stopped;
    end if;
    Timer.Acc.Chrono.Stop;
  end Resume;

  -- Checks if timer expiration time (Prev_Exp + Period) is reached
  -- If yes, add Period to expiration time
  function Has_Expired (Timer : in Passive_Timer) return Boolean is
    use type Chronos.Time_Rec;
  begin
    if Timer.Acc = null then
      raise Timer_Stopped;
    end if;
    if Timer.Acc.Chrono.Read < Timer.Acc.Next_Expiration then
      return False;
    else
      Timer.Acc.Next_Expiration :=
          Timer.Acc.Next_Expiration + Timer.Acc.Period;
      return True;
    end if;
  end Has_Expired;

  procedure Finalize (Timer : in out Passive_Timer) is
  begin
    Free (Timer.Acc);
  end Finalize;

end Passive_Timers;

