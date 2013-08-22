with Virtual_Time, Perpet;
package body Chronos.Passive_Timers is

  -- Timer status, independant from the associated clock status
  function Status (Timer : Passive_Timer) return Timer_Status is
  begin
    return (
      if not Timer.Running then Timers.Deleted
      elsif Timer.Chrono.Get_Status = Chronos.Stopped then Timers.Suspended
      else Timers.Running);
  end Status;

  -- True if timer is not Deleted
  function Running (Timer : Passive_Timer) return Boolean is
  begin
    return Timer.Running;
  end Running;

  -- Arm a passive timer with a given period
  -- Overwrites any previous setting on this timer
  -- Raises Invalid_Period if Period is <= 0.0
  procedure Start (Timer      : in out Passive_Timer;
                   Delay_Spec : in Timers.Delay_Rec) is
    Start_Time : Virtual_Time.Time;
    use type Timers.Delay_List, Virtual_Time.Time, Perpet.Delta_Rec;
  begin
    if Delay_Spec.Delay_Kind = Timers.Delay_Sec
    and then Delay_Spec.Delay_Seconds < 0.0 then
      raise Invalid_Delay;
    end if;
    -- (Re) initialize chrono
    Timer.Chrono.Stop;
    Timer.Chrono.Attach (Delay_Spec.Clock);
    Timer.Chrono.Start (Start_Time);
    -- Initialise timer
    Timer.Next_Expiration := (case Delay_Spec.Delay_Kind is
        when Timers.Delay_Sec => Perpet.To_Delta_Rec (Delay_Spec.Delay_Seconds),
        when Timers.Delay_Del => Delay_Spec.Delay_Delta,
        when Timers.Delay_Exp =>
          (if Delay_Spec.Expiration_Time > Start_Time then
             Delay_Spec.Expiration_Time - Start_Time
           else
             Timers.Default_Delta));
    Timer.Period := Delay_Spec.Period;
    Timer.Expired := False;
    Timer.Running := True;
  end Start;

  -- Stop a timer, which becomes unusable until re-armed
  -- Timer_Stopped : exception;
  procedure Stop (Timer : in out Passive_Timer) is
  begin
    if not Timer.Running then
      raise Timer_Stopped;
    end if;
    -- Stop chrono and detach it from its clock
    Timer.Chrono.Stop;
    Timer.Chrono.Attach (null);
    Timer.Running := False;
  end Stop;

  -- Suspend a timer: pending expirations can still be retrieved
  -- No action is timer is alread syspended
  procedure Suspend (Timer : in out Passive_Timer) is
  begin
    if not Timer.Running then
      raise Timer_Stopped;
    end if;
    if Timer.Expired then
      raise Timer_Expired;
    end if;
    Timer.Chrono.Stop;
  end Suspend;

  -- Resume a suspended a timer: new expirations are resumed
  -- No action is timer is not syspended
  procedure Resume (Timer : in out Passive_Timer) is
  begin
    if not Timer.Running then
      raise Timer_Stopped;
    end if;
    if Timer.Expired then
      raise Timer_Expired;
    end if;
    Timer.Chrono.Start;
  end Resume;

  -- Checks if timer expiration time (Prev_Exp + Period) is reached
  -- If yes, add Period to expiration time
  function Has_Expired (Timer : in out Passive_Timer) return Boolean is
    use type Chronos.Time_Rec;
  begin
    if not Timer.Running then
      raise Timer_Stopped;
    end if;
    if Timer.Expired then
      raise Timer_Expired;
    end if;
    if Timer.Chrono.Read < Timer.Next_Expiration then
      return False;
    else
      if Timer.Period /= 0.0 then
        Timer.Next_Expiration :=
            Timer.Next_Expiration + Timer.Period;
      else
        Timer.Expired := True;
      end if;
      return True;
    end if;
  end Has_Expired;

end Chronos.Passive_Timers;

