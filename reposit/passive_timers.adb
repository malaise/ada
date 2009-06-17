with Ada.Unchecked_Deallocation;
with Virtual_Time, Perpet;
package body Passive_Timers is

  procedure Free is new Ada.Unchecked_Deallocation (Timer_Rec, Timer_Access);

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
    if Timer.Acc = null then
      Timer.Acc := new Timer_Rec;
    end if;
    -- (Re) initialize chrono
    Timer.Acc.Chrono.Stop;
    Timer.Acc.Chrono.Attach (Delay_Spec.Clock);
    Timer.Acc.Chrono.Start (Start_Time);
    -- Initialise timer
    case Delay_Spec.Delay_Kind is
      when Timers.Delay_Sec =>
        Timer.Acc.Next_Expiration :=
            Perpet.To_Delta_Rec (Delay_Spec.Delay_Seconds);
      when Timers.Delay_Del =>
        Timer.Acc.Next_Expiration := Delay_Spec.Delay_Delta;
      when Timers.Delay_Exp =>
        if Delay_Spec.Expiration_Time > Start_Time then
          Timer.Acc.Next_Expiration := Delay_Spec.Expiration_Time - Start_Time;
        else
          Timer.Acc.Next_Expiration := Timers.Default_Delta;
        end if;
    end case;
    Timer.Acc.Period := Delay_Spec.Period;
    Timer.Acc.Expired := False;
  end Start;

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
    if Timer.Acc.Expired then
      raise Timer_Expired;
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
    if Timer.Acc.Expired then
      raise Timer_Expired;
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
    if Timer.Acc.Expired then
      raise Timer_Expired;
    end if;
    if Timer.Acc.Chrono.Read < Timer.Acc.Next_Expiration then
      return False;
    else
      if Timer.Acc.Period /= 0.0 then
        Timer.Acc.Next_Expiration :=
            Timer.Acc.Next_Expiration + Timer.Acc.Period;
      else
        Timer.Acc.Expired := True;
      end if;
      return True;
    end if;
  end Has_Expired;

  procedure Finalize (Timer : in out Passive_Timer) is
  begin
    Free (Timer.Acc);
  end Finalize;

end Passive_Timers;

