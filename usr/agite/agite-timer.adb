with Timers;
separate (Agite)
package body Timer is

  -- We don't make a real periodic because if Agite makes a long GIT
  -- operation we don't want a burst of executions
 Period : Timers.Period_Range;
 Tid : Timers.Timer_Id := Timers.No_Timer;
 function Timer_Cb (Id   : in Timers.Timer_Id;
                    Data : in Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Id, Data);
    Spec : Timers.Delay_Rec;
  begin
    Spec.Delay_Seconds := Period;
    Tid := Timers.Create (Spec, Timer_Cb'Unrestricted_Access);
    -- Signal to Agite the expiration
    return True;
  end Timer_Cb;

  procedure Start (Periodic : in Boolean := False) is
    Spec : Timers.Delay_Rec;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    if Periodic then
      Period := Config.Period;
      Spec.Delay_Seconds := Period;
    else
      Stop;
      Spec.Delay_Seconds := 1.0;
    end if;
    Tid := Timers.Create (Spec, Timer_Cb'Unrestricted_Access);
  end Start;

  procedure Stop is
    use type Timers.Timer_Id;
  begin
    if Tid /= Timers.No_Timer then
      Timers.Delete (Tid);
      Tid := Timers.No_Timer;
    end if;
  end Stop;

end Timer;

