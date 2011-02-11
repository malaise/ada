package body Task_Mng is

  -- The timer id
  Tid : Timers.Timer_Id;

  -- The timer callback
  function Callback (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data;
                     New_Id : Timers.Timer_Id) return Boolean is
    pragma Unreferenced (Id, Data);
  begin
    Tid := New_Id;
    -- Call user callback
    Call_Back;
    return False;
  end Callback;

  -- Start/restart/replace
  procedure Start (New_Period : in Duration := Activation_Period) is
    Expiration : Timers.Delay_Rec (Timers.Delay_Sec);
    New_Tid : Timers.Timer_Id;
  begin
    -- Set new period
    if New_Period < Min_Period then
      Expiration.Period := Min_Period;
    elsif New_Period > Timers.Period_Range'Last then
      Expiration.Period := Timers.Period_Range'Last;
    else
      Expiration.Period := New_Period;
    end if;
    Expiration.Delay_Seconds := Expiration.Period;
    -- Arm new timer ASAP
    New_Tid.Create (Expiration, Cb_Access);
    -- Cancel previous timer
    Stop;
    Tid := New_Tid;
  end Start;

  -- When the the task is started, stops it.
  -- If the task is already stopped, no effect.
  -- If the task has been aborted, exception is raised.
  procedure Stop is
  begin
    -- Cancel previous timer
    Tid.Delete_If_Exists;
  end Stop;

end Task_Mng;

