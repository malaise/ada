package body Task_Mng is

  -- The timer id
  Tid : Timers.Timer_Id;

  -- The timer callback
  function Callback (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Id, Data);
  begin
    -- Call user callback
    Call_Back;
    return False;
  end Callback;

  -- Start/restart/replace
  procedure Start (Period : in Duration) is
    Expiration : Timers.Delay_Rec (Timers.Delay_Sec);
    New_Tid : Timers.Timer_Id;
  begin
    -- Set new period
    Expiration.Period := (if Period < Min_Period then Min_Period
                          elsif Period > Timers.Period_Range'Last then
                            Timers.Period_Range'Last
                          else Period);
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

