generic

  Activation_Period : Duration := 1.0;

  -- The user defines this procedure which will be called regulary.
  -- The main program must call "delay" or schedule regulary
  --  (no Get except the ones with timeout of Con_Io).
  -- The call back is called only when the schedule procedure is called
  --  (the first time after activation date is reached)
  -- The activation will be aborted if the procedure raises an exception.
  -- If the period is too small, the task may "keep the hand" definitively,
  --  so the duration has a minimum : Min_Period. Any period value
  --  (for Activation_Period or New_Period of Start) less then Min_Period
  --  becomes Min_Period.

  with procedure Call_Back;

package Task_Mng is

  -- Default period at activation (instanciation)
  Def_Period : constant Duration := 1.0;
  Min_Period : constant Duration := 0.5;

  -- At elaboration, the task is ready but not started.
  -- This call starts effectively the task, eventually with a new period.
  -- If the task if already started, its period is updated.
  -- If the task has been aborted, exception is raised.
  -- If the period is <= 0, the task is started with previous period
  procedure Start (New_Period : in Duration := Activation_Period);

  Task_Aborted : exception;

  -- When the the task is started, stops it.
  -- If the task is already stopped, no effect.
  -- If the task has been aborted, exception is raised.
  procedure Stop;

  -- Aborts the task, mandatory for the main program to exit.
  -- If the task is already aborted, exception is raised.
  procedure Abort_Task;

  -- Returns the current period of activation.
  -- If the task is already aborted, exception is raised.
  function Get_Period return Duration;

  -- To activate (if period is reached) the call back
  procedure Schedule;

end Task_Mng;

