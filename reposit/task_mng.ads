generic

  ACTIVATION_PERIOD : DURATION := 1.0;

  -- The user defines this procedure which will be called regulary.
  -- The main program must call "delay" or schedule regulary
  --  (no GET except the ones with timeout of CON_IO).
  -- The call back is called only when the schedule procedure is called
  --  (the first time after activation date is reached)
  -- The activation will be aborted if the procedure raises an exception.
  -- If the period is too small, the task may "keep the hand" definitively,
  --  so the duration has a minimum : MIN_PERIOD. Any period value
  --  (for ACTIVATION_PERIOD or NEW_PERIOD of START) less then MIN_PERIOD
  --  becomes MIN_PERIOD.

  with procedure CALL_BACK;

package TASK_MNG is

  -- default period at activation (instanciation)
  DEF_PERIOD : constant DURATION := 1.0;
  MIN_PERIOD : constant DURATION := 0.5;

  -- At elaboration, the task is ready but not started.
  -- This call starts effectively the task, eventually with a new period.
  -- If the task if already started, its period is updated.
  -- If the task has been aborted, exception is raised.
  -- If the period is <= 0, the task is started with previous period
  procedure START (NEW_PERIOD : in DURATION := ACTIVATION_PERIOD);

  TASK_ABORTED : exception;

  -- When the the task is started, stops it.
  -- If the task is already stopped, no effect.
  -- If the task has been aborted, exception is raised.
  procedure STOP;

  -- Aborts the task, mandatory for the main program to exit.
  -- If the task is already aborted, exception is raised.
  procedure ABORT_TASK;

  -- Returns the current period of activation.
  -- If the task is already aborted, exception is raised.
  function GET_PERIOD return DURATION;

  -- To activate (if period is reached) the call back
  procedure SCHEDULE;

end TASK_MNG;
