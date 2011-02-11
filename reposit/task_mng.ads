with Timers;
generic

  Activation_Period : Duration := 1.0;

  -- The user defines this procedure which will be called regulary.
  -- The call back is called on timer expiration.
  with procedure Call_Back;

package Task_Mng is

  -- Default period at activation (instanciation)
  Def_Period : constant Duration := 1.0;
  -- Any Period lower than Min_Period becomes Min_Period
  Min_Period : constant Duration := 0.1;

  -- At elaboration, the activity is not started
  -- This call starts effectively the activity, eventually with a new period.
  -- If the activity if already started, its period is updated (next activation
  --  will occure at now + New_Period).
  procedure Start (New_Period : in Duration := Activation_Period);

  -- When the the task is started, stops it.
  -- If the task is already stopped, no effect.
  -- If the task has been aborted, exception is raised.
  procedure Stop;

private

  function Callback (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data;
                     New_Id : Timers.Timer_Id) return Boolean;

  Cb_Access : constant Timers.Timer_Callback := Callback'Access;
end Task_Mng;

