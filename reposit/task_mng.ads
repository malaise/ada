with Timers;
generic

  -- The default value for Period of Start
  Default_Period : Duration := 1.0;

  -- The user defines this procedure which will be called periodically.
  -- The callback is called on timer expiration.
  with procedure Call_Back;

package Task_Mng is

  -- Any Period lower than Min_Period becomes Min_Period
  Min_Period : constant Duration := 0.1;

  -- At elaboration, the activity is not started
  -- This call starts effectively the activity with a period.
  -- If the activity if already started, its period is updated (next activation
  --  will occure at now + Period).
  procedure Start (Period : in Duration := Default_Period);

  -- When the the activity is started, stops it.
  -- No effect if the activity is already stopped.
  procedure Stop;

private

  function Callback (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean;

  Cb_Access : constant Timers.Timer_Callback := Callback'Access;
end Task_Mng;

