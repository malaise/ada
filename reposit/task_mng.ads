with Timers;
generic

  -- The user-defined procedure that will be called periodically
  with procedure Call_Back;

package Task_Mng is

  -- Any Period lower than Min_Period becomes Min_Period
  Min_Period : constant Duration := 0.1;

  -- At elaboration, the activity is not started
  -- This call starts effectively the activity with a period (first
  --  activation will occur after the period)
  -- If the activity if already started, then its period is updated (next
  --  activation will occure at now + Period)
  procedure Start (Period : in Duration);

  -- If the the activity is started, then stops it
  -- No effect if the activity is already stopped
  procedure Stop;

private

  -- The Timers callback, defined in private part because of generic
  function Callback (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean;

  Cb_Access : constant Timers.Timer_Callback := Callback'Access;
end Task_Mng;

