with Ada.Unchecked_Deallocation;
package body Passive_Timers is

  -- type Passive_Timer is tagged private;


  -- Arm a passive timer with a given period
  -- Overwrites any previous setting on this timer
  -- Raises Invalid_Period if Period is <= 0.0
  procedure Arm (Timer : in out Passive_Timer;
                 Period : in Duration) is
    use type Ada.Calendar.Time;
  begin
    if Period <= 0.0 then
      raise Invalid_Period;
    end if;
    Timer.Acc.Next_Expiration := Ada.Calendar.Clock + Period;
    Timer.Acc.Period := Period;
  end Arm;


  -- Checks if timer expiration time (Prev_Exp + Period) is reached
  -- If yes, add Period to expiration time
  function Has_Expired (Timer : in Passive_Timer) return Boolean is
    use type Ada.Calendar.Time;
  begin
    if Timer.Acc.Period = Not_Started
    or else Ada.Calendar.Clock < Timer.Acc.Next_Expiration then
      return False;
    else
      Timer.Acc.Next_Expiration :=
          Timer.Acc.Next_Expiration + Timer.Acc.Period;
      return True;
    end if;
  end Has_Expired;

  procedure Free is new Ada.Unchecked_Deallocation (Timer_Rec, Timer_Access);
  procedure Finalize (Timer : in out Passive_Timer) is
  begin
    Free (Timer.Acc);
  end Finalize;

end Passive_Timers;

