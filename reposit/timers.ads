with Ada.Calendar;
package Timers is

  -- How to specify a timer, wait some seconds or until a specific time
  type Delay_List is (Delay_Sec, Delay_Exp);
  Infinite_Seconds : constant Duration := -1.0;

  -- How to specify a period for a timer
  subtype Period_Range is Duration range 0.0 .. Duration'Last;
  No_Period : Period_Range := 0.0;

  type Delay_Rec (Delay_Kind : Delay_List := Delay_Sec) is record
    Period : Period_Range := No_Period;
    case Delay_Kind is
      when Delay_Sec =>
        Delay_Seconds : Duration := Infinite_Seconds;
      when Delay_Exp =>
        Expiration_Time : Ada.Calendar.Time;
    end case;
  end record;
  -- Infinite delay
  Infinite_Delay : constant Delay_Rec(Delay_Sec)
                 := (Delay_Kind => Delay_Sec,
                     Period        => No_Period,
                     Delay_Seconds => Infinite_seconds);

  -- Timer unique identifier
  type Timer_Id is private;

  -- Timer callback: called when the timer expires with one argument:
  --  the timer Id
  -- Should return True if the timer expiration has to be reported by
  --    expire
  type Timer_Callback is access function (Id : in Timer_Id) return Boolean;

  -- Value returned by Next_Timer if no more timer
  No_Timer : constant Timer_Id;

  -- Create a new timer
  -- May raise Invalid_Delay if Delay_Seconds is < 0
  Invalid_Delay : exception;
  -- May raise No_More_Timer if resource exhausted
  No_More_Timer : exception;
  function Create (Delay_Spec : Delay_Rec;
                   Callback   : Timer_Callback) return Timer_Id;

  -- Delete a timer
  -- May raise Invalid_Timer if timer has no period and has expired
  Invalid_Timer : exception;
  procedure Delete (Id : in Timer_Id);


  -- For each timer for which if expiration time/delay is reached
  -- its callback is called
  -- then, if periodical it is re-armed (and may expire)
  --       if not it is deleted
  -- Return True if at least one timer has expired with a callback set
  --  and this callback has returned True
  function Expire return Boolean;

  -- Delay until next timer expires (or Infinite_Seconds)
  function Wait_For return Duration;
private

  type Timer_Id is record
    Timer_Num : Natural := 0;
  end record;
  No_Timer : constant Timer_Id := (Timer_Num => 0);

end Timers;

