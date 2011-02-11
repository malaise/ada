with Perpet, Virtual_Time;
package Timers is

  -- How to specify a timer, wait some seconds or until a specific time
  type Delay_List is (Delay_Sec, Delay_Del, Delay_Exp);

  -- May be returned by Wait_For
  -- Do not use for timers
  Infinite_Seconds : constant Duration := -1.0;

  -- How to specify a period for a timer
  subtype Period_Range is Duration range 0.0 .. Duration'Last;
  No_Period : Period_Range := 0.0;
  Default_Timeout : constant Duration := 0.0;
  Default_Delta : constant Perpet.Delta_Rec := (0, 0.0);

  type Delay_Rec (Delay_Kind : Delay_List := Delay_Sec) is record
    Clock  : Virtual_Time.Clock_Access := null;
    Period : Period_Range := No_Period;
    case Delay_Kind is
      when Delay_Sec =>
        Delay_Seconds : Duration := Default_Timeout;
      when Delay_Del =>
        Delay_Delta : Perpet.Delta_Rec := Default_Delta;
      when Delay_Exp =>
        Expiration_Time : Virtual_Time.Time;
    end case;
  end record;

  -- Infinite delay. Do not use for timers
  Infinite_Delay : constant Delay_Rec(Delay_Sec)
                 := (Delay_Kind    => Delay_Sec,
                     Clock         => null,
                     Period        => No_Period,
                     Delay_Seconds => Infinite_Seconds);

  -- Timer unique identifier
  type Timer_Id is tagged private;
  function Image (Id : Timer_Id) return String;

  -- Value returned by Next_Timer if no more timer
  No_Timer : constant Timer_Id;

  -- Is a Timer_Id set
  -- Beware that a Timer_Id may still set after expiration, even if no period
  function Is_Set (Id : Timer_Id) return Boolean;
  -- Reset a Timer_Id (set it to No_Timer)
  procedure Reset (Id : in out Timer_Id);

  -- Timer callback: called when the timer expires with three arguments:
  --  the timer Id if the timer created
  --  the Data provided at timer creation
  --  the New_Id to set: same as Id if the timer is periodic and No_Timer
  --    otherwise, so that the user callback may start with
  --    My_Global_Id := New_Id;
  -- Should return True if the timer expiration has to be reported by
  --    expire
  subtype Timer_Data is Natural;
  No_Data : constant Timer_Data := 0;

  type Timer_Callback is access
        function (Id : in Timer_Id;
                  Data : in Timer_Data;
                  New_Id : in Timer_Id) return Boolean;

  -- Create a new timer
  -- May raise Invalid_Delay if Delay_Seconds is < 0
  Invalid_Delay : exception;
  -- May raise No_More_Timer if resource exhausted
  No_More_Timer : exception;
  function Create (Delay_Spec : Delay_Rec;
                   Callback   : Timer_Callback;
                   Data       : Timer_Data := No_Data) return Timer_Id;
  procedure Create (Id         : in out Timer_Id;
                    Delay_Spec : in Delay_Rec;
                    Callback   : in Timer_Callback;
                    Data       : in Timer_Data := No_Data);

  -- Does a timer exist (still running, even if suspended)
  function Exists (Id : Timer_Id) return Boolean;

  -- Delete a timer
  -- May raise Invalid_Timer if timer has no period and has expired
  Invalid_Timer : exception;
  procedure Delete (Id : in out Timer_Id);

  -- Delete a timer if it exists
  -- No exception even if Timer_Id is not set
  procedure Delete_If_Exists (Id : in out Timer_Id);

  -- Suspend a timer: expirations, even the pending ones are suspended
  -- No action if timer is alread syspended
  -- May raise Invalid_Timer if timer has no period and has expired
  procedure Suspend (Id : in Timer_Id);

  -- Resume a suspended a timer: expirations, even the pending ones are resumed
  -- No action if timer is not syspended
  -- May raise Invalid_Timer if timer has no period and has expired
  procedure Resume (Id : in Timer_Id);


  -- For each timer for which if expiration time/delay is reached
  -- its callback is called
  -- then, if periodical it is re-armed (and may expire)
  --       if not it is deleted
  -- Return True if at least one timer has expired with a callback set
  --  and this callback has returned True or if at least one timer has
  --  expired with no callback set
  function Expire return Boolean;


  -- Expiration time
  type Expiration_Rec (Infinite : Boolean := True) is record
    case Infinite is
      when True => null;
      when False => Time : Virtual_Time.Time;
    end case;
  end record;
  Infinite_Expiration : constant Expiration_Rec := (Infinite => True);

  function "<" (E1, E2 : Expiration_Rec) return Boolean;

  -- Delay until next timer expires (or Infinite_Seconds)
  function Wait_For return Duration;
  -- Expiration time of next timer (or Infinite)
  function Wait_Until return Expiration_Rec;

  -- Compute nearest expiration time from Expiration and timers
  function Next_Expiration (Expiration : Expiration_Rec) return Expiration_Rec;

  -- Is expiration reached
  function Is_Reached (Expiration : Expiration_Rec) return Boolean;

  -- Interface for the virtual clock, don'use
  type Observer_Type is new Virtual_Time.Observer with null record;
  procedure Notify (An_Observer : in out Observer_Type;
                    Rtime, Vtime : in Virtual_Time.Time;
                    Speed : in Virtual_Time.Speed_Range;
                    Clock : in Virtual_Time.Clock_Access);

private

  type Timer_Id is tagged record
    Timer_Num : Natural := 0;
  end record;
  No_Timer : constant Timer_Id := (Timer_Num => 0);

end Timers;

