with Any_Def, Perpet, Virtual_Time, Smart_Reference;
package Timers is

  -- How to specify a timer, wait some seconds or until a specific time
  type Delay_List is (Delay_Sec, Delay_Del, Delay_Exp);

  -- May be returned by Wait_For
  -- Do not use for timers
  Infinite_Seconds : constant Duration := -1.0;

  -- How to specify a period for a timer
  subtype Period_Range is Duration range 0.0 .. Duration'Last;
  -- Timer with No_Period will expire only once and is called single-shot
  --  otherwise it is periodic
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
  No_Timer : constant Timer_Id;

  -- Timer status, independant from the associated clock status
  type Timer_Status is (Deleted,   -- Not created or single-shot expired
                        Running,   -- Will expire
                        Suspended);-- Will expire but currently suspended
  function Status (Id : in Timer_Id) return Timer_Status;
  -- True if timer is not Deleted
  function Exists (Id : in Timer_Id) return Boolean;

  -- Timer callback: called when the timer expires with two arguments:
  --  the timer Id if the timer created
  --  the Data provided at timer creation
  -- Should return True if the timer expiration has to be reported by Expire
  --  (and Event_Mng will report Timer_Event)
  subtype Timer_Data is Any_Def.Any;
  No_Data : constant Timer_Data := (Kind => Any_Def.None_Kind);

  type Timer_Callback is access
        function (Id : in Timer_Id;
                  Data : in Timer_Data) return Boolean;

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

  -- Delete a timer
  -- May raise Invalid_Timer if timer is Deleted
  Invalid_Timer : exception;
  procedure Delete (Id : in out Timer_Id);

  -- Delete a timer if it exists
  -- No exception even if Timer_Id is Deleted
  procedure Delete_If_Exists (Id : in out Timer_Id);

  -- Suspend a timer: expirations, even the pending ones are suspended
  -- No action if timer is alread syspended
  -- May raise Invalid_Timer if timer is Deleted
  procedure Suspend (Id : in Timer_Id);

  -- Resume a suspended a timer: expirations, even the pending ones are resumed
  -- No action if timer is not suspended
  -- May raise Invalid_Timer if timer is Deleted
  procedure Resume (Id : in Timer_Id);

  -- Return the delay until expiration
  -- May raise Invalid_Timer if timer is Deleted
  function Remaining (Id : Timer_Id) return Perpet.Delta_Rec;

  --------------------------------------------------------------
  -- The following operations are used by Event_Mng and X_Mng --
  -- They should not be used by "normal" applications         --
  --------------------------------------------------------------

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

  -- Timer descriptor
  subtype Exp_Rec is Delay_Rec(Delay_Exp);
  type Timer_Rec is record
    Status : Timer_Status;
    Exp : Exp_Rec;
    Cre : Virtual_Time.Time;
    Dat : Timer_Data;
    Cb  : Timer_Callback;
    Clock : Virtual_Time.Clock_Access;
    -- Clock speed 0
    Frozen : Boolean;
    Remaining : Perpet.Delta_Rec;
  end record;

  procedure Set (Dest : in out Timer_Rec; Val : in Timer_Rec);
  package Smart_Timer_Mng is new Smart_Reference (
    Object => Timer_Rec, Set => Set);

  type Timer_Id is new Smart_Timer_Mng.Handle with null record;

  Def_Timer : Timer_Id;
  No_Timer : constant Timer_Id := Def_Timer;

end Timers;

