package Timers.Expiration is

  --------------------------------------------------------------------
  -- The operations of this package are used by Event_Mng and X_Mng --
  -- They should not be used by "normal" applications               --
  --------------------------------------------------------------------

  -- For each timer for which the expiration time/delay is reached
  --  its callback is called
  --  then, if periodical it is re-armed (and may expire)
  --        if not it is deleted
  -- Return True if at least one timer has expired with a callback set
  --  and this callback has returned True or if at least one timer has
  --  expired with no callback set
  function Expire return Boolean;

  -- Expiration time (Time is real time)
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

end Timers.Expiration;

