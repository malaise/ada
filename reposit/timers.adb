with Dynamic_List, Event_Mng;
package body Timers is

  -- Return an image of a timer
  function Image (Id : Timer_Id) return String is
  begin
    return Id.Timer_Num'Img;
  end Image;

  -- Allocated timer Ids
  subtype Timer_Id_Range is Positive;
  Next_Timer_Id : Timer_Id_Range := Timer_Id_Range'First;
  -- Allocate a new (free) timer id
  function Get_Next_Id return Timer_Id_Range;

  -- List of running timers
  subtype Exp_Rec is Delay_Rec(Delay_Exp);
  type Timer_Rec is record
    Id  : Timer_Id_Range;
    Exp : Exp_Rec;
    Cre : Ada.Calendar.Time;
    Dat : Timer_Data;
    Cb  : Timer_Callback;
  end record;

  package Timer_List_Mng is new Dynamic_List (Timer_Rec);
  Timer_List : Timer_List_Mng.List_Type;

  -- Sort timers in crescent order of expiration times
  --  if same expiration, use creation time
  function "<" (T1, T2 : Timer_Rec) return Boolean is
    use type Ada.Calendar.Time;
  begin
    return T1.Exp.Expiration_Time < T2.Exp.Expiration_Time
    or else (T1.Exp.Expiration_Time = T2.Exp.Expiration_Time
      and then T1.Cre < T2.Cre);
  end "<";
  procedure Sort is new Timer_List_Mng.Sort ("<");

  -- Search Timer by Id
  function Id_Match (T1, T2 : Timer_Rec) return Boolean is
  begin
    return T1.Id = T2.Id;
  end Id_Match;
  procedure Search_Id is new Timer_List_Mng.Safe_Search (Id_Match);

  procedure Incr_Id is
  begin
    if Next_Timer_Id /= Timer_Id_Range'Last then
      Next_Timer_Id := Next_Timer_Id + 1;
    else
      Next_Timer_Id := Timer_Id_Range'First;
    end if;
  end Incr_Id;

  -- Allocate a new (free) timer id
  function Get_Next_Id return Timer_Id_Range is
    Start_Id : constant Timer_Id_Range := Next_Timer_Id;
    Timer : Timer_Rec;
    Found : Boolean;
  begin
    loop
      -- Init to next possible Id
      Timer.Id := Next_Timer_Id;
      -- Check not allocated
      Search_Id (Timer_List, Found, Timer, From => Timer_List_Mng.Absolute);
      if not Found then
          -- Good
          Incr_Id;
          return Timer.Id;
      end if;
      -- Bad luck, this Id is in use. Try next.
      Incr_Id;
      -- Check we have not tried ALL timers
      if Next_Timer_Id = Start_Id then
        raise No_More_Timer;
      end if;
    end loop;
  end Get_Next_Id;

  -- Create a new timer
  -- May raise Invalid_Delay if Delay_Seconds is < 0
  -- Invalid_Delay : exception;
  function Create (Delay_Spec : Delay_Rec;
                   Callback   : Timer_Callback;
                   Data       : Timer_Data := No_Data) return Timer_Id is

    Timer : Timer_Rec;
    This_Id : Timer_Id_Range;
    use Ada.Calendar;
  begin
    -- Compute expiration time ASAP
    Timer.Cre := Ada.Calendar.Clock;
    if Delay_Spec.Delay_Kind = Delay_Sec then
      if Delay_Spec.Delay_Seconds < 0.0 then
        raise Invalid_Delay;
      end if;
      Timer.Exp.Expiration_Time := Timer.Cre
                                 + Delay_Spec.Delay_Seconds;
    else
      Timer.Exp.Expiration_Time := Delay_Spec.Expiration_Time;
    end if;

    -- Allocate Id and copy period and callback
    Timer.Id := Get_Next_Id;
    Timer.Exp.Period := Delay_Spec.Period;
    Timer.Dat := Data;
    Timer.Cb := Callback;

    -- Insert in beginning of list and sort it
    if not Timer_List_Mng.Is_Empty (Timer_List) then
      Timer_List_Mng.Rewind (Timer_List);
    end if;
    Timer_List_Mng.Insert (Timer_List, Timer, Timer_List_Mng.Prev);
    Sort (Timer_List);

    -- If this timer is first then force wake-up of select
    This_Id := Timer.Id;
    Timer_List_Mng.Read (Timer_List, Timer, Timer_List_Mng.Current);
    if Timer.Id = This_Id then
      Event_Mng.Wake_Up;
    end if;

    -- Done
    return (Timer_Num => This_Id);
  end Create;

  -- Locate a timer in list
  -- May raise Invalid_Timer if timer has expired
  procedure Locate (Id : in Timer_Id) is
    Timer : Timer_Rec;
    Found : Boolean;
  begin
    -- Check validity
    if Id = No_Timer then
      raise Invalid_Timer;
    end if;
    -- Try current
    if not Timer_List_Mng.Is_Empty (Timer_List) then
      Timer_List_Mng.Read (Timer_List, Timer, Timer_List_Mng.Current);
      if Timer.Id = Id.Timer_Num then
        return ;
      end if;
    else
      raise Invalid_Timer;
    end if;

    -- Search timer
    Timer.Id := Id.Timer_Num;
    Search_Id (Timer_List, Found, Timer, From => Timer_List_Mng.Absolute);
    if not Found then
      -- Not found
      raise Invalid_Timer;
    end if;
  end Locate;

  -- Delete current timer
  procedure Delete_Current is
    Done : Boolean;
  begin
    Timer_List_Mng.Delete (Timer_List, Done => Done);
  end Delete_Current;

  -- Delete a timer
  -- May raise Invalid_Timer if timer has expired
  procedure Delete (Id : in Timer_Id) is
  begin
    -- Search timer
    Locate (Id);
    -- Delete timer
    Delete_Current;
  end Delete;

  -- Locate First timer to expire
  -- May raise Invalid_Timer if no more timer
  procedure First is
  begin
    if Timer_List_Mng.Is_Empty (Timer_List) then
      raise Invalid_Timer;
    end if;
    -- Move to beginning
    Timer_List_Mng.Rewind (Timer_List);
  end First;

  -- For each timer for which if expiration time/delay is reached
  -- its callback is called
  -- then, if periodical it is re-armed (and may expire)
  --       if not it is deleted
  -- Return True if at least one timer has expired
  function Expire return Boolean is
    Timer : Timer_Rec;
    One_True : Boolean;
    use type Ada.Calendar.Time;
  begin
    One_True := False;
    loop
      begin
        -- Search first timer
        First;
      exception
        when Invalid_Timer =>
          -- No  more timer
          exit;
      end;

      -- Get it
      Timer_List_Mng.Read (Timer_List, Timer, Timer_List_Mng.Current);
      -- Done when  no more to expire
      exit when Timer.Exp.Expiration_Time > Ada.Calendar.Clock;

      -- Expired: Remove single shot
      if Timer.Exp.Period = No_Period then
        Delete_Current;
      else
        -- Re-arm periodical, store and sort
        Timer.Exp.Expiration_Time := Timer.Exp.Expiration_Time
                                   + Timer.Exp.Period;
        Timer_List_Mng.Modify (Timer_List, Timer, Timer_List_Mng.Current);
        Sort (Timer_List);
      end if;
      -- Call callback
      if Timer.Cb = null then
        -- A timer with no cb is for generating events
        One_True := True;
      else
        if Timer.Cb ( (Timer_Num => Timer.Id), Timer.Dat) then
          -- At least this Cb has returned True
          One_True := True;
        end if;
      end if;
    end loop;

    return One_True;
  end Expire;

  -- Delay until next timer expires (or Infinite_Seconds)
  function Wait_For return Duration is
    Timer : Timer_Rec;
    Result : Duration;
    use Ada.Calendar;
  begin
    begin
      -- Search first timer and read it
      First;
    exception
      when Invalid_Timer =>
        -- No  more timer
      return Infinite_Seconds;
    end;
    Timer_List_Mng.Read (Timer_List, Timer, Timer_List_Mng.Current);

    -- Compute delay until expiration
    Result := Timer.Exp.Expiration_Time - Ada.Calendar.Clock;
    if Result < 0.0 then
      Result := 0.0;
    end if;
    return Result;
  end Wait_For;

  -- Compute next timeout from Expiration and timers
  function Next_Timeout (Expiration : Expiration_Rec) return Duration is
    Timeout_Dur, Timeout_Tim : Duration;
    use type Ada.Calendar.Time;
  begin
    -- Final timeout from the one specified
    if not Expiration.Infinite then
      Timeout_Dur := Expiration.Time - Ada.Calendar.Clock;
      -- Reached?
      if Timeout_Dur < 0.0 then
        Timeout_Dur := 0.0;
      end if;
    else
      Timeout_Dur := Infinite_Seconds;
    end if;

    -- Next timer timeout
    Timeout_Tim := Timers.Wait_For;

    -- Compute smaller timeout between timers and the one requested
    if Timeout_Tim = Infinite_Seconds then
      -- No timer
      if Expiration.Infinite then
        -- No timer and infinite timeout: infinite
        Timeout_Dur := Infinite_Seconds;
      else
        -- No timer and timeout set: keep timeout
        null;
      end if;
    else
      -- Some timer
      if Expiration.Infinite then
        -- Some timer and infinite timeout: take timer
        Timeout_Dur := Timeout_Tim;
      else
        -- Some timer and a timeout set: take smallest
        if Timeout_Tim <= Timeout_Dur then
          -- Timer is smallest
          Timeout_Dur := Timeout_Tim;
        else
          -- Keep timeout
          null;
        end if;
      end if;
    end if;

    return Timeout_Dur;
  end Next_Timeout;

  -- Is expiration reached
  function Is_Reached (Expiration : Expiration_Rec) return Boolean is
    use type Ada.Calendar.Time;
  begin
    return not Expiration.Infinite
    and then Ada.Calendar.Clock > Expiration.Time;
  end Is_Reached;

end Timers;

