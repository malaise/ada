with Ada.Calendar;
with Images;
package body Timers.Expiration is


  -- Image of expiration
  function Exp_Image (Expiration : Expiration_Rec) return String is
    use type Virtual_Time.Time;
  begin
    return (if Expiration.Infinite then
              "exp infinite "
            else
              "exp in: " & Duration'Image (Expiration.Time
                                           - Ada.Calendar.Clock)
                         & " ");
  end Exp_Image;

  -- For each timer for which if expiration time/delay is reached
  -- its callback is called
  -- then, if periodical it is re-armed (and may expire)
  --       if not it is deleted
  -- Return True if at least one timer has expired
  function Expire return Boolean is
    Id : Timer_Id;
    Timer : access Timer_Rec;
    One_True : Boolean;
    use type Virtual_Time.Time;
  begin
    Get_Mutex;
    One_True := False;
    -- Search first timer, exit when no more timer
    while First loop

      -- Get it
      Timer_List.Read (Id, Timer_List_Mng.Current);
      Timer := Id.Get_Access;
      -- Done when no more to expire
      exit when Timer.Status /= Running or else Timer.Frozen
      or else Timer.Exp.Expiration_Time > Ada.Calendar.Clock;

      -- Expired: Remove single shot
      if Timer.Exp.Period = No_Period then
        Delete_Current;
      else
        -- Re-arm periodical and sort
        Timer.Exp.Expiration_Time := Timer.Exp.Expiration_Time
             + Timer.Exp.Period / Virtual_Time.Get_Speed (Timer.Clock);
        Sort (Timer_List);
      end if;
      Put_Debug ("Expire", "expiring");
      -- Call callback
      if Timer.Cb = null then
        -- A timer with no cb is for generating events
        One_True := True;
      else
        if Timer.Cb (Id, Timer.Dat) then
          -- At least this Cb has returned True
          One_True := True;
        end if;
      end if;
    end loop;

    Put_Debug ("Expire", "-> " & One_True'Img);
    Release_Mutex;
    return One_True;
  end Expire;

  -- We add this accuracy to the result of Wait_For and Next_Timeout
  --  (if it is not infinite) in order to avoid too early call to
  --  Is_Reached due to rounding in client.
  Accuracy : constant Duration := 0.001;
  procedure Adjust (Dur : in out Duration) is
  begin
    if Dur /= Infinite_Seconds then
      Dur := Dur + Accuracy;
    end if;
  end Adjust;

  -- Date of expiration of next timer
  function Wait_Until return Expiration_Rec is
    Id : Timer_Id;
    Timer : access Timer_Rec;
  begin
    Get_Mutex;
      -- Search first timer and read it
    if not First then
      -- No more timer
      Put_Debug ("Wait_Until", "-> Infinite cause no timer");
      Release_Mutex;
      return Infinite_Expiration;
    end if;
    Timer_List.Read (Id, Timer_List_Mng.Current);
    Timer := Id.Get_Access;
    if Timer.Status /= Running or else Timer.Frozen then
      -- No more running timer
      Put_Debug ("Wait_Until", "-> Infinite cause no more running timer");
      Release_Mutex;
      return Infinite_Expiration;
    end if;

    Put_Debug ("Wait_Until", "-> "
             & Images.Date_Image (Timer.Exp.Expiration_Time));
    Release_Mutex;
    return (Infinite => False, Time =>Timer.Exp.Expiration_Time);
  end Wait_Until;

  function Wait_For return Duration is
    Now : Virtual_Time.Time;
    Next_Exp : Expiration_Rec;
    Result : Duration;
    use type Virtual_Time.Time;
  begin
    -- Get next timer and substract now
    Put_Debug ("Wait_For", "");
    Now :=  Ada.Calendar.Clock;
    Next_Exp := Wait_Until;
    if Next_Exp = Infinite_Expiration then
      Result := Infinite_Seconds;
    else
      Result := Next_Exp.Time - Now;
      if Result < 0.0 then
        -- Timer should have expired
        Result := 0.0;
      else
        Adjust (Result);
      end if;
    end if;

    Put_Debug ("Wait_For", "-> " & Result'Img);
    return Result;
  end Wait_For;

  function "<" (E1, E2 : Expiration_Rec) return Boolean is
    use type Virtual_Time.Time;
  begin
    return (if E1 = Infinite_Expiration then False
            elsif E2 = Infinite_Expiration then True
            else E1.Time < E2.Time);
  end "<";

  -- Compute next timeout from Expiration and timers
  function Next_Expiration (Expiration : Expiration_Rec)
           return Expiration_Rec is
    Next_Exp : Expiration_Rec;
    Result   : Expiration_Rec;
  begin
    Get_Mutex;
    -- First timer to expire
    Next_Exp := Wait_Until;

    -- A timer is due before provided expiration
    Result := (if Next_Exp < Expiration then Next_Exp
               -- Provided expiration will occure first
               else Expiration);

    Put_Debug ("Next_Expiration", "-> " & Exp_Image (Result));
    Release_Mutex;
    return Result;
  end Next_Expiration;

  -- Is expiration reached
  function Is_Reached (Expiration : Expiration_Rec) return Boolean is
    use type Virtual_Time.Time;
  begin
    return not Expiration.Infinite
           and then Ada.Calendar.Clock > Expiration.Time;
  end Is_Reached;

end Timers.Expiration;

