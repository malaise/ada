with Ada.Calendar, Ada.Text_Io;
with Dynamic_List, Environ, Date_Image;
package body Timers is

  -- Debugging
  Debug_Var_Name : constant String := "TIMERS_DEBUG";
  Debug : Boolean := False;
  Debug_Set : Boolean := False;

  -- Clock observer and list of observed clocks
  Observer : aliased Observer_Type;
  type Clock_Rec is record
    Nb_Timers : Positive;
    Clock : Virtual_Time.Clock_Access;
  end record;
  package Clocks_Dyn_List_Mng is new Dynamic_List (Clock_Rec);
  package Clocks_List_Mng renames Clocks_Dyn_List_Mng.Dyn_List;
  Clocks_List : Clocks_List_Mng.List_Type;
  function Match (Current, Criteria : Clock_Rec)
                 return Boolean is
    use type Virtual_Time.Clock_Access;
  begin
    return Current.Clock = Criteria.Clock;
  end Match;
  procedure Search_Clock is new Clocks_List_Mng.Search (Match);

  procedure Set_Debug is
  begin
    if Debug_Set then
      return;
    end if;
    Debug := Environ.Is_Yes (Debug_Var_Name);
    Debug_Set := True;
  exception
    when others =>
      null;
  end Set_Debug;

  function Delay_Image (Delay_Spec : Delay_Rec) return String is
    use type Virtual_Time.Time;
  begin
    if Delay_Spec.Delay_Kind = Delay_Sec then
      return "delay:" & Delay_Spec.Delay_Seconds'Img
           & ", period: " & Delay_Spec.Period'Img & " ";
    else
      return "exp in: " & Duration'Image (
                   Delay_Spec.Expiration_Time
                   - Virtual_Time.Current_Time (Delay_Spec.Clock))
           & ", period: " & Delay_Spec.Period'Img & " ";

    end if;
  end Delay_Image;

  function Exp_Image (Expiration : Expiration_Rec) return String is
    use type Virtual_Time.Time;
  begin
    if Expiration.Infinite then
      return "exp infinite ";
    else
      return "exp in: " & Duration'Image (Expiration.Time
                                          - Ada.Calendar.Clock)
                        & " ";
    end if;
  end Exp_Image;

  function Cb_Image (Callback : Timer_Callback) return String is
  begin
    if Callback = null then
      return ("*** No callback *** ");
    else
      return "";
    end if;
  end Cb_Image;

  procedure Put_Debug (Proc : in String; Msg : in String) is
  begin
    if not Debug then
      return;
    end if;
    Ada.Text_Io.Put_Line (
        Date_Image (Ada.Calendar.Clock)(12 .. 23)
      & " Timers." & Proc
      & ": " & Msg);
  end Put_Debug;

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

  -- List of timers, running or suspended
  subtype Exp_Rec is Delay_Rec(Delay_Exp);
  type Timer_Rec is record
    Id  : Timer_Id_Range;
    Exp : Exp_Rec;
    Cre : Virtual_Time.Time;
    Dat : Timer_Data;
    Cb  : Timer_Callback;
    Clock : Virtual_Time.Clock_Access;
    -- Suspend/Resume
    Suspended : Boolean;
    -- Clock speed 0
    Frozen : Boolean;
    Remaining : Perpet.Delta_Rec;
  end record;

  package Timer_Dyn_List_Mng is new Dynamic_List (Timer_Rec);
  package Timer_List_Mng renames Timer_Dyn_List_Mng.Dyn_List;
  Timer_List : Timer_List_Mng.List_Type;

  -- Sort timers in crescent order of expiration times
  --  if same expiration, use creation time
  -- Suspended and frozen timers are higher
  function "<" (T1, T2 : Timer_Rec) return Boolean is
    use type Virtual_Time.Time, Perpet.Delta_Rec;
    T1_Running : constant Boolean := not T1.Suspended and then not T1.Frozen;
    T2_Running : constant Boolean := not T2.Suspended and then not T2.Frozen;
  begin
    if T1_Running and then T2_Running then
      -- Both running => Expiration time
      return T1.Exp.Expiration_Time < T2.Exp.Expiration_Time
      or else (T1.Exp.Expiration_Time = T2.Exp.Expiration_Time
        and then T1.Cre < T2.Cre);
    end if;
    -- At least one suspended or frozen
    if T1_Running and then not T2_Running then
      return True;
    elsif not T1_Running and then T2_Running then
      return False;
    else
      -- Both suspended or frozen: sort by remaining time
      return T1.Remaining < T2.Remaining;
    end if;
  end "<";
  procedure Sort is new Timer_List_Mng.Sort ("<");

  -- Search Timer by Id
  function Id_Match (T1, T2 : Timer_Rec) return Boolean is
  begin
    return T1.Id = T2.Id;
  end Id_Match;
  procedure Search_Id is new Timer_List_Mng.Search (Id_Match);

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
    Start : Virtual_Time.Time;
    Clock : Clock_Rec;
    Found : Boolean;
    use type Virtual_Time.Time, Virtual_Time.Clock_Access,
             Virtual_Time.Speed_Range, Perpet.Delta_Rec;
  begin
    -- Get current time ASAP
    Timer.Cre := Ada.Calendar.Clock;
    Set_Debug;
    if Delay_Spec.Delay_Kind = Delay_Sec
    and then Delay_Spec.Delay_Seconds < 0.0 then
      raise Invalid_Delay;
    end if;

    -- Allocate Id and copy period and callback
    Timer.Id := Get_Next_Id;
    Timer.Exp.Period := Delay_Spec.Period;
    Timer.Dat := Data;
    Timer.Cb := Callback;
    Timer.Clock := Delay_Spec.Clock;
    Timer.Suspended := False;
    Timer.Frozen := Delay_Spec.Clock /= null
           and then Delay_Spec.Clock.Get_Speed = 0.0;

    -- Start time in virtual or real time
    if Delay_Spec.Clock /= null then
      Start := Delay_Spec.Clock.Virtual_Time_Of (Timer.Cre);
    else
      Start := Timer.Cre;
    end if;

    -- Compute expiration time or remaining delay (if clock frozen)
    if Timer.Frozen then
      -- Remaining virtual time
      case Delay_Spec.Delay_Kind is
        when Delay_Sec =>
          Timer.Remaining := Perpet.To_Delta_Rec (Delay_Spec.Delay_Seconds);
        when Delay_Del =>
          Timer.Remaining := Delay_Spec.Delay_Delta;
        when Delay_Exp =>
          if (Delay_Spec.Expiration_Time > Start) then
            Timer.Remaining := Delay_Spec.Expiration_Time - Start;
          else
            Timer.Remaining := Default_Delta;
          end if;
      end case;
      Put_Debug ("Create ", Timer.Id'Img & " frozen for "
        & Timer.Remaining.Days'Img & "D +  "
        & Timer.Remaining.Secs'Img & "s");
    else
      case Delay_Spec.Delay_Kind is
        when Delay_Sec =>
          Timer.Exp.Expiration_Time := Start
                                     + Delay_Spec.Delay_Seconds;
        when Delay_Del =>
          Timer.Exp.Expiration_Time := Start
                                     + Delay_Spec.Delay_Delta;
        when Delay_Exp =>
          Timer.Exp.Expiration_Time := Delay_Spec.Expiration_Time;
      end case;
      if Delay_Spec.Clock /= null then
        -- Expiration time in reference time (speed is not null)
        Timer.Exp.Expiration_Time :=
          Delay_Spec.Clock.Reference_Time_Of (Timer.Exp.Expiration_Time);
      end if;
      Put_Debug ("Create ", Timer.Id'Img & " started for "
                    & Date_Image (Timer.Exp.Expiration_Time));
    end if;

    -- Insert in beginning of list and sort
    if not Timer_List.Is_Empty then
      Timer_List.Rewind;
    end if;
    Timer_List.Insert (Timer, Timer_List_Mng.Prev);
    Sort (Timer_List);

    -- Register observer
    if Delay_Spec.Clock /= null then
      Clock.Clock := Delay_Spec.Clock;
      Search_Clock (Clocks_List, Found, Clock,
                    From => Clocks_List_Mng.Absolute);
      if not Found then
        -- This clock not used so far: insert it and register
        Clock.Nb_Timers := 1;
        Clocks_List.Insert (Clock);
        Delay_Spec.Clock.Add_Observer (Observer'Access);
      else
        -- This clock already known => incr its nb of timers
        Clocks_List.Read (Clock, Clocks_List_Mng.Current);
        Clock.Nb_Timers := Clock.Nb_Timers + 1;
        Clocks_List.Modify (Clock, Clocks_List_Mng.Current);
      end if;
    end if;

    -- Trace
    Put_Debug ("Create", Delay_Image (Delay_Spec)
             & Cb_Image (Callback)
             & " -> " & Timer.Id'Img);
    -- Done
    return (Timer_Num => Timer.Id);
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
    if Timer_List.Is_Empty then
      raise Invalid_Timer;
    end if;
    -- Try current
    Timer_List.Read (Timer, Timer_List_Mng.Current);
    if Timer.Id = Id.Timer_Num then
      return ;
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
    Timer : Timer_Rec;
    Clock : Clock_Rec;
    Found : Boolean;
    use type Virtual_Time.Clock_Access;
  begin
    -- Read timer (to see its clock) and delete it
    Timer_List.Read (Timer, Timer_List_Mng.Current);
    Timer_List.Delete (Done => Found);

    -- Update clock if any
    if Timer.Clock /= null then
      Clock.Clock := Timer.Clock;
      Search_Clock (Clocks_List, Found, Clock,
                    From => Clocks_List_Mng.Absolute);
      if not Found then
        -- Abnormal, clock shall be known
        Put_Debug ("Delete ", Timer.Id'Img
                 & " is being deleted but its clock is unknown!!!");
        raise Invalid_Timer;
      else
        Clocks_List.Read (Clock, Clocks_List_Mng.Current);
        if Clock.Nb_Timers /= 1 then
          -- Not last timer on this clock => decr nb of timers
          Clock.Nb_Timers := Clock.Nb_Timers - 1;
          Clocks_List.Modify (Clock, Clocks_List_Mng.Current);
        else
          -- Last timer on this clock => delete clock and unregister
          Clocks_List.Delete (Done => Found);
          Clock.Clock.Del_Observer (Observer'Access);
        end if;
      end if;
    end if;
  end Delete_Current;

  -- Delete a timer
  -- May raise Invalid_Timer if timer has expired
  procedure Delete (Id : in Timer_Id) is
  begin
    Set_Debug;
    -- Search timer
    Locate (Id);
    -- Delete timer
    Delete_Current;
    Put_Debug ("Delete", " id " & Id.Timer_Num'Img);
  end Delete;

  -- Suspend a timer: expirations, even the pending ones are suspended
  -- No action is timer is alread syspended
  -- May raise Invalid_Timer if timer has no period and has expired
  procedure Suspend (Id : in Timer_Id) is
    Timer : Timer_Rec;
    Now : Virtual_Time.Time;
    Speed : Virtual_Time.Speed_Range;
    use type Virtual_Time.Time, Virtual_Time.Clock_Access,
             Virtual_Time.Speed_Range, Perpet.Delta_Rec;
  begin
     Set_Debug;
    -- Search timer
    Locate (Id);
    -- Get it
    Timer_List.Read (Timer, Timer_List_Mng.Current);
    if Timer.Suspended then
      -- Already suspended
      return;
    end if;
    if not Timer.Frozen then
      -- Compute remaining in virtual time
      Now := Virtual_Time.Current_Time (Timer.Clock);
      Speed := Virtual_Time.Get_Speed (Timer.Clock);
      if Timer.Exp.Expiration_Time > Now then
        Timer.Remaining := (Timer.Exp.Expiration_Time - Now)
            * Perpet.Natural_Duration(Speed);
      else
        Timer.Exp.Expiration_Time := Now;
      end if;
    end if;
    -- Store it and re-sort
    Timer.Suspended := True;
    Timer_List.Modify (Timer, Timer_List_Mng.Current);
    Sort (Timer_List);
    Put_Debug ("Suspend", Id.Timer_Num'Img & " for "
        & Timer.Remaining.Days'Img & "D +  "
        & Timer.Remaining.Secs'Img & "s");
  end Suspend;

  -- Resume a suspended a timer: expirations, even the pending ones are resumed
  -- No action is timer is not syspended
  -- May raise Invalid_Timer if timer has no period and has expired
  procedure Resume (Id : in Timer_Id) is
    Timer : Timer_Rec;
    Speed : Virtual_Time.Speed_Range;
    use type Virtual_Time.Time, Perpet.Delta_Rec;
  begin
     Set_Debug;
    -- Search timer
    Locate (Id);
    -- Get it
    Timer_List.Read (Timer, Timer_List_Mng.Current);
    if not Timer.Suspended then
      -- Already running
      return;
    end if;
    if not Timer.Frozen then
      -- Re-apply remaining delay (virtual) to current expiration (reference)
      Speed := Virtual_Time.Get_Speed (Timer.Clock);
      Timer.Exp.Expiration_Time := Virtual_Time.Current_Time (null)
          + (Timer.Remaining * Perpet.Natural_Duration(1.0 / Speed));
    end if;
    -- Store it and re-sort
    Timer.Suspended := False;
    Timer_List.Modify (Timer, Timer_List_Mng.Current);
    Sort (Timer_List);

    --Done
    Put_Debug ("Resume ", Timer.Id'Img & " restarted for "
                    & Date_Image (Timer.Exp.Expiration_Time));

  end Resume;

  -- Locate First timer to expire
  -- Retuns False if no more timer
  function First return Boolean is
  begin
    if Timer_List.Is_Empty then
      return False;
    end if;
    -- Move to beginning
    Timer_List.Rewind;
    return True;
  end First;

  -- For each timer for which if expiration time/delay is reached
  -- its callback is called
  -- then, if periodical it is re-armed (and may expire)
  --       if not it is deleted
  -- Return True if at least one timer has expired
  function Expire return Boolean is
    Timer : Timer_Rec;
    One_True : Boolean;
    use type Virtual_Time.Time;
  begin
    Set_Debug;
    One_True := False;
    loop
      -- Search first timer, exit when no more timer
      exit when not First;

      -- Get it
      Timer_List.Read (Timer, Timer_List_Mng.Current);
      -- Done when no more to expire
      exit when Timer.Suspended or else Timer.Frozen
      or else Timer.Exp.Expiration_Time > Ada.Calendar.Clock;

      -- Expired: Remove single shot
      if Timer.Exp.Period = No_Period then
        Delete_Current;
      else
        -- Re-arm periodical, store and sort
        Timer.Exp.Expiration_Time := Timer.Exp.Expiration_Time
             + Timer.Exp.Period / Virtual_Time.Get_Speed (Timer.Clock);
        Timer_List.Modify (Timer, Timer_List_Mng.Current);
        Sort (Timer_List);
      end if;
      Put_Debug ("Expire", "expiring id " & Timer.Id'Img);
      -- Call callback
      if Timer.Cb = null then
        -- A timer with no cb is for generating events
        One_True := True;
      else
        if Timer.Cb ( (Timer_Num => Timer.Id),
                      Timer.Dat) then
          -- At least this Cb has returned True
          One_True := True;
        end if;
      end if;
    end loop;

    Put_Debug ("Expire", "-> " & One_True'Img);
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

  -- Expiration of next timer
  function Wait_Until return Expiration_Rec is
    Timer : Timer_Rec;
  begin
    Set_Debug;
      -- Search first timer and read it
    if not First then
      -- No more timer
      Put_Debug ("Wait_Until", "-> Infinite cause no timer");
      return Infinite_Expiration;
    end if;
    Timer_List.Read (Timer, Timer_List_Mng.Current);
    if Timer.Suspended or else Timer.Frozen then
      -- No more running timer
      Put_Debug ("Wait_Until", "-> Infinite cause no more running timer"
                 & Timer.Id'Img);
      return Infinite_Expiration;
    end if;

    Put_Debug ("Wait_Until", "-> "
                           & Date_Image (Timer.Exp.Expiration_Time)
                           & " cause id " & Timer.Id'Img);
    return (Infinite => False, Time =>Timer.Exp.Expiration_Time);
  end Wait_Until;

  function Wait_For return Duration is
    Now : Virtual_Time.Time;
    Next_Exp : Expiration_Rec;
    Result : Duration;
    use type Virtual_Time.Time;
  begin
    -- Get next timer and substract now
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
    if E1 = Infinite_Expiration then
      return False;
    elsif E2 = Infinite_Expiration then
      return True;
    else
     return E1.Time < E2.Time;
    end if;
  end "<";

  -- Compute next timeout from Expiration and timers
  function Next_Expiration (Expiration : Expiration_Rec)
           return Expiration_Rec is
    Next_Exp : Expiration_Rec;
    Result   : Expiration_Rec;
    use type Virtual_Time.Time;
  begin
    -- First timer to expire
    Next_Exp := Wait_Until;

    if Next_Exp < Expiration then
      -- A timer is due before provided expiration
      Result := Next_Exp;
    else
      -- Provided expiration will occure first
      Result := Expiration;
    end if;

    if Result.Infinite then
      Put_Debug ("Next_Expiration", "-> infinite");
    else
      Put_Debug ("Next_Expiration", "-> " & Date_Image (Result.Time));
    end if;
    return Result;
  end Next_Expiration;

  -- Is expiration reached
  function Is_Reached (Expiration : Expiration_Rec) return Boolean is
    use type Virtual_Time.Time;
    Result : Boolean;
  begin
    Set_Debug;
    Result := not Expiration.Infinite
              and then Ada.Calendar.Clock > Expiration.Time;
    Put_Debug ("Is_Reached", Exp_Image(Expiration)
                           & " -> " & Result'Img);
    return Result;
  end Is_Reached;

  -- Clock update notification
  procedure Notify (An_Observer : in out Observer_Type;
                    Rtime, Vtime : in Virtual_Time.Time;
                    Speed : in Virtual_Time.Speed_Range;
                    Clock : in Virtual_Time.Clock_Access) is
    pragma Unreferenced (An_Observer, Vtime);
    New_Speed : constant Virtual_Time.Speed_Range := Clock.Get_Speed;

    -- Update current timer if needed
    procedure Update is
      Timer : Timer_Rec;
      use type Virtual_Time.Time, Virtual_Time.Clock_Access,
               Virtual_Time.Speed_Range, Perpet.Delta_Rec;
    begin
      Timer_List.Read (Timer, Timer_List_Mng.Current);
      if Timer.Clock /= Clock then
        return;
      end if;
      if Timer.Frozen and then New_Speed /= 0.0 then
        -- Clock restarted at Rtime, compute expiration with new speed
        if not Timer.Suspended then
          Timer.Exp.Expiration_Time := Rtime
              + (Timer.Remaining * Perpet.Natural_Duration(1.0 / New_Speed));
          Put_Debug ("Update ", Timer.Id'Img & " restarted for "
                    & Date_Image (Timer.Exp.Expiration_Time));
        end if;
        Timer.Frozen := False;
      elsif not Timer.Frozen and then New_Speed = 0.0 then
        -- Clock froze at Rtime, store remaining time in previous Vtime
        if not Timer.Suspended then
          Timer.Remaining := (Timer.Exp.Expiration_Time - Rtime)
              * Perpet.Natural_Duration(Speed);
          Put_Debug ("Update ", Timer.Id'Img & " frozen for "
              & Timer.Remaining.Days'Img & "D +  "
              & Timer.Remaining.Secs'Img & "s");
        end if;
        Timer.Frozen := True;
      elsif not Timer.Frozen then
        -- Jump or change speed while timer is running
        -- Apply speed factor and add to now
        Timer.Exp.Expiration_Time :=
              (Timer.Exp.Expiration_Time - Rtime) * Duration(Speed / New_Speed)
            + Virtual_Time.Current_Time (null);
      end if;
      Timer_List.Modify (Timer, Timer_List_Mng.Current);
    end Update;

  begin
    if not First then
      Put_Debug ("Notify", "No timer!!!");
      return;
    end if;
    loop
      Update;
      exit when not Timer_List.Check_Move;
      Timer_List.Move_To;
    end loop;
    Sort (Timer_List);
  end Notify;

end Timers;

