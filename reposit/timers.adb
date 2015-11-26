with Ada.Calendar;
with Trace, Images, Mutexes;
package body Timers is

  -- Debugging
  package Logger is new Trace.Basic_Logger ("Timers");

  -- The mutex that protect the whole
  -- Must be recursive because timer Cb can call Timers
  Mutex : Mutexes.Mutex (Mutexes.Simple, Recursive => True);
  procedure Get_Mutex is
  begin
    Mutex.Get;
  end Get_Mutex;
  procedure Release_Mutex is
  begin
    Mutex.Release;
  end Release_Mutex;

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
  function Search_Clock is new Clocks_List_Mng.Search (Match);

  procedure Set (Dest : in out Timer_Rec; Val : in Timer_Rec) is
  begin
    Dest := Val;
  end Set;

  function Delay_Image (Delay_Spec : Delay_Rec) return String is
    use type Virtual_Time.Time;
  begin
    return (if Delay_Spec.Delay_Kind = Delay_Sec then
              "delay:" & Delay_Spec.Delay_Seconds'Img
              & ", period: " & Delay_Spec.Period'Img & " "
            else
              "exp in: " & Duration'Image (
                   Delay_Spec.Expiration_Time
                   - Virtual_Time.Current_Time (Delay_Spec.Clock))
              & ", period: " & Delay_Spec.Period'Img & " ");

  end Delay_Image;

  function Cb_Image (Callback : Timer_Callback) return String is
  begin
    return (if Callback = null then "*** No callback *** " else "");
  end Cb_Image;

  procedure Put_Debug (Proc : in String; Msg : in String) is
  begin
    Logger.Log_Debug (Proc & ": " & Msg);
  end Put_Debug;

  -- Sort timers in crescent order of expiration times
  --  if same expiration, use creation time
  -- Suspended and frozen timers are higher
  function "<" (I1, I2 : Timer_Id) return Boolean is
    T1, T2 : Timer_Rec;
    T1_Running, T2_Running : Boolean;
    use type Virtual_Time.Time, Perpet.Delta_Rec;
  begin
    I1.Get (T1);
    I2.Get (T2);
    -- Handle case when one or both are deleted
    if T1.Status /= Deleted and then T2.Status = Deleted then
      return True;
    elsif T1.Status = Deleted and then T2.Status /= Deleted then
      return False;
    elsif T1.Status = Deleted and then T2.Status = Deleted then
      return  T1.Cre < T2.Cre;
    end if;
    T1_Running := T1.Status = Running and then not T1.Frozen;
    T2_Running := T2.Status = Running and then not T2.Frozen;
    if T1_Running and then T2_Running then
      -- Both running => Expiration time
      return T1.Exp.Expiration_Time < T2.Exp.Expiration_Time
      or else (T1.Exp.Expiration_Time = T2.Exp.Expiration_Time
        and then T1.Cre < T2.Cre);
    end if;
    -- At least one suspended or frozen
    return (if T1_Running and then not T2_Running then True
            elsif not T1_Running and then T2_Running then False
            -- Both suspended or frozen: sort by remaining time
            else T1.Remaining < T2.Remaining);
  end "<";
  procedure Sort_Timers is new Timer_List_Mng.Sort ("<");
  procedure Sort (List : in out Timer_List_Mng.List_Type) renames Sort_Timers;

  -- Search Timer by Id
  function Search_Id is new Timer_List_Mng.Search ("=");

  -- Timer status, independant from the associated clock status
  function Status (Id : in Timer_Id) return Timer_Status is
    Timer : Timer_Rec;
  begin
    if not Id.Is_Set then
      -- No reference: not created or deleted
      return Deleted;
    end if;
    Id.Get (Timer);
    return Timer.Status;
  end Status;

  -- True if timer is not Deleted
  function Exists (Id : in Timer_Id) return Boolean is
  begin
    return Status (Id) /= Deleted;
  end Exists;

  -- Create a new timer
  -- May raise Invalid_Delay if Delay_Seconds is < 0
  -- Invalid_Delay : exception;
  function Create (Delay_Spec : Delay_Rec;
                   Callback   : Timer_Callback;
                   Data       : Timer_Data := No_Data) return Timer_Id is
    Id : Timer_Id;
    Timer : Timer_Rec;
    Start : Virtual_Time.Time;
    Clock : Clock_Rec;
    use type Virtual_Time.Time, Virtual_Time.Clock_Access,
             Virtual_Time.Speed_Range, Perpet.Delta_Rec;
  begin
    -- Get current time ASAP
    Timer.Cre := Ada.Calendar.Clock;
    if Delay_Spec.Delay_Kind = Delay_Sec
    and then Delay_Spec.Delay_Seconds < 0.0 then
      raise Invalid_Delay;
    end if;

    Get_Mutex;
    -- Init status and copy period and callback
    Timer.Status := Running;
    Timer.Exp.Period := Delay_Spec.Period;
    Timer.Dat := Data;
    Timer.Cb := Callback;
    Timer.Clock := Delay_Spec.Clock;
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
      Timer.Remaining := (case Delay_Spec.Delay_Kind is
          when Delay_Sec => Perpet.To_Delta_Rec (Delay_Spec.Delay_Seconds),
          when Delay_Del => Delay_Spec.Delay_Delta,
          when Delay_Exp =>
             (if (Delay_Spec.Expiration_Time > Start) then
                Delay_Spec.Expiration_Time - Start
              else Default_Delta));
      Put_Debug ("Create",
                 "frozen for " & Timer.Remaining.Days'Img & "D +  "
                               & Timer.Remaining.Secs'Img & "s");
    else
      Timer.Exp.Expiration_Time := (case Delay_Spec.Delay_Kind is
          when Delay_Sec => Start + Delay_Spec.Delay_Seconds,
          when Delay_Del => Start + Delay_Spec.Delay_Delta,
          when Delay_Exp => Delay_Spec.Expiration_Time);
      if Delay_Spec.Clock /= null then
        -- Expiration time in reference time (speed is not null)
        Timer.Exp.Expiration_Time :=
          Delay_Spec.Clock.Reference_Time_Of (Timer.Exp.Expiration_Time);
      end if;
      Put_Debug ("Create",
                 "started for "
               & Images.Date_Image (Timer.Exp.Expiration_Time));
    end if;

    -- Insert in beginning of list and sort
    Id.Init (Timer);
    Timer_List.Rewind (Check_Empty => False);
    Timer_List.Insert (Id, Timer_List_Mng.Prev);
    Sort (Timer_List);

    -- Register observer
    if Delay_Spec.Clock /= null then
      Clock.Clock := Delay_Spec.Clock;
      if not Search_Clock (Clocks_List, Clock,
                           From => Clocks_List_Mng.Current_Absolute) then
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
             & Cb_Image (Callback));
    -- Done
    Release_Mutex;
    return Id;
  end Create;

  procedure Create (Id         : in out Timer_Id;
                    Delay_Spec : in Delay_Rec;
                    Callback   : in Timer_Callback;
                    Data       : in Timer_Data := No_Data) is
  begin
    -- In case of error
    Id := No_Timer;
    Id := Create (Delay_Spec, Callback, Data);
  end Create;

  -- Locate a timer in list
  -- May raise Invalid_Timer if timer has expired
  procedure Locate (Id : in Timer_Id) is
    Tid : Timer_Id;
  begin
    -- Check validity
    if Id = No_Timer
    or else Timer_List.Is_Empty then
      raise Invalid_Timer;
    end if;
    -- Try current
    Timer_List.Read (Tid, Timer_List_Mng.Current);
    if Tid = Id then
      return;
    end if;

    -- Search timer
    if not Search_Id (Timer_List, Id,
                      From => Timer_List_Mng.Current_Absolute) then
      raise Invalid_Timer;
    end if;
  end Locate;

  -- Delete current timer
  procedure Delete_Current is
    Tid : Timer_Id;
    Timer : access Timer_Rec;
    Clock : Clock_Rec;
    Found : Boolean;
    use type Virtual_Time.Clock_Access;
  begin
    -- Read timer (to see its clock) and delete it
    Timer_List.Read (Tid, Timer_List_Mng.Current);
    Timer_List.Delete (Moved => Found);
    Timer := Tid.Get_Access;
    if Timer.Status = Deleted then
      Put_Debug ("Delete", "timer status is already deleted!!!");
      raise Invalid_Timer;
    end if;
    Timer.Status := Deleted;

    -- Update clock if any
    if Timer.Clock /= null then
      Clock.Clock := Timer.Clock;
      if not Search_Clock (Clocks_List, Clock,
                           From => Clocks_List_Mng.Current_Absolute) then
        -- Abnormal, clock shall be known
        Put_Debug ("Delete", "but its clock is unknown!!!");
        raise Invalid_Timer;
      else
        Clocks_List.Read (Clock, Clocks_List_Mng.Current);
        if Clock.Nb_Timers /= 1 then
          -- Not last timer on this clock => decr nb of timers
          Clock.Nb_Timers := Clock.Nb_Timers - 1;
          Clocks_List.Modify (Clock, Clocks_List_Mng.Current);
        else
          -- Last timer on this clock => delete clock and unregister
          Clocks_List.Delete (Moved => Found);
          Clock.Clock.Del_Observer (Observer'Access);
        end if;
      end if;
    end if;
  end Delete_Current;

  -- Delete a timer
  -- May raise Invalid_Timer if timer has expired
  procedure Delete (Id : in out Timer_Id) is
  begin
    Get_Mutex;
    -- Search timer
    Locate (Id);
    -- Delete timer
    Delete_Current;
    Put_Debug ("Delete", "deleted");
    Release_Mutex;
  end Delete;

  -- Delete a timer if it exists
  -- No exception even if Timer_Id is not set
  procedure Delete_If_Exists (Id : in out Timer_Id) is
    Timer : access Timer_Rec;
  begin
    if not Id.Is_Set then
      Put_Debug ("Delete_If_Exists", "not set");
      return;
    elsif Timer_List.Is_Empty then
      Put_Debug ("Delete_If_Exists", "empty list");
      return;
    end if;
    -- Check that timer is not already deleted
    Timer := Id.Get_Access;
    if Timer.Status = Deleted then
      Put_Debug ("Delete_If_Exists", "already deleted");
      return;
    end if;
    -- Delete timer
    Delete (Id);
    Put_Debug ("Delete_If_Exists", "deleted");
  end Delete_If_Exists;

  -- Suspend a timer: expirations, even the pending ones are suspended
  -- No action is timer is alread syspended
  -- May raise Invalid_Timer if timer has no period and has expired
  procedure Suspend (Id : in Timer_Id) is
    Timer : access Timer_Rec;
    Now : Virtual_Time.Time;
    Speed : Virtual_Time.Speed_Range;
    use type Virtual_Time.Time, Virtual_Time.Clock_Access,
             Virtual_Time.Speed_Range, Perpet.Delta_Rec;
  begin
    Get_Mutex;
    -- Get access to timer
    Timer := Id.Get_Access;
    if Timer.Status = Deleted then
      Release_Mutex;
      raise Invalid_Timer;
    elsif Timer.Status = Suspended then
      -- Already suspended
      Release_Mutex;
      return;
    end if;
    if not Timer.Frozen then
      -- Compute remaining in virtual time from expiration time
      Now := Virtual_Time.Current_Time (Timer.Clock);
      Speed := Virtual_Time.Get_Speed (Timer.Clock);
      if Timer.Exp.Expiration_Time > Now then
        Timer.Remaining := (Timer.Exp.Expiration_Time - Now)
            * Perpet.Natural_Duration(Speed);
      else
        Timer.Remaining := Default_Delta;
      end if;
    end if;
    -- Update status and re-sort
    Timer.Status := Suspended;
    Sort (Timer_List);
    Put_Debug ("Suspend", "for " & Timer.Remaining.Days'Img & "D +  "
                                  & Timer.Remaining.Secs'Img & "s");
    Release_Mutex;
  end Suspend;

  -- Resume a suspended a timer: expirations, even the pending ones are resumed
  -- No action is timer is not syspended
  -- May raise Invalid_Timer if timer has no period and has expired
  procedure Resume (Id : in Timer_Id) is
    Timer : access Timer_Rec;
    Speed : Virtual_Time.Speed_Range;
    use type Virtual_Time.Time, Perpet.Delta_Rec;
  begin
    Get_Mutex;
    -- Get access to timer
    Timer := Id.Get_Access;
    if Timer.Status = Deleted then
      Release_Mutex;
      raise Invalid_Timer;
    elsif Timer.Status = Running then
      -- Already running
      Release_Mutex;
      return;
    end if;
    if not Timer.Frozen then
      -- Re-apply remaining delay (virtual) to current expiration (reference)
      Speed := Virtual_Time.Get_Speed (Timer.Clock);
      Timer.Exp.Expiration_Time := Virtual_Time.Current_Time (null)
          + (Timer.Remaining * Perpet.Natural_Duration(1.0 / Speed));
    end if;
    -- Update status and re-sort
    Timer.Status := Running;
    Sort (Timer_List);

    --Done
    Put_Debug ("Resume",
               "restarted for "
             & Images.Date_Image (Timer.Exp.Expiration_Time));
    Release_Mutex;
  end Resume;

  -- Return the delay until expiration
  -- May raise Invalid_Timer if timer is Deleted
  function Remaining (Id : Timer_Id) return Perpet.Delta_Rec is
    Timer : access Timer_Rec;
    Now : Virtual_Time.Time;
    Speed : Virtual_Time.Speed_Range;
    use type Virtual_Time.Time, Virtual_Time.Speed_Range, Perpet.Delta_Rec;
  begin
    Get_Mutex;
    -- Get access to timer
    Timer := Id.Get_Access;
    if Timer.Status = Deleted then
      Release_Mutex;
      raise Invalid_Timer;
    elsif Timer.Frozen or else Timer.Status = Suspended then
      -- Clock frozen or timer suspended (or both)
      Release_Mutex;
      return Timer.Remaining;
    end if;
    -- Compute remaining time up to expiration
    Now := Virtual_Time.Current_Time (Timer.Clock);
    Speed := Virtual_Time.Get_Speed (Timer.Clock);
    if Timer.Exp.Expiration_Time > Now then
      return Perpet.Delta_Rec'(
         (Timer.Exp.Expiration_Time - Now) * Perpet.Natural_Duration(Speed));
    else
      return Default_Delta;
    end if;
  end Remaining;

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

  -- Clock update notification
  procedure Notify (An_Observer : in out Observer_Type;
                    Rtime, Vtime : in Virtual_Time.Time;
                    Speed : in Virtual_Time.Speed_Range;
                    Clock : in Virtual_Time.Clock_Access) is
    pragma Unreferenced (An_Observer, Vtime);
    New_Speed : constant Virtual_Time.Speed_Range := Clock.Get_Speed;

    -- Update current timer if needed
    procedure Update is
      Id : Timer_Id;
      Timer : access Timer_Rec;
      use type Virtual_Time.Time, Virtual_Time.Clock_Access,
               Virtual_Time.Speed_Range, Perpet.Delta_Rec;
    begin
      Timer_List.Read (Id, Timer_List_Mng.Current);
      Timer := Id.Get_Access;
      if Timer.Clock /= Clock or else Timer.Status = Deleted then
        return;
      end if;
      if Timer.Frozen and then New_Speed /= 0.0 then
        -- Clock restarted at Rtime, compute expiration with new speed
        if Timer.Status = Running then
          Timer.Exp.Expiration_Time := Rtime
              + (Timer.Remaining * Perpet.Natural_Duration(1.0 / New_Speed));
          Put_Debug ("Update",
                     "restarted for "
                   & Images.Date_Image (Timer.Exp.Expiration_Time));
        end if;
        Timer.Frozen := False;
      elsif not Timer.Frozen and then New_Speed = 0.0 then
        -- Clock froze at Rtime, store remaining time in previous Vtime
        if Timer.Status = Running then
          Timer.Remaining := (Timer.Exp.Expiration_Time - Rtime)
              * Perpet.Natural_Duration(Speed);
          Put_Debug ("Update",
                     "frozen for " & Timer.Remaining.Days'Img & "D +  "
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
    end Update;

  begin
    Get_Mutex;
    if not First then
      Put_Debug ("Notify", "No timer!!!");
      Release_Mutex;
      return;
    end if;
    loop
      Update;
      exit when not Timer_List.Check_Move;
      Timer_List.Move_To;
    end loop;
    Sort (Timer_List);
    Release_Mutex;
  end Notify;

end Timers;

