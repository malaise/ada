with Ada.Calendar, Ada.Text_Io;
with Dynamic_List, Event_Mng, Environ, Date_Image;
package body Timers is

  -- Debugging
  Debug_Var_Name : constant String := "TIMERS_DEBUG";
  Debug : Boolean := False;
  Debug_Set : Boolean := False;

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
    Running : Boolean;
    Remaining : Duration;
  end record;

  package Timer_Dyn_List_Mng is new Dynamic_List (Timer_Rec);
  package Timer_List_Mng renames Timer_Dyn_List_Mng.Dyn_List;
  Timer_List : Timer_List_Mng.List_Type;

  -- Sort timers in crescent order of expiration times
  --  if same expiration, use creation time
  -- Suspended timers are higher
  function "<" (T1, T2 : Timer_Rec) return Boolean is
    use type Virtual_Time.Time;
  begin
    if T1.Running and then T2.Running then
      -- Both running => Expiration time
      return T1.Exp.Expiration_Time < T2.Exp.Expiration_Time
      or else (T1.Exp.Expiration_Time = T2.Exp.Expiration_Time
        and then T1.Cre < T2.Cre);
    end if;
    -- At least one suspended
    if T1.Running and then not T2.Running then
      return True;
    elsif not T1.Running and then T2.Running then
      return False;
    else
      -- Both suspended: sort by remaining time
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
    This_Id : Timer_Id_Range;
    use type  Virtual_Time.Time, Virtual_Time.Clock_Access;
  begin
    -- Compute expiration time ASAP
    Timer.Cre := Ada.Calendar.Clock;
    Set_Debug;
    if Delay_Spec.Delay_Kind = Delay_Sec then
      if Delay_Spec.Delay_Seconds < 0.0 then
        raise Invalid_Delay;
      end if;
      -- @@@
      Timer.Exp.Expiration_Time := Timer.Cre
                                 + Delay_Spec.Delay_Seconds;
    else
      -- @@@
      Timer.Exp.Expiration_Time := Delay_Spec.Expiration_Time;
    end if;

    -- Allocate Id and copy period and callback
    Timer.Id := Get_Next_Id;
    Timer.Exp.Period := Delay_Spec.Period;
    Timer.Dat := Data;
    Timer.Cb := Callback;
    Timer.Clock := Delay_Spec.Clock;
    Timer.Running := True;
    Timer.Remaining := 0.0;

    -- Insert in beginning of list and sort
    if not Timer_List.Is_Empty then
      Timer_List.Rewind;
    end if;
    Timer_List.Insert (Timer, Timer_List_Mng.Prev);
    Sort (Timer_List);

    -- Register observer
    -- @@@

    -- If this timer is first then force wake-up of select
    This_Id := Timer.Id;
    Timer_List.Read (Timer, Timer_List_Mng.Current);
    if Timer.Id = This_Id then
      Event_Mng.Wake_Up;
    end if;

    -- Trace
    Put_Debug ("Create", Delay_Image (Delay_Spec)
             & Cb_Image (Callback)
             & " -> " & This_Id'Img);
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
    Done : Boolean;
  begin
    Timer_List.Delete (Done => Done);
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
    Now : constant Virtual_Time.Time := Ada.Calendar.Clock;
    Timer : Timer_Rec;
    use type Virtual_Time.Time;
  begin
     Set_Debug;
    -- Search timer
    Locate (Id);
    -- Get it
    Timer_List.Read (Timer, Timer_List_Mng.Current);
    if not Timer.Running then
      -- Already suspended
      return;
    end if;
    -- Update it (not running and remaining time until next expiration)
    Timer.Running := False;
    Timer.Remaining := Timer.Exp.Expiration_Time - Now;
    -- Store it and re-sort
    Timer_List.Modify (Timer, Timer_List_Mng.Current);
    Sort (Timer_List);
    Put_Debug ("Suspend", " id " & Id.Timer_Num'Img);
  end Suspend;

  -- Resume a suspended a timer: expirations, even the pending ones are resumed
  -- No action is timer is not syspended
  -- May raise Invalid_Timer if timer has no period and has expired
  procedure Resume (Id : in Timer_Id) is
    Now : constant  Virtual_Time.Time := Ada.Calendar.Clock;
    Timer : Timer_Rec;
    This_Id : Timer_Id_Range;
    use type Virtual_Time.Time;
  begin
     Set_Debug;
    -- Search timer
    Locate (Id);
    -- Get it
    Timer_List.Read (Timer, Timer_List_Mng.Current);
    if Timer.Running then
      -- Already running
      return;
    end if;
    -- Update it (running and next expiration time)
    Timer.Running := True;
    -- Re-apply remaining delay to current time
    Timer.Exp.Expiration_Time := Now + Timer.Remaining;
    -- Store it and re-sort
    Timer_List.Modify (Timer, Timer_List_Mng.Current);
    Sort (Timer_List);

    -- If this timer is first then force wake-up of select
    This_Id := Timer.Id;
    Timer_List.Read (Timer, Timer_List_Mng.Current);
    if Timer.Id = This_Id then
      Event_Mng.Wake_Up;
    end if;
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
      exit when not Timer.Running
      or else Timer.Exp.Expiration_Time > Ada.Calendar.Clock;

      -- Expired: Remove single shot
      if Timer.Exp.Period = No_Period then
        Delete_Current;
      else
        -- Re-arm periodical, store and sort
        Timer.Exp.Expiration_Time := Timer.Exp.Expiration_Time
                                   + Timer.Exp.Period;
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
    if not Timer.Running then
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

  function Next_Timeout (Expiration : Expiration_Rec) return Duration is
    Now : Virtual_Time.Time;
    Next_Exp : Expiration_Rec;
    Result : Duration;
    use type Virtual_Time.Time;
  begin
    -- Get next expiration and substract now
    Now :=  Ada.Calendar.Clock;
    Next_Exp := Next_Expiration (Expiration);

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

    Put_Debug ("Next_Timeout", "-> " & Result'Img);
    return Result;
  end Next_Timeout;

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
                    Vtime : in Virtual_Time.Time;
                    Clock : in Virtual_Time.Clock_Access) is
  begin
    -- @@@
    null;
  end Notify;

end Timers;

