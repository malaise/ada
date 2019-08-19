package body Virtual_Time is

  -- Affection of observer
  procedure Set (To : out Observer_Access; Val : in Observer_Access) is
  begin
    To := Val;
  end Set;

  -- Get current time
  function Current_Time (A_Clock : Clock) return Time is
    (Virtual_Time_Of (A_Clock, Ada.Calendar.Clock));

  -- Use Ada.Calendar.Clock if A_Clock is null
  function Current_Time (A_Clock : Clock_Access := null) return Time is
    -- No clock => real time
    (if A_Clock = null then Ada.Calendar.Clock
     else Current_Time (A_Clock.all));

  -- Is Time reached
  function Is_Reached (Expiration_Time : Time;
                       A_Clock : Clock) return Boolean is
    use type Ada.Calendar.Time;
  begin
    return Current_Time (A_Clock) > Expiration_Time;
  end Is_Reached;
  function Is_Reached (Expiration_Time : Time;
                       A_Clock : Clock_Access := null) return Boolean is
    use type Ada.Calendar.Time;
  begin
    return Current_Time (A_Clock) > Expiration_Time;
  end Is_Reached;

  -- Notify observers of a clock change
  procedure Notify_Observers (A_Clock : in out Clock;
                              Rtime, Vtime : in Time;
                              Speed : in Speed_Range) is
    Obs : Observer_Access;
  begin
    if A_Clock.Observers.Is_Empty then
      return;
    end if;
    A_Clock.Observers.Rewind;
    loop
      A_Clock.Observers.Read (Obs, List_Mng.Current);
      begin
        Obs.all.Notify (Rtime, Vtime, Speed, A_Clock'Unrestricted_Access);
      exception
        when others => null;
      end;
      exit when not A_Clock.Observers.Check_Move;
      A_Clock.Observers.Move_To;
    end loop;
  end Notify_Observers;

  -- Set a new synchro point
  procedure Set_Time (A_Clock : in out Clock;
                      Reference_Time : in Time;
                      Virtual_Time : in Time) is
    Rtime : constant Time := Ada.Calendar.Clock;
    Vtime : constant Time := Virtual_Time_Of (A_Clock, Rtime);
  begin
    if A_Clock.Speed /= Frozen then
      raise Vtime_Error;
    end if;
    A_Clock.Refe_Time := Reference_Time;
    A_Clock.Virt_Time := Virtual_Time;
    Notify_Observers (A_Clock, Rtime, Vtime, A_Clock.Speed);
  end Set_Time;

   -- Get current synchro point
  procedure Get_Synchro (A_Clock : in Clock;
                         Reference_Time : out Time;
                         Virtual_Time : out Time) is
  begin
    Reference_Time := A_Clock.Refe_Time;
    Virtual_Time := A_Clock.Virt_Time;
  end Get_Synchro;

  -- Return  Ada.Calendar.Clock if A_Clock is null
  procedure Get_Synchro (A_Clock : in Clock_Access;
                         Reference_Time : out Time;
                         Virtual_Time : out Time) is
    Now : Time;
  begin
    if A_Clock = null then
      -- No clock => real time
      Now := Ada.Calendar.Clock;
      Reference_Time := Now;
      Virtual_Time := Now;
    else
      Get_Synchro (A_Clock.all, Reference_Time, Virtual_Time);
    end if;
  end Get_Synchro;

  -- Set a new speed
  procedure Set_Speed (A_Clock : in out Clock;
                       Speed : in Speed_Range) is
    Now : constant Time := Ada.Calendar.Clock;
    Prev_Speed : constant Speed_Range := A_Clock.Speed;
  begin
    -- Set synchro point to now (with previous speed)
    A_Clock.Virt_Time := Virtual_Time_Of (A_Clock, Now);
    A_Clock.Refe_Time := Now;
    -- Set new speed
    A_Clock.Speed := Speed;
    Notify_Observers (A_Clock, A_Clock.Refe_Time, A_Clock.Virt_Time,
                      Prev_Speed);
  end Set_Speed;

  -- Set speed to 0.0 and save previous speed for a later resume
  procedure Suspend (A_Clock : in out Clock) is
  begin
    if A_Clock.Suspended then
      return;
    end if;
    -- Save previous speed and set speed to 0.0 and save previous speed
    A_Clock.Prev_Speed := A_Clock.Speed;
    Set_Speed (A_Clock, Frozen);
    A_Clock.Suspended := True;
  end Suspend;

  -- Resume is suspended, no effect otherwise
  procedure Resume (A_Clock : in out Clock) is
  begin
    if not A_Clock.Suspended then
      return;
    end if;
    -- Restore saved speed
    Set_Speed (A_Clock, A_Clock.Prev_Speed);
    A_Clock.Suspended := False;
  end Resume;

  -- Get current speed
  function Get_Speed (A_Clock : Clock) return Speed_Range is (A_Clock.Speed);

  -- Return 1.0 if A_Clock is null
  function Get_Speed (A_Clock : Clock_Access) return Speed_Range is
    (if A_Clock = null then Std else Get_Speed (A_Clock.all));

  -- Get Virtual time corresponding to a Reference time
  function Virtual_Time_Of (A_Clock : Clock;
                            Reference_Time : Time) return Time is
    use type Time;
  begin
    -- (Cur_Virt - Ref_Virt) / (Cur_Real - Ref_Real) = Speed
    -- Cur_Virt = (Cur_Real - Ref_Real) * Speed + Ref_Virt
    return (Reference_Time - A_Clock.Refe_Time) * A_Clock.Speed
           + A_Clock.Virt_Time;
  end Virtual_Time_Of;

  -- Return Reference_Time if A_Clock is null
  function Virtual_Time_Of (A_Clock : Clock_Access;
                            Reference_Time : Time) return Time is
    (if A_Clock = null then Reference_Time
     else Virtual_Time_Of (A_Clock.all, Reference_Time));

  -- Get Reference time corresponding to a Virtual time
  function Reference_Time_Of (A_Clock : Clock;
                              Virtual_Time : Time) return Time is
    use type Time;
  begin
    -- (Cur_Virt - Ref_Virt) / (Cur_Real - Ref_Real) = Speed
    -- Cur_Real = (Cur_Virt - Ref_Virt) / speed + Ref_Real
    if A_Clock.Speed = Frozen then
      raise Vtime_Error;
    end if;
    return (Virtual_Time - A_Clock.Virt_Time) / A_Clock.Speed
           + A_Clock.Refe_Time;
  end Reference_Time_Of;

  -- Return Virtual_Time is A_Clock is null
  function Reference_Time_Of (A_Clock : Clock_Access;
                              Virtual_Time : Time) return Time is
    (if A_Clock = null then Virtual_Time
     else Reference_Time_Of (A_Clock.all, Virtual_Time));

  -- Add a new observer
  procedure Add_Observer (A_Clock : in out Clock;
                          An_Observer :  not null access Observer'Class) is
  begin
    A_Clock.Observers.Rewind (List_Mng.Prev, False);
    A_Clock.Observers.Insert (Observer_Access(An_Observer));
  end Add_Observer;

  -- Del an observer
  procedure Del_Observer (A_Clock : in out Clock;
                          An_Observer :  not null access Observer'Class) is
    Obs : Observer_Access;
  begin
    if A_Clock.Observers.Is_Empty then
      return;
    end if;
    -- Iterate until this observer is found (and delete it) or
    --  until end of list
    A_Clock.Observers.Rewind;
    loop
      A_Clock.Observers.Read (Obs, List_Mng.Current);
      if Obs = Observer_Access (An_Observer) then
        -- This observer matches: delete and exit
        if A_Clock.Observers.Check_Move then
          A_Clock.Observers.Delete (List_Mng.Next);
        else
          A_Clock.Observers.Delete (List_Mng.Prev);
        end if;
        exit;
      end if;
      exit when not A_Clock.Observers.Check_Move;
      A_Clock.Observers.Move_To;
    end loop;
  end Del_Observer;

end Virtual_Time;

