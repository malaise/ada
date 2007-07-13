package body Door_Manager is

  -- Access to condition --
  -- Get access to the condition
  function Get (A_Door : Door;
                Waiting_Time : Duration) return Boolean is
  begin
    return Condition_Manager.Get (A_Door.Door_Pointer.Cond, Waiting_Time);
  end Get;

  -- Get access to the condition : infinite wait
  procedure Get (A_Door : in Door) is
  begin
    Condition_Manager.Get (A_Door.Door_Pointer.Cond);
  end Get;

  -- Release access to the condition
  procedure Release (A_Door : in Door) is
  begin
    Condition_Manager.Release (A_Door.Door_Pointer.Cond);
  end Release;


  -- Utilities --
  -- Open the door if number of waiters is reached
  function Check_Open (A_Door : in Door) return Boolean is
  begin
    if A_Door.Door_Pointer.Current >= A_Door.Door_Pointer.Expected then
      -- Release all waiters of wait
      Condition_Manager.Broadcast (A_Door.Door_Pointer.Cond);
      A_Door.Door_Pointer.Current := 0;
      return True;
    else
      return False;
    end if;
  end Check_Open;
  procedure Check_Open (A_Door : in Door) is
    Dummy : Boolean;
  begin
    Dummy := Check_Open (A_Door);
  end Check_Open;

  -- Check that current tasks has access
  procedure Check_Access (A_Door : in Door) is
  begin
    if not Condition_Manager.Is_Owner (A_Door.Door_Pointer.Cond) then
      raise Not_Owner;
    end if;
  end Check_Access;


  -- Set/get nb waiters --
  -- Set the expected number of waiters
  procedure Set_Nb_Waiters (A_Door : in Door;
                            To : in Positive) is
  begin
    Check_Access (A_Door);
    A_Door.Door_Pointer.Expected := To;
    -- This may lead to open the door
    Check_Open (A_Door);
  end Set_Nb_Waiters;

  -- Get the expected number of waiters
  function Get_Nb_Waiters (A_Door : in Door) return Positive is
  begin
    Check_Access (A_Door);
    return A_Door.Door_Pointer.Expected;
  end Get_Nb_Waiters;

  -- Increment/decrement the expected number of waiters
  -- (The expected number of waiters remains always Positive)
  procedure Add_To_Nb_Waiters (A_Door : in Door;
                               Val : in Integer) is
  begin
    Check_Access (A_Door);
    begin
      if A_Door.Door_Pointer.Expected + Val >= 1 then
        A_Door.Door_Pointer.Expected := A_Door.Door_Pointer.Expected + Val;
      else
        A_Door.Door_Pointer.Expected := 1;
      end if;
    exception
      -- Overflow
      when Constraint_Error =>
        A_Door.Door_Pointer.Expected := Positive'Last;
    end;
    -- This may lead to open the door
    Check_Open (A_Door);
  end Add_To_Nb_Waiters;


  -- Wait --
  -- Wait until the required number of waiters is reached
  function Wait (A_Door : Door;
                 Waiting_Time : Duration) return Boolean is
    Result : Boolean;
  begin
    Check_Access (A_Door);
    -- One more waiter
    A_Door.Door_Pointer.Current := A_Door.Door_Pointer.Current + 1;
    if Check_Open (A_Door) then
      -- Expected number of waiters is reached, release them
      return True;
    else
      -- Wait
      Result := Condition_Manager.Wait (A_Door.Door_Pointer.Cond, Waiting_Time);
      if not Result then
        -- Giving up: one waiter less
        A_Door.Door_Pointer.Current := A_Door.Door_Pointer.Current - 1;
      end if;
      return Result;
    end if;
  end Wait;

  procedure Wait (A_Door : in Door) is
    Ok : Boolean;
  begin
    Ok := Wait (A_Door, -1.0);
  end Wait;

end Door_Manager;

