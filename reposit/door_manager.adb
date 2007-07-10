package body Door_Manager is

  -- Check if the door can be open
  procedure Check_Open (A_Door : in Door) is
  begin
    if A_Door.Door_Pointer.Current >= A_Door.Door_Pointer.Expected then
      -- Release all callers of wait
      Condition_Manager.Broadcast (A_Door.Door_Pointer.Cond);
      A_Door.Door_Pointer.Current := 0;
    end if;
  end Check_Open;

  -- Get access to the door (prior getting or setting its number of waiters)
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

  procedure Check_Access (A_Door : in Door) is
  begin
    if not Condition_Manager.Is_Owner (A_Door.Door_Pointer.Cond) then
      raise Not_Owner;
    end if;
  end Check_Access;

  -- Set the expected number of waiters
  procedure Set_Nb_Waiters (A_Door : in Door;
                            To : in Positive) is
  begin
    Check_Access (A_Door);
    A_Door.Door_Pointer.Expected := To;
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
    Check_Open (A_Door);
  end Add_To_Nb_Waiters;

  -- Wait until the required number of waiters is reached
  function Wait (A_Door : Door;
                 Waiting_Time : Duration) return Boolean is
    Result : Boolean;
  begin
    Check_Access (A_Door);
    -- One more waiter
    A_Door.Door_Pointer.Current := A_Door.Door_Pointer.Current + 1;
    if A_Door.Door_Pointer.Current >= A_Door.Door_Pointer.Expected then
      -- Expected number of waiters is reached, release them
      Check_Open (A_Door);
      return True;
    else
      -- Wait
      Result := Condition_Manager.Wait (A_Door.Door_Pointer.Cond, Waiting_Time);
      if not Result then
        -- Gave up: one waiter less
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

