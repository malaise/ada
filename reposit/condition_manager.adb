package body Condition_Manager is

  -- The protected object which implements the condition
  protected body Condition_Protect is

    -- Get the mutex
    entry Get when Free is
    begin
      Free := False;
    end Get;

    -- Release the mutex
    procedure Release is
    begin
      -- The mutex must be acquired
      if Free then
        raise Condition_Is_Free;
      end if;
      Free := True;
    end Release;

    -- The entry to register to wait (release mutex)
    entry Wait when True is
    begin
      -- Atomically release the mutex and wait
      Release;
      requeue Wakeup_Queue;
    end Wait;

    -- Wakeup one waiting task
    procedure Signal is
    begin
      if Wakeup_Queue'Count /= 0 then
        State := Signaled;
      end if;
    end Signal;

    -- Wakeup all waiting tasks
    procedure Broadcast is
    begin
      if Wakeup_Queue'Count /= 0 then
        State := Broadcasted;
      end if;
    end Broadcast;

    -- The queue of waiting tasks
    entry Wakeup_Queue when State /= Blocked is
    begin
      -- Current task is released by a signal or a broadcast
      if State = Signaled then
        -- Only the first task must be released, others remain waiting
        State := Blocked;
      else
        -- Last task re-blocks the condition
        if Wakeup_Queue'Count = 0 then
          State := Blocked;
        end if;
      end if;
      -- Acquire the mutex refore releasing the task
      requeue Get;
    end Wakeup_Queue;

  end Condition_Protect;

  -- Get access to the condition
  -- If delay is negative, wait until mutex is got
  -- If delay is null, try and give up if not free
  -- If delay is positive, try during the specified delay
  function Get (A_Condition : Condition;
                Waiting_Time : Duration) return Boolean is
    Result : Boolean;
  begin
    if Waiting_Time < 0.0 then
      -- Negative delay : unconditional waiting
      A_Condition.Condition_Pointer.Get;
      Result := True;
    else
      -- Delay
      select
        A_Condition.Condition_Pointer.Get;
        Result := True;
      or
        delay Waiting_Time;
        Result := False;
      end select;
    end if;
    return Result;
  end Get;

  -- Get access to the condition : infinite wait
  procedure Get (A_Condition : in Condition) is
    Dummy : Boolean;
  begin
    Dummy := Get (A_Condition, -1.0);
  end Get;

  -- Exception Condition_Is_Free is raised if the condition was already
  --  free.
  procedure Release (A_Condition : in Condition) is
  begin
    A_Condition.Condition_Pointer.Release;
  end Release;

  -- The calling task must own the condition's mutex
  -- Atomically release the mutex and block the calling task on the condition
  -- Upon successful return, the mutex is already allocated to the calling
  --   task
  function Wait (A_Condition  : Condition;
                 Waiting_Time : Duration)  return Boolean is
    Result : Boolean;
  begin
    if Waiting_Time < 0.0 then
      -- Negative delay : unconditional waiting
      A_Condition.Condition_Pointer.Wait;
      Result := True;
    else
      -- Delay
      select
        A_Condition.Condition_Pointer.Wait;
        Result := True;
      or
        delay Waiting_Time;
        Result := False;
      end select;
    end if;
    return Result;
  end Wait;

  procedure Wait (A_Condition  : in Condition) is
  begin
    A_Condition.Condition_Pointer.Wait;
  end Wait;

  -- Unblock at least one of the blocked tasks
  procedure Signal (A_Condition  : in Condition) is
  begin
    A_Condition.Condition_Pointer.Signal;
  end Signal;

  -- Unblock all the blocked tasks
  procedure Broadcast (A_Condition  : in Condition) is
  begin
    A_Condition.Condition_Pointer.Broadcast;
  end Broadcast;

end Condition_Manager;


