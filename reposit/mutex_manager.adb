package body Mutex_Manager is

  -- The protected object which implements the simple mutex
  protected body Mutex_Protect is
    -- Mutex is free at creation
    entry Mutex_Get when Free is
    begin
      Free := False;
    end Mutex_Get;

    procedure Mutex_Release (Status : out Boolean) is
    begin
      -- Mutex is released, but status is set to false if it was already free
      Status := not Free;
      Free := True;
    end Mutex_Release;
  end Mutex_Protect;

  ----------------------------------------------------------------------------

  -- The protected object which implements the read/write mutex
  protected body Rw_Mutex_Protect is

    -- Gets the lock. Blocking.
    -- Do not let a new request be inserted in the middle of a queue while
    -- we are swapping queues.
    entry Mutex_Get (Kind : in Access_Kind) when not Swapping is
    begin
      -- We are sure that, while a writer is writing, any kind of request
      --  must wait (Open = False). In this case the release will
      --  open the queue
      -- In all other cases, request will pass the entry for active check
      if not Writer then
        Open := True;
      end if;
      -- Queue the request in the active queue
      requeue Queues (Current_Queue) with abort;
    end Mutex_Get;

    -- Releases the lock. No Check of kind but the lock must have been
    -- got.
    procedure Mutex_Release (Status : out Boolean) is
    begin
      -- Default result is OK.
      Status := True;
      if Readers > 0 then
        -- There are readers, one of them is releasing the lock
        Readers := Readers - 1;
        if Readers = 0 then
          -- The last reader leaves, so the lock becomes available
          --  for writers
          Open := Swapping or else Queues(Current_Queue)'Count > 0;
        end if;
      elsif Writer then
        -- The writer is releasing the lock
        Writer := False;
        Open := Swapping or else Queues(Current_Queue)'Count > 0;
      else
        -- Called while no lock was got
        Status := False;
      end if;
    end Mutex_Release;

    -- Two queues, one active at a time
    -- Passes when swapping queues or else when open
    entry Queues (for Queue in Queue_Range) (Kind : in Access_Kind)
    when Queue = Current_Queue
    and then (Swapping or else Open) is
    begin
      if Swapping then
        -- Swapping queueing tasks from one queue to the other
        if Queues (Queue)'Count = 0 then
          -- This is the last task: end of swapping
          -- Open remains unchanged (maybe open by a release)
          Swapping := False;
          Current_Queue := Current_Queue + 1;
        end if;
        -- Requeue the task on the other queue
        requeue Queues (Queue + 1) with abort;
      else
        -- The queue is open: The lock is either free or allocated to reader(s)
        if Kind = Read then
          -- Read lock
          Readers := Readers + 1;
          -- End the scheduling if we are the last reader
          Open := Queues (Queue)'Count /= 0;
        else
          -- Write lock:
          -- Either we get the lock (queue is closed until we release)
          -- or we queue (no currently queueing read can pass)
          -- so in both case, the queue is closed
          -- Note that a new request may re-open the queue and pass
          --  before us if it as a better prio
          Open := False;
          if Readers = 0 then
            -- We get the write lock
            Writer := True;
          else
            -- We have to wait until last reader releases the lock
            -- If we are alone, requeue ourself. Otherwise
            --  requeue in the alternate queue this task, then all the other
            --  queueing tasks, so we remain first (if same prios)
            Swapping := Queues (Queue)'Count > 0;
            if Swapping then
              requeue Queues (Queue + 1) with abort;
            else
              requeue Queues (Queue) with abort;
            end if;
          end if;
        end if;
      end if;
    end Queues;

  end Rw_Mutex_Protect;

  ----------------------------------------------------------------------------

  function Get_Mutex (A_Mutex      : Mutex;
                      Waiting_Time : Duration;
                      Kind         : Access_Kind := Read) return Boolean is
    Result : Boolean;
  begin
    if Waiting_Time < 0.0 then
      -- Negative delay : unconditional waiting
      if A_Mutex.Kind = Simple then
        A_Mutex.Mutex_Pointer.Mutex_Get;
      else
        A_Mutex.Rw_Mutex_Pointer.Mutex_Get (Kind);
      end if;
      Result := True;
    else
      if A_Mutex.Kind = Simple then
        -- Delay
        select
          A_Mutex.Mutex_Pointer.Mutex_Get;
          Result := True;
        or
          delay Waiting_Time;
          Result := False;
        end select;
      else
        select
          A_Mutex.Rw_Mutex_Pointer.Mutex_Get (Kind);
          Result := True;
        or
          delay Waiting_Time;
          Result := False;
        end select;
      end if;
    end if;
    return Result;
  end Get_Mutex;

  -- Get a mutex : infinite wait
  procedure Get_Mutex (A_Mutex      : Mutex;
                       Kind         : Access_Kind := Read) is
    Dummy : Boolean;
  begin
    Dummy := Get_Mutex (A_Mutex, -1.0, Kind);
  end Get_Mutex;


  procedure Release_Mutex (A_Mutex : in Mutex) is
    Mut_Status : Boolean;
  begin
    -- Request releasing
    if A_Mutex.Kind = Simple then
      A_Mutex.Mutex_Pointer.Mutex_Release (Mut_Status);
    else
      A_Mutex.Rw_Mutex_Pointer.Mutex_Release (Mut_Status);
    end if;
    if not Mut_Status then
      -- The mutex was already free
      raise Mutex_Already_Free;
    end if;
  end Release_Mutex;

end Mutex_Manager;

