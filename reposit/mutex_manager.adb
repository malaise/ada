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


  -- The protected object which implements the read/write mutex
  protected body Rw_Mutex_Protect is

    -- Gets the lock. Blocking.
    entry Mutex_Get (Kind : in Access_Kind) when not Swapping is
    begin
      -- A request always re-schedules except when there is a writer writing
      -- (in this case the release will re-schedule)
      if not Writer then
        Re_Schedule := True;
      end if;
      -- Queue the request in this active queue
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
          Re_Schedule := Nb_Waiting > 0;
        end if;
      elsif Writer then
        -- The writer is releasing the lock
        Writer := False;
        Re_Schedule := Nb_Waiting > 0;
      else
        -- Called while no lock was got
        Status := False;
      end if;
    end Mutex_Release;

    -- Two queues, one active at a time
    -- Open when Re_Scedulling (and no current writer)
    --      or when swapping queues
    entry Queues (for Queue in Queue_Range) (Kind : in Access_Kind)
    when Queue = Current_Queue
    and then ((not Writer and then Re_Schedule) or else Swapping) is
    begin
      if Swapping then
        -- Swapping queueing tasks from one queue to the other
        if Queues (Queue)'Count = 0 then
          -- This is the last task: end of swapping
          Swapping := False;
          Re_Schedule := False;
          Current_Queue := Current_Queue + 1;
        end if;
        -- Requeue the task on the other queue
        requeue Queues (Queue + 1) with abort;
      else
        -- (re) Scheduling. The lock is either free or allocated to reader(s)
        if Kind = Read then
          -- Read lock
          Readers := Readers + 1;
          -- End the scheduling if we are the last reader
          if Queues (Queue)'Count = 0 then
            Re_Schedule := False;
          end if;
        else
          -- Write lock
          -- Either we get the lock or we queue
          -- in both case, this is the end of the scheduling
          Re_Schedule := False;
          if Readers = 0 then
            -- We get the write lock
            Writer := True;
          else
            -- We have to wait until last reader releases
            -- Need to swap queues in order to re-evaluate the order
            --  versus priorities (except if we are alone waiting)
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

    -- Number of tasks waiting (in both Queues and Rw_Mutex_Get)
    function Nb_Waiting return Natural is
    begin
      return Queues(0)'Count + Queues(1)'Count + Mutex_Get'Count;
    end Nb_Waiting;

  end Rw_Mutex_Protect;


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

