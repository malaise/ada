package body Mutex_Manager is

  use type Ada.Task_Identification.Task_Id;

  -- The protected object which implements the simple mutex
  protected body Mutex_Protect is
    -- Gets the lock. Blocking.
    entry Mutex_Get when Free is
      Id : constant Ada.Task_Identification.Task_Id
         := Mutex_Get'Caller;
    begin
      if not Free and then Id = Owner then
        raise Already_Got;
      end if;
      Free := False;
      Owner := Id;
    end Mutex_Get;

    -- Release the lock
    procedure Mutex_Release is
    begin
      if Free
      or else Ada.Task_Identification.Current_Task /= Owner then
        raise Not_Owner;
      end if;
      Free := True;
    end Mutex_Release;

    -- Is current task owning the lock
    function Mutex_Owns return Boolean is
    begin
      return not Free
             and then Ada.Task_Identification.Current_Task = Owner;
    end Mutex_Owns;
  end Mutex_Protect;

  ----------------------------------------------------------------------------

  -- The protected object which implements the read/write mutex
  protected body Rw_Mutex_Protect is

    -- Gets the lock. Blocking.
    -- Do not let a new request be inserted in the middle of a queue while
    -- we are swapping queues.
    entry Mutex_Get (Kind : in Access_Kind) when not Swapping is
    begin
      -- Are we already the writer
      if Writer and then Mutex_Get'Caller = Owner then
        raise Already_Got;
      end if;

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
    procedure Mutex_Release is
    begin
      if Readers > 0 then
        -- There are readers, one of them is releasing the lock
        Readers := Readers - 1;
        if Readers = 0 then
          -- The last reader leaves, so the lock becomes available
          --  for writers
          Open := Swapping or else Queues(Current_Queue)'Count > 0;
        end if;
      elsif Writer
      and then Ada.Task_Identification.Current_Task = Owner then
        -- The writer is releasing the lock
        Writer := False;
        Open := Swapping or else Queues(Current_Queue)'Count > 0;
      else
        -- Called while no lock was got or not called by the writer
        raise Not_Owner;
      end if;
    end Mutex_Release;

    function Mutex_Owns return Boolean is
    begin
      return Writer
      and then Ada.Task_Identification.Current_Task = Owner;
    end Mutex_Owns;

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
            Owner := Queues'Caller;
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

  -- The write/read access lock and queues. No time.
  protected body Wr_Mutex_Protect is

    -- Gets the lock. Blocking.
    entry Mutex_Get (Kind : in Access_Kind) when True is
    begin
      -- Are we already owner of the write lock?
      if Writer and then Mutex_Get'Caller = Owner then
        raise Already_Got;
      end if;
      -- Queue properly
      if Kind = Read then
        requeue Reading_Queue with abort;
      else
        requeue Writing_Queue with abort;
      end if;
    end Mutex_Get;

    -- Releases the lock.
    procedure Mutex_Release is
    begin
      -- Default result is OK.
      if Readers > 0 then
        -- There are readers, one of them is releasing the lock
        -- No check of ownership is possible.
        Readers := Readers - 1;
      elsif Writer
      and then Ada.Task_Identification.Current_Task = Owner then
        -- The writer is releasing the lock
        Writer := False;
      else
        -- Called while no lock was got or not called by the writer
        raise Not_Owner;
      end if;
    end Mutex_Release;

    function Mutex_Owns return Boolean is
    begin
      return Writer
      and then Ada.Task_Identification.Current_Task = Owner;
    end Mutex_Owns;

    -- The queue for readers. Open when no writer waiting nor writing
    entry Reading_Queue when Writing_Queue'Count = 0 and then not Writer is
    begin
      Readers := Readers + 1;
    end Reading_Queue;

    -- The queue for writers. Open when no reader reading and no writer writing
    entry Writing_Queue when Readers = 0 and then not Writer is
    begin
      Writer := True;
      Owner := Writing_Queue'Caller;
    end Writing_Queue;

  end Wr_Mutex_Protect;

  ----------------------------------------------------------------------------

  function Get (A_Mutex      : Mutex;
                Waiting_Time : Duration;
                Kind         : Access_Kind := Read) return Boolean is
    Result : Boolean;
  begin
    if Waiting_Time < 0.0 then
      -- Negative delay : unconditional waiting
      case A_Mutex.Kind is
        when Simple =>
          A_Mutex.Mutex_Pointer.Mutex_Get;
        when Read_Write =>
          A_Mutex.Rw_Mutex_Pointer.Mutex_Get (Kind);
        when Write_Read =>
          A_Mutex.Wr_Mutex_Pointer.Mutex_Get (Kind);
      end case;
      Result := True;
    else
      case A_Mutex.Kind is
        when Simple =>
          -- Delay
          select
            A_Mutex.Mutex_Pointer.Mutex_Get;
            Result := True;
          or
            delay Waiting_Time;
            Result := False;
          end select;
        when Read_Write =>
          select
            A_Mutex.Rw_Mutex_Pointer.Mutex_Get (Kind);
            Result := True;
          or
            delay Waiting_Time;
            Result := False;
          end select;
        when Write_Read =>
          select
            A_Mutex.Wr_Mutex_Pointer.Mutex_Get (Kind);
            Result := True;
          or
            delay Waiting_Time;
            Result := False;
          end select;
        end case;
    end if;
    return Result;
  end Get;

  -- Get a mutex : infinite wait
  procedure Get (A_Mutex      : in Mutex;
                 Kind         : in Access_Kind := Read) is
    Dummy : Boolean;
  begin
    Dummy := Get (A_Mutex, -1.0, Kind);
  end Get;

  -- Release a mutex
  procedure Release (A_Mutex : in Mutex) is
  begin
    -- Request releasing
    case A_Mutex.Kind is
      when Simple =>
        A_Mutex.Mutex_Pointer.Mutex_Release;
      when Read_Write =>
        A_Mutex.Rw_Mutex_Pointer.Mutex_Release;
      when Write_Read =>
        A_Mutex.Wr_Mutex_Pointer.Mutex_Release;
    end case;
  end Release;

  -- Does current task own the mutex (for write)
  function Is_Owner (A_Mutex : Mutex) return Boolean is
  begin
    -- Request releasing
    case A_Mutex.Kind is
      when Simple =>
        return A_Mutex.Mutex_Pointer.Mutex_Owns;
      when Read_Write =>
        return A_Mutex.Rw_Mutex_Pointer.Mutex_Owns;
      when Write_Read =>
        return A_Mutex.Wr_Mutex_Pointer.Mutex_Owns;
    end case;
  end Is_Owner;

end Mutex_Manager;

