with Sys_Calls, Upper_Str;
package body Mutexes is

  use type Ada.Task_Identification.Task_Id;
  function Image (Id : in Ada.Task_Identification.Task_Id) return String
           renames Ada.Task_Identification.Image;

  Debug_Set : Boolean := False;
  Debug_On : Boolean := False;
  procedure Trace (Id : in Ada.Task_Identification.Task_Id;
                   Msg : in String) is
  begin
    if not Debug_Set then
      -- Initial setup
      Debug_Set := True;
      declare
        Str : constant String := Upper_Str (Sys_Calls.Getenv ("TRACE_Mutexes"));
      begin
        Debug_On := Str = "Y" or else Str = "YES";
      end;
    end if;
    if Debug_On then
      Sys_Calls.Put_Line_Output (Msg & " " & Image (Id));
    end if;
  exception
    when Sys_Calls.Env_Not_Set =>
      null;
  end Trace;

  -- The protected object which implements the simple mutex
  protected body Si_Mutex_Protect is
    -- Gets the lock. Blocking.
    entry Mutex_Get when True is
    begin
      if Count = 0
      or else Mutex_Get'Caller = Owner then
        -- Mutex is free or already allocated to me
        Count := Count + 1;
        Owner := Mutex_Get'Caller;
      else
        Trace (Mutex_Get'Caller, "Simple get queueing");
        requeue Waiting_Queue with abort;
      end if;
    end Mutex_Get;

    -- Release the lock
    procedure Mutex_Release (Full : in Boolean) is
    begin
      if Count > 0 and then Ada.Task_Identification.Current_Task = Owner then
        -- Mutex allocated to me
        if Full then
          Count := 0;
        else
          Trace (Ada.Task_Identification.Current_Task, "Owner releases");
          Count := Count - 1;
        end if;
      else
        raise Not_Owner;
      end if;
    end Mutex_Release;

    -- Is current task owning the lock
    function Mutex_Owns return Boolean is
      (Count > 0 and then Ada.Task_Identification.Current_Task = Owner);

    -- The queue
    entry Waiting_Queue when Count = 0 is
    begin
      Trace (Waiting_Queue'Caller, "Got mutex");
      Count := 1;
      Owner := Waiting_Queue'Caller;
    end Waiting_Queue;

  end Si_Mutex_Protect;

  ----------------------------------------------------------------------------

  -- The protected object which implements the read/write mutex
  protected body Rw_Mutex_Protect is

    -- Gets the lock. Blocking.
    -- Do not let a new request be inserted in the middle of a queue while
    -- we are swapping queues.
    entry Mutex_Get (Kind : in Access_Kind) when not Swapping is
    begin
      -- Are we already the writer
      if Writer /= 0 and then Mutex_Get'Caller = Owner then
        if Recursive then
          Writer := Writer + 1;
          return;
        else
          raise Already_Got;
        end if;
      end if;

      -- Queue the request in the active queue
      Trace (Mutex_Get'Caller, "Read_Write get queueing");
      requeue Queues(Current_Queue) with abort;
    end Mutex_Get;

    -- Releases the lock. No Check of kind but the lock must have been
    -- got.
    procedure Mutex_Release (Full : in Boolean) is
    begin
      if Readers > 0 then
        -- There are readers, one of them is releasing the lock
        Readers := Readers - 1;
        if Readers = 0 then
          -- The last reader leaves, so the lock becomes available
          --  for writers
          Trace (Ada.Task_Identification.Current_Task, "Last reader releases");
          Open := True;
        else
          Trace (Ada.Task_Identification.Current_Task, "Reader releases");
        end if;
      elsif Writer /= 0
      and then Ada.Task_Identification.Current_Task = Owner then
        -- The writer is releasing the lock
        if Writer /= 1 and then not Full then
          Writer := Writer - 1;
          return;
        end if;
        -- Really releasing
        Trace (Ada.Task_Identification.Current_Task, "Writer releases");
        Writer := 0;
        Open := True;
      else
        -- Called while no lock was got or not called by the writer
        raise Not_Owner;
      end if;
    end Mutex_Release;

    function Mutex_Owns return Boolean is
      (Writer /= 0 and then Ada.Task_Identification.Current_Task = Owner);

    -- Two queues, one active at a time
    -- Passes when swapping queues or else when open
    entry Queues (for Queue in Queue_Range) (Kind : in Access_Kind)
          when Queue = Current_Queue
          and then (Swapping or else Open) is
    begin
      if Swapping then
        -- Swapping queueing tasks from one queue to the other
        Trace (Queues'Caller, "Swapping");
        if Queues(Queue)'Count = 0 then
          -- This is the last task: end of swapping
          -- Open remains unchanged (maybe open by a release)
          Swapping := False;
          Current_Queue := Current_Queue + 1;
          Trace (Queues'Caller, "End swapping");
        end if;
        -- Requeue the task on the other queue
        requeue Queues(Queue + 1) with abort;
      else
        -- The queue is open: The lock is either free or allocated to reader(s)
        if Kind = Read then
          Trace (Queues'Caller, "Another reader");
          -- Read lock
          Readers := Readers + 1;
        else
          -- Write lock:
          -- If we are here, it means that the gate is open so no writer
          --  has already got the lock
          -- Either we get the lock (queue is closed until we release)
          -- or we queue (no currently queueing read can pass)
          -- so in both case, the queue is closed
          -- Note that a new request may re-open the queue and enter
          --  before us if it as a better prio
          Open := False;
          if Readers = 0 then
            -- No reader => we get the write lock
            Writer := 1;
            Owner := Queues'Caller;
            Trace (Queues'Caller, "Writer has got lock");
          else
            -- We have to wait until last reader releases the lock
            -- If we are alone, requeue ourself. Otherwise
            --  requeue in the alternate queue this task, then all the other
            --  queueing tasks, so we remain first (if same prios)
            Swapping := Queues(Queue)'Count > 0;
            if Swapping then
              Trace (Queues'Caller, "Start swapping");
              requeue Queues(Queue + 1) with abort;
            else
              Trace (Queues'Caller, "Writer waits for current reader");
              requeue Queues(Queue) with abort;
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
      if Writer /= 0 and then Mutex_Get'Caller = Owner then
        if Recursive then
          Writer := Writer + 1;
          return;
        else
          raise Already_Got;
        end if;
      end if;
      -- Queue properly
      if Kind = Read then
        Trace (Mutex_Get'Caller, "Write_Read get queueing read");
        requeue Reading_Queue with abort;
      else
        Trace (Mutex_Get'Caller, "Write_Read get queueing write");
        requeue Writing_Queue with abort;
      end if;
    end Mutex_Get;

    -- Releases the lock.
    procedure Mutex_Release (Full : in Boolean) is
    begin
      -- Default result is OK.
      if Readers > 0 then
        -- There are readers, one of them is releasing the lock
        -- No check of ownership is possible.
        Trace (Ada.Task_Identification.Current_Task, "Reader releases");
        Readers := Readers - 1;
      elsif Writer /= 0
      and then Ada.Task_Identification.Current_Task = Owner then
        -- The writer is releasing the lock
        if Writer /= 1 and then not Full then
          Trace (Ada.Task_Identification.Current_Task, "Writer releases");
          Writer := Writer - 1;
          return;
        end if;
        Writer := 0;
      else
        -- Called while no lock was got or not called by the writer
        raise Not_Owner;
      end if;
    end Mutex_Release;

    function Mutex_Owns return Boolean is
      (Writer /= 0 and then Ada.Task_Identification.Current_Task = Owner);

    -- The queue for readers. Open when no writer waiting nor writing
    entry Reading_Queue when Writing_Queue'Count = 0 and then Writer = 0 is
    begin
      Trace (Reading_Queue'Caller, "Got read mutex");
      Readers := Readers + 1;
    end Reading_Queue;

    -- The queue for writers. Open when no reader reading and no writer writing
    entry Writing_Queue when Readers = 0 and then Writer = 0 is
    begin
      Trace (Writing_Queue'Caller, "Got write mutex");
      Writer := 1;
      Owner := Writing_Queue'Caller;
    end Writing_Queue;

  end Wr_Mutex_Protect;

  ----------------------------------------------------------------------------

  function Get (A_Mutex      : in out Mutex;
                Waiting_Time : in Duration := Infinite;
                Kind         : in Access_Kind := Read) return Boolean is
    Result : Boolean;
  begin
    if Waiting_Time < 0.0 then
      -- Negative delay : unconditional waiting
      case A_Mutex.Kind is
        when Simple =>
          A_Mutex.Si_Mutex.Mutex_Get;
        when Read_Write =>
          A_Mutex.Rw_Mutex.Mutex_Get (Kind);
        when Write_Read =>
          A_Mutex.Wr_Mutex.Mutex_Get (Kind);
      end case;
      Result := True;
    else
      case A_Mutex.Kind is
        when Simple =>
          -- Delay
          select
            A_Mutex.Si_Mutex.Mutex_Get;
            Result := True;
          or
            delay Waiting_Time;
            Result := False;
          end select;
        when Read_Write =>
          select
            A_Mutex.Rw_Mutex.Mutex_Get (Kind);
            Result := True;
          or
            delay Waiting_Time;
            Result := False;
          end select;
        when Write_Read =>
          select
            A_Mutex.Wr_Mutex.Mutex_Get (Kind);
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
  procedure Get (A_Mutex      : in out Mutex;
                 Kind         : in Access_Kind := Read) is
    Dummy : Boolean;
  begin
    Dummy := Get (A_Mutex, Infinite, Kind);
  end Get;

  -- Release a mutex
  procedure Release (A_Mutex : in out Mutex; Fully : in Boolean := False) is
  begin
    -- Request releasing
    case A_Mutex.Kind is
      when Simple =>
        A_Mutex.Si_Mutex.Mutex_Release (Fully);
      when Read_Write =>
        A_Mutex.Rw_Mutex.Mutex_Release (Fully);
      when Write_Read =>
        A_Mutex.Wr_Mutex.Mutex_Release (Fully);
    end case;
  end Release;

  -- Does current task own the mutex (for write)
  function Is_Owner (A_Mutex : Mutex) return Boolean is
    -- Request releasing
    (case A_Mutex.Kind is
       when Simple     => A_Mutex.Si_Mutex.Mutex_Owns,
       when Read_Write => A_Mutex.Rw_Mutex.Mutex_Owns,
       when Write_Read => A_Mutex.Wr_Mutex.Mutex_Owns);

end Mutexes;

