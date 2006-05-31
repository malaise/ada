-- Mutex (single and Read_Write) management
package Mutex_Manager is

  -- Kind of mutex
  type Mutex_Kind is (Simple, Read_Write);

  -- Kind of requested access for a Read_Write mutex
  type Access_Kind is (Read, Write);

  -- Mutex object, simple by default, free at creation
  type Mutex (Kind : Mutex_Kind := Simple) is private;

  -- Get un mutex.
  -- Simple mutex provides exclusive access (Access_Kind is not significant).
  -- With Read_Write mutex, simultaneous read are possible, but writer is alone.
  --  If delay is negative, wait until mutex is got
  --  If delay is null, try and give up if not free
  --  If delay is positive, try during the specified delay
  function Get_Mutex (A_Mutex      : Mutex;
                      Waiting_Time : Duration;
                      Kind         : Access_Kind := Read) return Boolean;
  -- Get a mutex : infinite wait
  procedure Get_Mutex (A_Mutex      : Mutex;
                       Kind         : Access_Kind := Read);

  -- When releasing a (simple or read/write) mutex that is already free
  Mutex_Already_Free : exception;

  -- Release a mutex.
  -- Exception Mutex_Already_Free is raised if the mutex was already free.
  procedure Release_Mutex (A_Mutex : in Mutex);

private

  -- Simple mutex
  -- The simple access lock and queue. No time.
  protected type Mutex_Protect is
    entry Mutex_Get;
    procedure Mutex_Release (Status : out Boolean);
  private
    Free : Boolean := True;
  end Mutex_Protect;

  type Mutex_Access is access Mutex_Protect;

  --------------------------------------------------------------------------

  -- Read/write mutex
  -- We want to queue all requests (read and write) within the same queue
  --  to ensure fairness (avoid starvation).
  -- But the condition for read is "no writer" (not Writer)
  --  and for write it is "no writer and no reader"
  --  (not Writer and then Readers = 0)
  -- So the guard of the common queue is "no writer", but a writer may pass it
  --  while there are reader. In this case, we would like to requeue it in the
  --  same queue and in the beginning of it; note that "requeue" requeues at
  --  the end :-(. The artifact is to requeue the task, and all other
  --  tasks queueing after it as well, in an alternate queue (Swapping).
  -- So there are two queues
  type Queue_Range is mod 2;

  -- The read/write access lock and queues. No time.
  protected type Rw_Mutex_Protect is

    -- Gets the lock. Blocking.
    entry Mutex_Get (Kind : in Access_Kind);

    -- Releases the lock. No Check of kind but the lock must have been
    -- got.
    procedure Mutex_Release (Status : out Boolean);

  private
    -- Number of readers
    Readers : Natural := 0;
    -- Is there a writer
    Writer  : Boolean := False;

    -- Two queues, one is active at a time
    entry Queues (Queue_Range) (Kind : in Access_Kind);
    Current_Queue : Queue_Range := Queue_Range'First;
    -- The status of the queue:
    -- Swapping or not
    Swapping : Boolean := False;
    -- If not zwapping, open or not
    -- if swapping, will be open or not
    Open : Boolean := False;

  end Rw_Mutex_Protect;

  type Rw_Mutex_Access is access Rw_Mutex_Protect;

  --------------------------------------------------------------------------

  -- Create a new lock
  type Mutex (Kind : Mutex_Kind := Simple) is record
    case Kind is
      when Simple =>
        Mutex_Pointer : Mutex_Access := new Mutex_Protect;
      when Read_Write =>
        Rw_Mutex_Pointer : Rw_Mutex_Access := new Rw_Mutex_Protect;
    end case;
  end record;

end Mutex_Manager;

