with Ada.Task_Identification;
-- Mutex (single and Read_Write) management
package Mutex_Manager is

  -- Kind of mutex
  -- Simple is standard mutex providing exclusive acccess. Efficient.
  -- Read_Write allows several readers but one writer at a time.
  --  this implementation is fair but somewhat CPU consuming because
  --  the arival of a reader while writer(s) wait triggers a re-evaluation
  --  to let it pass if it has a higher priority.
  -- Write_Read is like Read_Write but it priviledges writer(s).
  --   No reader passes as soon as writer(s) queue.
  type Mutex_Kind is (Simple, Read_Write, Write_Read);

  -- Kind of requested access for a Read_Write mutex
  type Access_Kind is (Read, Write);

  -- Mutex object, simple by default, free at creation
  type Mutex (Kind : Mutex_Kind := Simple) is private;

  -- Get un mutex.
  -- Simple mutex provides exclusive access (Access_Kind is not significant).
  -- With Read_Write mutex, simultaneous read are possible, but writer is alone.
  -- If delay is negative, wait until mutex is got
  -- If delay is null, try and give up if not free
  -- If delay is positive, try during the specified delay
  -- Raises Already_Got of current task has already got the simple mutex
  --  or if it has already got a RW mutex for write
  -- Note that there is no check of "Read then Write" deadlock
  Already_Got : exception;
  function Get (A_Mutex      : Mutex;
                Waiting_Time : Duration;
                Kind         : Access_Kind := Read) return Boolean;
  -- Get a mutex : infinite wait
  procedure Get (A_Mutex      : in Mutex;
                 Kind         : in Access_Kind := Read);


  -- Release a mutex.
  -- Raises Not_Owner if current task doesn't own the simple mutex
  --  or does not own a RW mutex for write
  Not_Owner : exception;
  procedure Release (A_Mutex : in Mutex);


  -- Is current task the owner of a simple mutex
  -- Is it the writer in case of a RW/WR mutex
  function Is_Owner (A_Mutex : in Mutex) return Boolean;

private

  -- Simple mutex
  -- The simple access lock and queue. No time.
  protected type Mutex_Protect is
    entry Mutex_Get;
    procedure Mutex_Release;
    function Mutex_Owns return Boolean;
  private
    Free : Boolean := True;
    Owner : Ada.Task_Identification.Task_Id;
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
  -- Benefit: a new reader may enter while writer(s) are queueing, if its
  --  priority is better. Fairness ++.
  -- Drawback: each new reader while writer(s) are queueing triggers an open
  --  then a swap of the queues. Perfo --.

  -- So there are two queues
  type Queue_Range is mod 2;

  -- The read/write access lock and queues. No time.
  protected type Rw_Mutex_Protect is

    -- Gets the lock. Blocking.
    entry Mutex_Get (Kind : in Access_Kind);

    -- Releases the lock
    procedure Mutex_Release;

    -- Is current task write owner of the lock
    function Mutex_Owns return Boolean;
  private
    -- Number of readers
    Readers : Natural := 0;
    -- Is there a writer
    Writer  : Boolean := False;
    -- Writer identification
    Owner : Ada.Task_Identification.Task_Id;

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

  -- Write/Read mutex
  -- Writers always have priority on pending readers, so we have a readers and
  --  a writers queue
  -- Benefit: the freshest data is amways available. Efficient implementation.
  -- Drawback: risk of starvation of readers if to many/long writers whatever
  --  prio they have

  -- The write/read access lock and queues. No time.
  protected type Wr_Mutex_Protect is

    -- Gets the lock. Blocking.
    entry Mutex_Get (Kind : in Access_Kind);

    -- Releases the lock. No Check of kind but the lock must have been
    -- got.
    procedure Mutex_Release;

    -- Is current task write owner of the lock
    function Mutex_Owns return Boolean;
  private
    -- Number of readers reading
    Readers : Natural := 0;
    -- Is there a writer writing
    Writer  : Boolean := False;
    -- Writer identification
    Owner : Ada.Task_Identification.Task_Id;

    -- The queues
    entry Reading_Queue;
    entry Writing_Queue;

  end Wr_Mutex_Protect;

  type Wr_Mutex_Access is access Wr_Mutex_Protect;

  --------------------------------------------------------------------------

  -- Create a new lock
  type Mutex (Kind : Mutex_Kind := Simple) is record
    case Kind is
      when Simple =>
        Mutex_Pointer : Mutex_Access := new Mutex_Protect;
      when Read_Write =>
        Rw_Mutex_Pointer : Rw_Mutex_Access := new Rw_Mutex_Protect;
      when Write_Read =>
        Wr_Mutex_Pointer : Wr_Mutex_Access := new Wr_Mutex_Protect;
    end case;
  end record;

end Mutex_Manager;

