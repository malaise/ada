with Ada.Task_Identification, Ada.Finalization;
-- Mutex (single and Read_Write) management
package Mutex_Manager is

  -- Kind of mutex
  -- Simple is standard mutex providing exclusive acccess. Efficient.
  -- Read_Write allows several readers but one writer at a time.
  --  This implementation is fair but somewhat CPU consuming because
  --  the arrival of a reader while writer(s) are waiting triggers a
  --  re-evaluation to let it pass if it has a higher (task) priority.
  -- Write_Read is like Read_Write but it priviledges writer(s).
  --   No reader passes as soon as writer(s) queue, which is less fair
  --   but more efficient.
  type Mutex_Kind is (Simple, Read_Write, Write_Read);

  -- Kind of requested access for a Read_Write and Write_Read mutex
  type Access_Kind is (Read, Write);

  -- Mutex object, free at creation
  type Mutex (Kind : Mutex_Kind;
              Recursive : Boolean) is tagged limited private;
  subtype Simple_Mutex is Mutex (Simple, False);

  -- Get a mutex.
  -- Simple mutex provides exclusive access (Access_Kind is not significant).
  -- With RW mutex (Read_Write or Write_Read), simultaneous read are possible,
  --  but there is only one writer at a time and no reader at that time.
  -- If delay is negative, wait until mutex is got,
  -- If delay is null, try and give up if not free,
  -- If delay is positive, try during the specified delay.
  -- Raises Already_Got if the mutex is not Recursive and if the current task
  --  has already got the simple mutex or if it has already got the RW mutex
  --  for write.
  -- Note that there is no check of "Read then Write" deadlock.
  Already_Got : exception;
  function Get (A_Mutex      : Mutex;
                Waiting_Time : Duration;
                Kind         : Access_Kind := Read) return Boolean;
  -- Get a mutex : infinite wait.
  procedure Get (A_Mutex      : in Mutex;
                 Kind         : in Access_Kind := Read);
  function Get (A_Mutex : Mutex) return Boolean;


  -- Release a mutex.
  -- Raises Not_Owner if current task doesn't own the simple mutex
  --  or does not own the RW mutex for write (no check when releasing
  --  a RW mutex aquired for read).
  -- Fully is used to release a recusive mutex acquired several times
  Not_Owner : exception;
  procedure Release (A_Mutex : in Mutex;
                     Fully : in Boolean := False);


  -- Is current task the owner of a simple mutex
  -- Is it the writer in case of a RW mutex
  function Is_Owner (A_Mutex : in Mutex) return Boolean;

private

  -- Simple mutex
  -- The simple access lock and queue. No time.
  protected type Mutex_Protect (Recursive : Boolean) is
    entry Mutex_Get;
    procedure Mutex_Release (Full : in Boolean);
    function Mutex_Owns return Boolean;
  private
    -- Owner of the mutex
    Owner : Ada.Task_Identification.Task_Id;
    -- Number of times it has got the lock, 0 <=> free
    Count : Natural := 0;

    -- The queue for waiting
    entry Waiting_Queue;
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
  protected type Rw_Mutex_Protect (Recursive : Boolean) is

    -- Gets the lock. Blocking.
    entry Mutex_Get (Kind : in Access_Kind);

    -- Releases the lock (Full is not significant for a reader).
    procedure Mutex_Release (Full : in Boolean);

    -- Is current task write owner of the lock
    function Mutex_Owns return Boolean;
  private
    -- Number of readers
    Readers : Natural := 0;
    -- Writer identification
    Owner : Ada.Task_Identification.Task_Id;
    -- Numer of times it has got the write lock
    Writer : Natural := 0;

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
  -- Benefit: the freshest data is always available. Efficient implementation.
  -- Drawback: risk of starvation of readers if to many/long writers, whatever
  --  prio they have

  -- The write/read access lock and queues. No time.
  protected type Wr_Mutex_Protect (Recursive : Boolean) is

    -- Gets the lock. Blocking.
    entry Mutex_Get (Kind : in Access_Kind);

    -- Releases the lock. No Check of kind but the lock must have been
    -- got. (Full is not significant for a reader).
    procedure Mutex_Release (Full : in Boolean);

    -- Is current task write owner of the lock
    function Mutex_Owns return Boolean;
  private
    -- Number of readers reading
    Readers : Natural := 0;
    -- Writer identification
    Owner : Ada.Task_Identification.Task_Id;
    -- Numer of times it has got the write lock
    Writer : Natural := 0;

    -- The queues
    entry Reading_Queue;
    entry Writing_Queue;

  end Wr_Mutex_Protect;

  type Wr_Mutex_Access is access Wr_Mutex_Protect;

  --------------------------------------------------------------------------

  -- The general purpose mutex
  type Mutex (Kind : Mutex_Kind;
              Recursive : Boolean) is new Ada.Finalization.Limited_Controlled
  with record
    case Kind is
      when Simple =>
        Mutex_Pointer : Mutex_Access := new Mutex_Protect (Recursive);
      when Read_Write =>
        Rw_Mutex_Pointer : Rw_Mutex_Access := new Rw_Mutex_Protect (Recursive);
      when Write_Read =>
        Wr_Mutex_Pointer : Wr_Mutex_Access := new Wr_Mutex_Protect (Recursive);
    end case;
  end record;

  overriding procedure Finalize (M : in out Mutex);

end Mutex_Manager;

