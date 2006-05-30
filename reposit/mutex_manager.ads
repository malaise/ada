-- Mutex (single and Read_Write) management
package Mutex_Manager is

  -- When releasing a (simple or read/write) mutex that is already free
  Mutex_Already_Free : exception;

  --------------------------------------------------------------------------

  -- Mutex object, free at creation
  type Mutex is private;

  -- Get un mutex.
  --  If delay is negative, wait until mutex is got
  --  If delay is null, try and give up if not free
  --  If delay is positive, try during the specified delay
  function Get_Mutex (A_Mutex      : Mutex;
                      Waiting_Time : Duration) return Boolean;

  -- Release a mutex.
  -- Exception Mutex_Already_Free is raised if the mutex was already free.
  procedure Release_Mutex (A_Mutex : in Mutex);

  --------------------------------------------------------------------------

  -- A lock object, free at creation
  type Rw_Mutex is private;

  -- Kind of requested access
  type Access_Kind is (Read, Write);

  -- Get a read/write mutex.
  -- Simultaneous read are possible, but write is alone.
  --  If delay is negative, wait until lock is got (and return True)
  --  If delay is null, try and give up if not free
  --  If delay is positive, try during the specified delay
  function Get_Rw_Mutex (A_Rw_Mutex       : Rw_Mutex;
                         Kind         : Access_Kind;
                         Waiting_Time : Duration) return Boolean;

  -- Release a read/write mutex.
  -- Exception Mutex_Already_Free is raised if the mutex was already free.
  procedure Release_Rw_Mutex (A_Rw_Mutex : in Rw_Mutex);

private

  -- The simple access lock and queue. No time.
  protected type Mut_Protect is
    entry Mut_Get;
    procedure Mut_Rel (Status : out Boolean);
  private
    Free : Boolean := True;
  end Mut_Protect;

  type Mut_Access is access Mut_Protect;

  -- Create a new mutex
  type Mutex is record
    Pointer : Mut_Access := new Mut_Protect;
  end record;


  --------------------------------------------------------------------------

  -- There will be two queues
  type Queue_Range is mod 2;

  -- The read/write access lock and queues. No time.
  protected type Rw_Mutex_Protect is

    -- Gets the lock. Blocking.
    entry Rw_Mutex_Get (Kind : in Access_Kind);

    -- Releases the lock. No Check of kind but the lock must have been
    -- got.
    procedure Rw_Mutex_Rel (Status : out Boolean);

  private
    -- Number of readers
    Readers : Natural := 0;
    -- Is there a writer
    Writer  : Boolean := False;
    -- Re-schedule the queueing
    Re_Schedule  : Boolean := False;

    -- Two queues, one active at a time
    entry Queues (Queue_Range) (Kind : in Access_Kind);
    Current_Queue : Queue_Range := Queue_Range'First;

    -- True while swapping the tasks from one queue to the other
    Swapping : Boolean := False;

    -- Number of tasks waiting (in both Queues and both Queues)
    function Nb_Waiting return Natural;

  end Rw_Mutex_Protect;


  type Rw_Mutex_Access is access Rw_Mutex_Protect;

  -- Create a new lock
  type Rw_Mutex is record
    Pointer : Rw_Mutex_Access := new Rw_Mutex_Protect;
  end record;

end Mutex_Manager;

