with Ada.Task_Identification, Ada.Finalization;
-- Mutex that can bee obtained several times by the same task
package Recursive_Mutex is

  -- Mutex object, free at creation
  type Mutex is tagged limited private;

  -- Get un mutex.
  -- The same task cat obteain the mutex several times
  -- If delay is negative, wait until mutex is got,
  -- If delay is null, try and give up if not free,
  -- If delay is positive, try during the specified delay.
  function Get (A_Mutex      : Mutex;
                Waiting_Time : Duration) return Boolean;
  -- Get a mutex : infinite wait.
  procedure Get (A_Mutex      : in Mutex);

  -- Release a mutex.
  -- The task shall release the mutex as many times it has got it.
  Not_Owner : exception;
  procedure Release (A_Mutex : in Mutex);

  -- Release completely the mutex
  procedure Release_Full (A_Mutex : in Mutex);

  -- Is current task the owner of the mutex
  function Is_Owner (A_Mutex : in Mutex) return Boolean;

private

  protected type Mutex_Protect is

    -- Gets the lock. Blocking.
    entry Mutex_Get;

    -- Releases the lock.
    procedure Mutex_Release (Full : in Boolean);

    -- Is current task write owner of the lock
    function Mutex_Owns return Boolean;
  private
    -- Owner identification
    Owner : Ada.Task_Identification.Task_Id;
    -- Numer of times it has got the write lock
    Count : Natural := 0;

    -- The queue
    entry Queue;

  end Mutex_Protect;
  type Mutex_Access is access Mutex_Protect;

  -- The general purpose mutex
  type Mutex is new Ada.Finalization.Limited_Controlled with record
    Mutex_Pointer : Mutex_Access := new Mutex_Protect;
  end record;

  overriding procedure Finalize (A_Mutex : in out Mutex);

end Recursive_Mutex;

