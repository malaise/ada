with SYSTEM;

-- Mutex management
package MUTEX_MANAGER is

  pragma ELABORATE_BODY(MUTEX_MANAGER);
  -- Mutex object, free at creation
  type MUTEX is limited private;

  -- Get un mutex.
  --  If dealy is negative, wait until mutex is got
  --  If delay is null, try and give up if not free
  --  If delay is positive, try during the specified delay
  function GET_MUTEX (A_MUTEX      : MUTEX;
                      WAITING_TIME : DURATION) return BOOLEAN;

  -- Release a mutex. Exception is raised if the mutex was already free.
  procedure RELEASE_MUTEX (A_MUTEX : in MUTEX);

  MUTEX_ALREADY_FREE : exception;

private

  task type MUT_TASK is
    -- Prio max
    pragma PRIORITY(SYSTEM.PRIORITY'LAST);

    entry MUT_GET;
    entry MUT_REL(STATUS : out BOOLEAN);
  end MUT_TASK;

  type MUT_PTR is access MUT_TASK;

  -- Create a new mutex
  type MUTEX is record
    POINTER : MUT_PTR := new MUT_TASK;
  end record;

end MUTEX_MANAGER;

