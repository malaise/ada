with System;

-- Mutex management
package Mutex_Manager is

  pragma Elaborate_Body(Mutex_Manager);
  -- Mutex object, free at creation
  type Mutex is limited private;

  -- Get un mutex.
  --  If dealy is negative, wait until mutex is got
  --  If delay is null, try and give up if not free
  --  If delay is positive, try during the specified delay
  function Get_Mutex (A_Mutex      : Mutex;
                      Waiting_Time : Duration) return Boolean;

  -- Release a mutex. Exception is raised if the mutex was already free.
  procedure Release_Mutex (A_Mutex : in Mutex);

  Mutex_Already_Free : exception;

private

  task type Mut_Task is
    -- Prio max
    pragma Priority(System.Priority'Last);

    entry Mut_Get;
    entry Mut_Rel(Status : out Boolean);
  end Mut_Task;

  type Mut_Ptr is access Mut_Task;

  -- Create a new mutex
  type Mutex is record
    Pointer : Mut_Ptr := new Mut_Task;
  end record;

end Mutex_Manager;

