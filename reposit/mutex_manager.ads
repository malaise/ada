with System;

-- Mutex management
package Mutex_Manager is

  -- Mutex object, free at creation
  type Mutex is limited private;

  -- Get un mutex.
  --  If delay is negative, wait until mutex is got
  --  If delay is null, try and give up if not free
  --  If delay is positive, try during the specified delay
  function Get_Mutex (A_Mutex      : Mutex;
                      Waiting_Time : Duration) return Boolean;

  -- Release a mutex. Exception is raised if the mutex was already free.
  procedure Release_Mutex (A_Mutex : in Mutex);

  Mutex_Already_Free : exception;

private

  protected type Mut_Protect is
    entry Mut_Get;
    entry Mut_Rel(Status : out Boolean);
  private
    Free : Boolean := True;
  end Mut_Protect;

  type Mut_Access is access Mut_Protect;

  -- Create a new mutex
  type Mutex is record
    Pointer : Mut_Access := new Mut_Protect;
  end record;

end Mutex_Manager;

