with Unchecked_Deallocation;
package body Recursive_Mutex is

  use type Ada.Task_Identification.Task_Id;

  -- Get un mutex.
  -- The same task cat obteain the mutex several times
  -- If delay is negative, wait until mutex is got,
  -- If delay is null, try and give up if not free,
  -- If delay is positive, try during the specified delay.
  function Get (A_Mutex      : Mutex;
                Waiting_Time : Duration) return Boolean is
    Result : Boolean;
  begin
    if Waiting_Time < 0.0 then
      -- Negative delay : unconditional waiting
      A_Mutex.Mutex_Pointer.Mutex_Get;
      Result := True;
    else
      -- Delay
      select
        A_Mutex.Mutex_Pointer.Mutex_Get;
        Result := True;
      or
        delay Waiting_Time;
        Result := False;
      end select;
    end if;
    return Result;
  end Get;

  -- Get a mutex : infinite wait.
  procedure Get (A_Mutex : in Mutex) is
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    Dummy := Get (A_Mutex, -1.0);
  end Get;

  -- Release a mutex.
  -- The task shall release the mutex as many times it has got it.
  -- Not_Owner : exception;
  procedure Release (A_Mutex : in Mutex) is
  begin
    A_Mutex.Mutex_Pointer.Mutex_Release (Full => False);
    null;
  end Release;

  -- Release completely the mutex
  procedure Release_Full (A_Mutex : in Mutex) is
  begin
    A_Mutex.Mutex_Pointer.Mutex_Release (Full => True);
  end Release_Full;

  -- Is current task the owner of the mutex
  function Is_Owner (A_Mutex : in Mutex) return Boolean is
  begin
    return A_Mutex.Mutex_Pointer.Mutex_Owns;
  end Is_Owner;


  protected body Mutex_Protect is

    -- Gets the lock. Blocking.
    entry Mutex_Get when True is
    begin
      if Count = 0
      or else Mutex_Get'Caller = Owner then
        -- Mutex is free or already allocated to me
        Count := Count + 1;
        Owner := Mutex_Get'Caller;
      else
        requeue Queue with abort;
      end if;
    end Mutex_Get;

    -- Releases the lock.
    procedure Mutex_Release (Full : in Boolean) is
    begin
      if Count > 0 and then Ada.Task_Identification.Current_Task = Owner then
        -- Mutex allocated to me
        if Full then
          Count := 0;
        else
          Count := Count - 1;
        end if;
      else
        raise Not_Owner;
      end if;
    end Mutex_Release;

    -- Is current task write owner of the lock
    function Mutex_Owns return Boolean is
    begin
      return Count > 0
             and then Ada.Task_Identification.Current_Task = Owner;
    end Mutex_Owns;

    -- The queue
    entry Queue when Count = 0 is
    begin
      Count := 1;
      Owner := Queue'Caller;
    end Queue;

  end Mutex_Protect;


  overriding procedure Finalize (A_Mutex : in out Mutex) is
    procedure Free is new
       Unchecked_Deallocation(Object=>Mutex_Protect, Name=>Mutex_Access);
  begin
    Free (A_Mutex.Mutex_Pointer);
  end Finalize;

end Recursive_Mutex;

