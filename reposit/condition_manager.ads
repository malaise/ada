package Condition_Manager is

  type Condition is private;

  -- Get access to the condition
  -- If delay is negative, wait until mutex is got
  -- If delay is null, try and give up if not free
  -- If delay is positive, try during the specified delay
  function Get (A_Condition : Condition;
                Waiting_Time : Duration) return Boolean;
  -- Get access to the condition : infinite wait
  procedure Get (A_Condition : in Condition);

  Condition_Is_Free : exception;

  -- Exception Condition_Is_Free is raised if the condition was already
  --  free.
  procedure Release (A_Condition : in Condition);

  -- The calling task must own the condition's mutex
  -- Atomically release the mutex and block the calling task on the condition
  -- Upon successful return, the access to the condition is already granted to
  --  the calling task
  function Wait (A_Condition  : Condition;
                 Waiting_Time : Duration) return Boolean;
  procedure Wait (A_Condition  : in Condition);

  -- Unblock at least one of the blocked tasks
  procedure Signal (A_Condition  : in Condition);

  -- Unblock all the blocked tasks
  procedure Broadcast (A_Condition  : in Condition);

private

  type Condition_State_List is (Blocked, Signaled, Broadcasted);

  protected type Condition_Protect is
    -- Get/release mutex
    entry Get;
    procedure Release;
    -- Wait for signal or broadcast
    entry Wait;
    -- Signal  one waiter or broadcast all
    procedure Signal;
    procedure Broadcast;
  private
    -- The intermal queue of waiting tasks
    entry Wakeup_Queue;
    -- The mutex embeeded with the condition
    Free : Boolean := True;
    -- The condition state
    State : Condition_State_List := Blocked;
  end Condition_Protect;

  type Condition_Access is access Condition_Protect;

  type Condition is record
    Condition_Pointer : Condition_Access := new Condition_Protect;
  end record;

end Condition_Manager;

