with Ada.Task_Identification;
package Condition_Manager is

  -- A condition is a waiting point on which one or several tasks may
  --  wait until un-blocked, either one by one (signal) or all together
  --  (broadcast).
  type Condition is tagged private;

  -- Before waiting on a condition the user must get access to it
  -- By starting to wait the application automatically releases the exclusive
  --  access to the condition


  -- Get access to the condition
  -- If delay is negative, wait until mutex is got
  -- If delay is null, try and give up if not free
  -- If delay is positive, try during the specified delay
  -- Raises Already_Got if current task already owns the access
  Already_Got : exception;
  function Get (A_Condition : Condition;
                Waiting_Time : Duration) return Boolean;
  -- Get access to the condition : infinite wait
  procedure Get (A_Condition : in Condition);

  -- Release access to the condition
  -- Raises Not_Owner if current task does not own the access
  Not_Owner : exception;
  procedure Release (A_Condition : in Condition);

  -- Does current task have the access to the condition
  function Is_Owner (A_Condition : Condition) return Boolean;


  -- Atomically release the mutex and block the calling task on the condition
  -- Upon successful return, the access to the condition is already granted
  --  again to the calling task
  -- The calling task must have already got access the the condition, otherwise
  --   No_Access is raised
  No_Access : exception;
  function Wait (A_Condition  : Condition;
                 Waiting_Time : Duration) return Boolean;
  procedure Wait (A_Condition  : in Condition);


  -- Unblock one of the waiting tasks
  procedure Signal (A_Condition  : in Condition);

  -- Unblock all the waiting tasks
  procedure Broadcast (A_Condition  : in Condition);

private

  type Condition_State_List is (Blocked, Signaled, Broadcasted);

  protected type Condition_Protect is
    -- Get/release mutex
    entry Get;
    procedure Release;
    function Owns return Boolean;
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
    -- Owner of the mutex
    Owner : Ada.Task_Identification.Task_Id;
    -- The condition state
    State : Condition_State_List := Blocked;
  end Condition_Protect;

  type Condition_Access is access Condition_Protect;

  type Condition is tagged record
    Condition_Pointer : Condition_Access := new Condition_Protect;
  end record;

end Condition_Manager;

