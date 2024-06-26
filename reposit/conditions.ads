-- Conditions to synchronize several threads
with Locks;
private with Ada.Task_Identification;
package Conditions is

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
  -- Raises No_Access if current task does not own the access
  No_Access : exception;
  procedure Release (A_Condition : in Condition);

  -- Does current task have the access to the condition
  function Is_Owner (A_Condition : Condition) return Boolean;

  subtype Key_Type is Locks.Key_Type;
  Fake : Key_Type renames Locks.Fake;
  Pass : Key_Type renames Locks.Pass;

  -- Get a key that is valid for a condition
  function Get_Key (A_Condition : Condition) return Key_Type;
  -- Is Is a key valid for a condition
  function Is_Valid (A_Condition : Condition; Key : Key_Type) return Boolean;

  -- If Key is valid, then simply pass through the condition (return True)
  -- By default a Key is Fake
  -- Otherwise atomically release the access and block the calling task on the
  -- condition
  -- Upon successful return, the access to the condition is already granted
  --   again to the calling task
  -- If the Waiting_Time is reached, then Wait returns False and the current
  --   task does not have access to the condition
  -- The calling task must have already got access the the condition, otherwise
  --   No_Access is raised
  function Wait (A_Condition  : Condition;
                 Waiting_Time : Duration;
                 Key : Key_Type := Fake) return Boolean;
  procedure Wait (A_Condition : in Condition; Key : in Key_Type := Fake);


  -- Unblock one of the waiting tasks
  procedure Signal (A_Condition : in Condition);

  -- Unblock all the waiting tasks
  procedure Broadcast (A_Condition : in Condition);

private

  type Condition_State_List is (Blocked, Signaled, Broadcasted);

  -- The protected condition
  protected type Condition_Protect is
    -- Get/release mutex
    entry Get;
    procedure Release;
    function Owns return Boolean;
    function Lock return Locks.Lock;
    -- Wait for signal or broadcast
    entry Wait;
    -- Signal one waiter or broadcast all
    procedure Signal;
    procedure Broadcast;
  private
    -- The intermal queue of waiting tasks
    entry Wakeup_Queue;
    -- The mutex embedded with the condition
    Free : Boolean := True;
    -- Owner of the mutex
    Owner : Ada.Task_Identification.Task_Id;
    -- The lock for the condition
    A_Lock : Locks.Lock := Locks.Create_Closed_Lock;
    -- The condition state
    State : Condition_State_List := Blocked;
  end Condition_Protect;

  -- Access to the protected condition, so that copies share the same state
  type Condition_Access is access Condition_Protect;
  type Condition is tagged record
    Condition_Pointer : Condition_Access := new Condition_Protect;
  end record;

end Conditions;

