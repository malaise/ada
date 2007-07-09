with Condition_Manager;
package Door_Manager is

  -- Door is open by default (Nb_Waiters = 1)
  type Door is private;


  -- Get access to the door (prior getting or setting its number of waiters)
  -- If delay is negative, wait until mutex is got
  -- If delay is null, try and give up if not free
  -- If delay is positive, try during the specified delay
  function Get (A_Door : Door;
                Waiting_Time : Duration) return Boolean;
  -- Get access to the condition : infinite wait
  procedure Get (A_Door : in Door);

  Door_Is_Free : exception;

  -- Exception Door_Is_Free is raised if the door was already
  --  free.
  procedure Release (A_Door : in Door);

  -- Get/Set the number of waiters (door access should have been granted)
  -- Set the expected number of waiters
  procedure Set_Nb_Waiters (A_Door : in Door;
                            To : in Positive);

  -- Get the expected number of waiters
  function Get_Nb_Waiters (A_Door : Door) return Positive;

  -- Increment/decrement the expected number of waiters
  -- (The expected number of waiters remains always Positive)
  procedure Add_To_Nb_Waiters (A_Door : in Door;
                               Val : in Integer);


  -- Wait until the required number of waiters is reached
  --  (door access should have been granted)
  -- Upon successful return, the access to the door is already granted to the
  --  calling task
  function Wait (A_Door : in Door;
                 Waiting_Time : Duration) return Boolean;
  procedure Wait (A_Door : in Door);

private

  type Door_Rec is record
    -- The expected number of waiters
    Expected : Positive := 1;
    -- The current number of waiters
    Current : Natural := 0;
    -- the waiting condition
    Cond : Condition_Manager.Condition;
  end record;

  type Door_Access is access Door_Rec;

  type Door is record
    Door_Pointer : Door_Access := new Door_Rec;
  end record;

end Door_Manager;

