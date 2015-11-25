with Condition_Manager;
package Door_Manager is

  -- A door is a waiting point on which one or several tasks may
  --  wait until the required number of waiters is reached.
  -- Door is open at creation (Nb_Waiters = 1)
  -- Door can be completely closed (Nb_Waiters = 0)
  type Door is tagged private;


  -- Get access to the door (prior getting or setting its number of waiters,
  --  or prior waiting)
  -- If delay is negative, wait until mutex is got
  -- If delay is null, try and give up if not free
  -- If delay is positive, try during the specified delay
  -- Raises Already_Got if current task already owns the access
  Already_Got : exception renames Condition_Manager.Already_Got;
  function Get (A_Door : Door;
                Waiting_Time : Duration) return Boolean;
  -- Get access to the door : infinite wait
  procedure Get (A_Door : in Door);

  -- Release access to the door
  -- Raises Not_Owner if current task does not own the access
  Not_Owner : exception renames Condition_Manager.Not_Owner;
  procedure Release (A_Door : in Door);



  -- All the following operations require the caller to own the access
  -- to the door, otherwise they raise No_Access
  No_Access : exception;

  -- Get/Set the number of waiters (door access should have been granted)
  -- Set the expected number of waiters
  -- 0 Means that the door is closed, (until the number of waiters is set back
  --  to a positive value)
  Open   : constant Natural := 1;
  Closed : constant Natural := 0;
  procedure Set_Nb_Waiters (A_Door : in Door;
                            To : in Natural);

  -- Get the expected number of waiters
  function Get_Nb_Waiters (A_Door : Door) return Natural;

  -- Increment/decrement the expected number of waiters
  --  (to at most Natural'Last)
  -- Raises Constraint_Error (after releasing) if Nb_Waiters + Val < 0
  procedure Add_To_Nb_Waiters (A_Door : in Door;
                               Val : in Integer);


  -- If Key is Pass, then simply pass through the door (return True)
  type Key_Type is private;
  Fake, Pass : constant Key_Type;
  -- Otherwise *atomically* release access and block the calling task, until
  --   the required number of waiters is reached (new waiters arriving or
  ---  someone reducing the Nb_Waiters)
  -- Upon successful return, the access to the door is already granted to the
  --  calling task
  function Wait (A_Door : Door;
                 Waiting_Time : Duration;
                 Key : Key_Type := Fake) return Boolean;
  procedure Wait (A_Door : in Door; Key : in Key_Type := Fake);

private

  -- Key
  type Key_Type is new Boolean;
  Fake : constant Key_Type := False;
  Pass : constant Key_Type := True;

  type Door_Rec is record
    -- The expected number of waiters
    Expected : Natural := 1;
    -- The current number of waiters
    Current : Natural := 0;
    -- The condition on which waiters are waiting
    Cond : Condition_Manager.Condition;
  end record;

  -- Access to the Rec, so that copies share the same status and condition
  type Door_Access is access Door_Rec;
  type Door is tagged record
    Door_Pointer : Door_Access := new Door_Rec;
  end record;

end Door_Manager;

