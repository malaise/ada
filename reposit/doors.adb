package body Doors is

  -- Access to condition --
  -- Get access to the condition
  function Get (A_Door : Door;
                Waiting_Time : Duration) return Boolean is
    (A_Door.Door_Pointer.Cond.Get (Waiting_Time));

  -- Get access to the condition : infinite wait
  procedure Get (A_Door : in Door) is
  begin
    A_Door.Door_Pointer.Cond.Get;
  end Get;

  -- Release access to the condition
  procedure Release (A_Door : in Door) is
  begin
    A_Door.Door_Pointer.Cond.Release;
  end Release;

  -- Does current task have the access to the door
  function Is_Owner (A_Door : Door) return Boolean is
    (A_Door.Door_Pointer.Cond.Is_Owner);


  -- Get a key that is valid for a door
  function Get_Key (A_Door : Door) return Key_Type is
    (A_Door.Door_Pointer.Cond.Get_Key);

  -- Is Is a key valid for a door
  function Is_Valid (A_Door : Door; Key : Key_Type) return Boolean is
    (A_Door.Door_Pointer.Cond.Is_Valid (Key));


  -- Utilities --
  -- Open the door if number of waiters is reached
  function Check_Open (A_Door : in Door) return Boolean is
  begin
    if A_Door.Door_Pointer.Expected /= 0
    and then A_Door.Door_Pointer.Current >= A_Door.Door_Pointer.Expected then
      -- Release all waiters of wait
      A_Door.Door_Pointer.Cond.Broadcast;
      A_Door.Door_Pointer.Current := 0;
      return True;
    else
      return False;
    end if;
  end Check_Open;
  procedure Check_Open (A_Door : in Door) is
    Dummy : Boolean;
  begin
    Dummy := Check_Open (A_Door);
  end Check_Open;

  -- Check that current tasks has access
  procedure Check_Access (A_Door : in Door) is
  begin
    if not A_Door.Door_Pointer.Cond.Is_Owner then
      raise No_Access;
    end if;
  end Check_Access;


  -- Set/get nb waiters --
  -- Set the expected number of waiters
  procedure Set_Nb_Waiters (A_Door : in Door;
                            To : in Natural) is
  begin
    Check_Access (A_Door);
    A_Door.Door_Pointer.Expected := To;
    -- This may lead to open the door
    Check_Open (A_Door);
  end Set_Nb_Waiters;

  -- Get the expected number of waiters
  function Get_Nb_Waiters (A_Door : in Door) return Natural is
  begin
    Check_Access (A_Door);
    return A_Door.Door_Pointer.Expected;
  end Get_Nb_Waiters;

  -- Increment/decrement the expected number of waiters
  -- (The expected number of waiters remains always Positive)
  procedure Add_To_Nb_Waiters (A_Door : in Door;
                               Val : in Integer) is
  begin
    Check_Access (A_Door);
    if A_Door.Door_Pointer.Expected + Val < 0 then
       Release (A_Door);
       raise Constraint_Error;
    end if;
    begin
      A_Door.Door_Pointer.Expected :=
          A_Door.Door_Pointer.Expected + Val;
    exception
      -- Overflow
      when Constraint_Error =>
        A_Door.Door_Pointer.Expected := Natural'Last;
    end;
    -- This may lead to open the door
    Check_Open (A_Door);
  end Add_To_Nb_Waiters;


  -- Wait --
  -- Wait until the required number of waiters is reached
  function Wait (A_Door : Door;
                 Waiting_Time : Duration;
                 Key : Key_Type := Fake) return Boolean is
    Result : Boolean;
  begin
    Check_Access (A_Door);
    if A_Door.Door_Pointer.Cond.Is_Valid (Key) then
      -- If Key is Pass, then simply return True (access remains granted)
      return True;
    end if;
    -- One more waiter
    A_Door.Door_Pointer.Current := A_Door.Door_Pointer.Current + 1;
    if Check_Open (A_Door) then
      -- Expected number of waiters is reached, release them
      return True;
    else
      -- Wait
      Result := A_Door.Door_Pointer.Cond.Wait (Waiting_Time);
      if not Result then
        -- Giving up: one waiter less
        A_Door.Door_Pointer.Current := A_Door.Door_Pointer.Current - 1;
      end if;
      return Result;
    end if;
  end Wait;

  procedure Wait (A_Door : in Door; Key : in Key_Type := Fake) is
    Dummy : Boolean;
  begin
    Dummy := Wait (A_Door, -1.0, Key);
  end Wait;

end Doors;

