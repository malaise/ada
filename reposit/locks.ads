-- Open/Close or Wait on a lock
package Locks is

  -- A lock
  type Lock is tagged private;

  -- The lock can be open or closed, open by default
  procedure Open  (A_Lock : in Lock);
  procedure Close (A_Lock : in Lock);

  -- If Key is Pass, then simply pass through the lock (return True)
  type Key_Type is private;
  Fake, Pass : constant Key_Type;
  -- Otherwise wait until the lock is open or timeout
  -- Return True if not timeout
  function Wait (A_Lock : Lock;
                 Waiting_Time : Duration;
                 Key : Key_Type := Fake) return Boolean;
  procedure Wait (A_Lock : in Lock; Key : in Key_Type := Fake);

private

  -- Key
  type Key_Type is record
    Is_Pass : Boolean := False;
  end record;
  Fake : constant Key_Type := (others => <>);
  Pass : constant Key_Type := (Is_Pass => True);

  protected type Lock_Protect is
    procedure Open;
    procedure Close;
    entry Wait;
  private
    Is_Open : Boolean := True;
  end Lock_Protect;

  -- Access to the protected lock, so that copies share the same state
  type Lock_Access is access Lock_Protect;
  type Lock is tagged record
    Lock_Pointer : Lock_Access := new Lock_Protect;
  end record;

end Locks;


