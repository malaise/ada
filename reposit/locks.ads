-- Open/Close or Wait on a lock
private with Magic_Numbers;
package Locks is

  -- A lock
  type Lock is tagged private;

  -- The lock can be open or closed, open by default
  procedure Open  (A_Lock : in Lock);
  procedure Close (A_Lock : in Lock);
  -- Create a closed lock
  function Create_Closed_Lock return Lock;

  -- A key
  type Key_Type is private;
  -- Pass is valid for all locks, Fake is valid for no lock
  Fake, Pass : constant Key_Type;

  -- Get a key that is valid for a lock
  function Get_Key (A_Lock : Lock) return Key_Type;
  -- Is a key valid for a lock (Pass or got from this lock)
  function Is_Valid (A_Lock : Lock; Key : Key_Type) return Boolean;

  -- If Key is valid for the lock, then simply pass through the lock
  --   (and return True)
  -- Otherwise wait until the lock is open (and return True)
  --  or timeout (and return False)
  -- By default a Key is Fake
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
    Magic : Magic_Numbers.Extended_Magic_Long := Magic_Numbers.Magic_Long0;
  end record;
  Fake : constant Key_Type := (others => <>);
  Pass : constant Key_Type := (Is_Pass => True, others => <>);

  protected type Lock_Protect is
    procedure Open;
    procedure Close;
    entry Wait;
  private
    Is_Open : Boolean := True;
  end Lock_Protect;

  -- Access to the protected lock, so that copies share the same state
  type Lock_Access is not null access Lock_Protect;
  type Lock is tagged record
    Lock_Pointer : Lock_Access := new Lock_Protect;
    Magic : Magic_Numbers.Magic_Long := Magic_Numbers.Generate;
  end record;

end Locks;

