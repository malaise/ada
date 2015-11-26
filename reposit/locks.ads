package Locks is

  -- A lock
  type Lock is tagged private;

  -- The lock can be open or closed, open by default
  procedure Open  (A_Lock : in Lock);
  procedure Close (A_Lock : in Lock);

  -- Wait until the lock is open or timeout
  -- Return True if not timeout
  function Wait (A_Lock : Lock;
                 Waiting_Time : Duration) return Boolean;
  procedure Wait (A_Lock : in Lock);

private

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


