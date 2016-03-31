package body Locks is

  protected body Lock_Protect is

    procedure Open is
    begin
      Is_Open := True;
    end Open;

    procedure Close is
    begin
      Is_Open := False;
    end Close;

    entry Wait when Is_Open is
    begin
      null;
    end Wait;
  end Lock_Protect;

  -- The lock can be open or closed, open by default
  procedure Open  (A_Lock : in Lock) is
  begin
    A_Lock.Lock_Pointer.Open;
  end Open;

  procedure Close (A_Lock : in Lock) is
  begin
    A_Lock.Lock_Pointer.Close;
  end Close;

  -- Wait until the lock is open or timeout
  -- Return True if not timeout
  function Wait (A_Lock : Lock;
                 Waiting_Time : Duration;
                 Key : Key_Type := Fake) return Boolean is
    Result : Boolean;
  begin
     if Key = Pass then
      -- If Key is Pass, then simply return True
      return True;
    end if;

    if Waiting_Time < 0.0 then
      -- Negative delay : unconditional waiting
      A_Lock.Lock_Pointer.Wait;
      Result := True;
    else
      -- Delay
      select
        A_Lock.Lock_Pointer.Wait;
        Result := True;
      or
        delay Waiting_Time;
        Result := False;
      end select;
    end if;
    return Result;
  end Wait;

  procedure Wait (A_Lock : in Lock; Key : in Key_Type := Fake) is
  begin
     if Key = Pass then
      -- If Key is Pass, then simply return
      return;
    end if;
    A_Lock.Lock_Pointer.Wait;
  end Wait;

end Locks;

