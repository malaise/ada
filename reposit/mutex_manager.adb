package body Mutex_Manager is

  -- The protected object which implements the mutex
  protected body Mut_Protect is
    -- Mutex is free at creation
    entry Mut_Get when Free is
    begin
      Free := False;
    end Mut_Get;

    entry Mut_Rel(Status : out Boolean) when True is
    begin
      -- Mutex is released, but status is set to false if it was already free
      Status := not Free;
      Free := True;
    end Mut_Rel;
  end Mut_Protect;


  -- The entry point
  function Get_Mutex (A_Mutex      : Mutex;
                      Waiting_Time : Duration) return Boolean is
    Result : Boolean;
  begin
    if Waiting_Time < 0.0 then
      -- Negative delay : unconditional waiting
      A_Mutex.Pointer.Mut_Get;
      Result := True;
    else
      -- Delay
      select
        A_Mutex.Pointer.Mut_Get;
        Result := True;
      or
        delay Waiting_Time;
        Result := False;
      end select;
    end if;
    return Result;
  end Get_Mutex;
 
  procedure Release_Mutex (A_Mutex : in Mutex) is
    Mut_Status : Boolean;
  begin
    -- Request releasing
    A_Mutex.Pointer.Mut_Rel(Mut_Status);
    if not Mut_Status then
      -- The mutex was already free
      raise Mutex_Already_Free;
    end if;
  end Release_Mutex;

end Mutex_Manager;

