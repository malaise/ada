package body Mutex_Manager is

  -- the task which implements the mutex
  task body Mut_Task is
    -- Mutex is free at creation
    Free : Boolean := True;
  begin
    loop
      select
        when Free =>
          -- Mutex can be got only if it is free
          accept Mut_Get do
            Free := False;
          end Mut_Get;
      or
        accept Mut_Rel(Status : out Boolean) do
          -- Mutex is released, but status is set to false if it was already free
          Status := not Free;
          Free := True;
        end Mut_Rel;
      or
        -- For clean termination
        terminate;
      end select;
    end loop;
  end Mut_Task;


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
