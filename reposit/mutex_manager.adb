package body MUTEX_MANAGER is

  -- the task which implements the mutex
  task body MUT_TASK is
    -- Mutex is free at creation
    FREE : BOOLEAN := TRUE;
  begin
    loop
      select
        when FREE =>
          -- Mutex can be got only if it is free
          accept MUT_GET do
            FREE := FALSE;
          end MUT_GET;
      or
        accept MUT_REL(STATUS : out BOOLEAN) do
          -- Mutex is released, but status is set to false if it was already free
          STATUS := not FREE;
          FREE := TRUE;
        end MUT_REL;
      or
        -- For clean termination
        terminate;
      end select;
    end loop;
  end MUT_TASK;


  -- The entry point
  function GET_MUTEX (A_MUTEX      : MUTEX;
                      WAITING_TIME : DURATION) return BOOLEAN is
    RESULT : BOOLEAN;
  begin
    if WAITING_TIME < 0.0 then
      -- Negative delay : unconditional waiting
      A_MUTEX.POINTER.MUT_GET;
      RESULT := TRUE;
    else
      -- Delay
      select
        A_MUTEX.POINTER.MUT_GET;
        RESULT := TRUE;
      or
        delay WAITING_TIME;
        RESULT := FALSE;
      end select;
    end if;
    return RESULT;
  end GET_MUTEX;
 
  procedure RELEASE_MUTEX (A_MUTEX : in MUTEX) is
    MUT_STATUS : BOOLEAN;
  begin
    -- Request releasing
    A_MUTEX.POINTER.MUT_REL(MUT_STATUS);
    if not MUT_STATUS then
      -- The mutex was already free
      raise MUTEX_ALREADY_FREE;
    end if;
  end RELEASE_MUTEX;

end MUTEX_MANAGER;
