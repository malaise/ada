with SYSTEM, MUTEX_MANAGER;
with U_RAND;
package body RND is

  type C_STRUCT_TIMEVAL is record
    SEC : INTEGER;
    USEC : INTEGER;
  end record;
  function C_GETTIMEOFDAY (TV : SYSTEM.ADDRESS; TZ : SYSTEM.ADDRESS)
  return INTEGER;
  pragma IMPORT (C, C_GETTIMEOFDAY, "gettimeofday");

  -- protection of critical sections
  LOCK : MUTEX_MANAGER.MUTEX;

  -- trunc a real, for randomize and random
  function LONG_TRUNC (X : FLOAT) return LONG_LONG_INTEGER is
    MAX : constant FLOAT := FLOAT (LONG_LONG_INTEGER'LAST);
    MIN : constant FLOAT := FLOAT (LONG_LONG_INTEGER'FIRST);
    INT : LONG_LONG_INTEGER;
  begin
    if (X > MAX) or else (X < MIN) then raise CONSTRAINT_ERROR; end if;
    INT := LONG_LONG_INTEGER (X);
    -- adjust at +/- 1
    if X > 0.0 then
      -- if x > 0 error by excess
      if FLOAT (INT) > X then INT := INT - 1; end if;
      return INT;
    else
      -- if  x < 0 error by default
      if FLOAT (INT) < X then INT := INT + 1; end if;
      return INT;
    end if;
  exception
    when others => raise CONSTRAINT_ERROR;
  end LONG_TRUNC;

  -- initialisation of sequence
  procedure RANDOMIZE (INIT : in FLOAT := 1.0) is
    -- the result of mutex allocation is always true, because infinite waiting
    OK : BOOLEAN;
    F : FLOAT;
    I : U_RAND.SEED_RANGE_1;

    -- gives a "random" number
    function INIT_ALEAT return FLOAT is
      TV : C_STRUCT_TIMEVAL;
      DUMMY : INTEGER;
    begin
      DUMMY := C_GETTIMEOFDAY (TV'ADDRESS, SYSTEM.NULL_ADDRESS);
      return FLOAT(TV.USEC) / 1.0E6;
    end INIT_ALEAT;

  begin
    -- 0 <= init <= 1 : OK, otherwise random
    if 0.0 <= INIT and then INIT < 1.0 then
      F := INIT;
    else
      F := INIT_ALEAT;
    end if;
    I := U_RAND.SEED_RANGE_1 (F * FLOAT(U_RAND.SEED_RANGE_1'LAST - 1) + 1.0);
    
    OK := MUTEX_MANAGER.GET_MUTEX (LOCK, -1.0);
    U_RAND.START (NEW_I => I);
    MUTEX_MANAGER.RELEASE_MUTEX (LOCK);
  end RANDOMIZE;


  -- Next element in sequence
  function RANDOM (MINI : FLOAT := 0.0; MAXI : FLOAT := 1.0)
    return FLOAT is
    -- Returned value
    VAL : FLOAT;
    OK : BOOLEAN;
  begin
    OK := MUTEX_MANAGER.GET_MUTEX (LOCK, -1.0);
    VAL := U_RAND.NEXT;
    MUTEX_MANAGER.RELEASE_MUTEX (LOCK);
    -- Here 0 <= VAL < 1
    if MINI >= MAXI then
      return VAL;
    else
      return MINI + (VAL * (MAXI - MINI) );
    end if;
  end RANDOM;

  function DISCR_RANDOM (MINI : NUM := NUM'FIRST; MAXI : NUM := NUM'LAST)
    return NUM is
  begin
    return
      NUM'VAL (
        INTEGER (
          LONG_TRUNC (
            RANDOM (FLOAT (NUM'POS (MINI)), FLOAT (NUM'POS (MAXI)) + 1.0)
          )
        )
      );
  end DISCR_RANDOM;

  function INT_RANDOM (MINI : INTEGER := 0; MAXI : INTEGER := 1)
    return INTEGER is
  begin
    return
      INTEGER (
       LONG_TRUNC (RANDOM (FLOAT (MINI), FLOAT (MAXI) + 1.0) )
      );
  end INT_RANDOM;

  function FLOAT_RANDOM (MINI : FLOAT := 0.0; MAXI : FLOAT := 1.0)
    return FLOAT is
  begin
    return
      RANDOM (MINI, MAXI);
  end FLOAT_RANDOM;

  function DUR_RANDOM (MINI : DURATION := 0.0; MAXI : DURATION := 1.0)
    return DURATION is
  begin
    return
      DURATION (RANDOM (FLOAT(MINI), FLOAT(MAXI) ) );
  end DUR_RANDOM;

end RND;
