
--------------------------------------------------------------------------
-- The following is an implementation of a "universal" random number    --
-- generator algorithm developed by Dr. George Marsaglia of the         --
-- Supercomputer Computations Research Institute (SCRI) at Florida      --
-- State University.  This generator has a period of ~2**144 and has    --
-- been tailored for reproducibility in all CPU's with at least         --
-- 16 bit integer arithmetic and 24 bit floating point.  This algorithm --
-- does not generate random numbers < 2**-24.  At the end of this file  --
-- you will find a self test program that checks generated results      --
-- against known expected results and reports any inaccuracies.         --
--                                                                      --
-- Further references: "Toward a Universal Random Number Generator",    --
-- appearing in the Journal of The American Statistical Association.    --
--                                                                      --
-- This code appeared in the March/April publication of SIGAda's        --
-- Ada Letters and is considered public domain.  PCK                    --
--------------------------------------------------------------------------


package body U_RAND is
  M3     : constant := 97;
  INIT_C : constant := 362436.0/16777216.0;
  CD     : constant := 7654321.0/16777216.0;
  CM     : constant := 16777213.0/16777216.0;

  subtype RANGE_1 is INTEGER range 0 .. M1 - 1;
  subtype RANGE_2 is INTEGER range 0 .. M2 - 1;
  subtype RANGE_3 is INTEGER range 1 .. M3;

  I, J, K : RANGE_1;
  NI, NJ  : INTEGER;
  L       : RANGE_2;
  C       : FLOAT;
  U       : array(RANGE_3) of FLOAT;

  procedure START(NEW_I : in SEED_RANGE_1 := DEFAULT_I;
                  NEW_J : in SEED_RANGE_1 := DEFAULT_J;
                  NEW_K : in SEED_RANGE_1 := DEFAULT_K;
                  NEW_L : in SEED_RANGE_2 := DEFAULT_L) is
    S, T : FLOAT;
    M    : RANGE_1;
  begin
    I := NEW_I;
    J := NEW_J;
    K := NEW_K;
    L := NEW_L;
    NI := RANGE_3'LAST;
    NJ := (RANGE_3'LAST/3) + 1;
    C := INIT_C;

    for II in RANGE_3 loop
      S := 0.0;
      T := 0.5;
      for JJ in 1 .. 24 loop
        M := (((J*I) mod M1)*K) mod M1;
        I := J;
        J := K;
        K := M;
        L := (53*L + 1) mod M2;
        if ((L*M) mod 64) >= 32 then
          S := S + T;
        end if;
        T := 0.5*T;
      end loop;
      U(II) := S;
    end loop;
  end START;

  function NEXT return FLOAT is
    TEMP : FLOAT;
  begin
    TEMP := U(NI) - U(NJ);
    if TEMP < 0.0 then
      TEMP := TEMP + 1.0;
    end if;
    U(NI) := TEMP;
    NI := NI - 1;
    if NI = 0 then
      NI := RANGE_3'LAST;
    end if;
    NJ := NJ - 1;
    if NJ = 0 then
      NJ := RANGE_3'LAST;
    end if;
    C := C - CD;
    if C < 0.0 then
      C := C + CM;
    end if;
    TEMP := TEMP - C;
    if TEMP < 0.0 then
      TEMP := TEMP + 1.0;
    end if;
    return TEMP;
  end NEXT;

begin
  -- initialize table U
  START;
end U_RAND;
