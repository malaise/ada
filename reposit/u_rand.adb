
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


package body U_Rand is
  M3     : constant := 97;
  Init_C : constant := 362436.0/16777216.0;
  Cd     : constant := 7654321.0/16777216.0;
  Cm     : constant := 16777213.0/16777216.0;

  subtype Range_1 is Integer range 0 .. M1 - 1;
  subtype Range_2 is Integer range 0 .. M2 - 1;
  subtype Range_3 is Integer range 1 .. M3;

  I, J, K : Range_1;
  Ni, Nj  : Integer;
  L       : Range_2;
  C       : Float;
  U       : array(Range_3) of Float;

  procedure Start(New_I : in Seed_Range_1 := Default_I;
                  New_J : in Seed_Range_1 := Default_J;
                  New_K : in Seed_Range_1 := Default_K;
                  New_L : in Seed_Range_2 := Default_L) is
    S, T : Float;
    M    : Range_1;
  begin
    I := New_I;
    J := New_J;
    K := New_K;
    L := New_L;
    Ni := Range_3'Last;
    Nj := (Range_3'Last/3) + 1;
    C := Init_C;

    for Ii in Range_3 loop
      S := 0.0;
      T := 0.5;
      for Jj in 1 .. 24 loop
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
      U(Ii) := S;
    end loop;
  end Start;

  function Next return Float is
    Temp : Float;
  begin
    Temp := U(Ni) - U(Nj);
    if Temp < 0.0 then
      Temp := Temp + 1.0;
    end if;
    U(Ni) := Temp;
    Ni := Ni - 1;
    if Ni = 0 then
      Ni := Range_3'Last;
    end if;
    Nj := Nj - 1;
    if Nj = 0 then
      Nj := Range_3'Last;
    end if;
    C := C - Cd;
    if C < 0.0 then
      C := C + Cm;
    end if;
    Temp := Temp - C;
    if Temp < 0.0 then
      Temp := Temp + 1.0;
    end if;
    return Temp;
  end Next;

begin
  -- initialize table U
  Start;
end U_Rand;
