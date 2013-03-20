package body U_Rand is
  Init_C : constant := 362436.0/16777216.0;
  Cd     : constant := 7654321.0/16777216.0;
  Cm     : constant := 16777213.0/16777216.0;

  subtype Range_1 is Integer range 0 .. M1 - 1;
  subtype Range_2 is Integer range 0 .. M2 - 1;

  procedure Start(Gen   : in out Generator;
                  New_I : in Seed_Range_1 := Default_I;
                  New_J : in Seed_Range_1 := Default_J;
                  New_K : in Seed_Range_1 := Default_K;
                  New_L : in Seed_Range_2 := Default_L) is
    I, J, K : Range_1;
    L       : Range_2;
    S, T : Float;
    M    : Range_1;
  begin
    I := New_I;
    J := New_J;
    K := New_K;
    L := New_L;
    Gen.Ni := Range_3'Last;
    Gen.Nj := (Range_3'Last/3) + 1;
    Gen.C := Init_C;

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
      Gen.U(Ii) := S;
    end loop;
    Gen.Started := True;
  end Start;

  procedure Next (Gen : in out Generator; Val : out Float) is
  begin
    Val := Next (Gen);
  end Next;

  function Next (Gen : in out Generator) return Float is
    Temp : Float;
  begin
    if not Gen.Started then
      Start (Gen);
    end if;
    Temp := Gen.U(Gen.Ni) - Gen.U(Gen.Nj);
    if Temp < 0.0 then
      Temp := Temp + 1.0;
    end if;
    Gen.U(Gen.Ni) := Temp;
    Gen.Ni := Gen.Ni - 1;
    if Gen.Ni = 0 then
      Gen.Ni := Range_3'Last;
    end if;
    Gen.Nj := Gen.Nj - 1;
    if Gen.Nj = 0 then
      Gen.Nj := Range_3'Last;
    end if;
    Gen.C := Gen.C - Cd;
    if Gen.C < 0.0 then
      Gen.C := Gen.C + Cm;
    end if;
    Temp := Temp - Gen.C;
    if Temp < 0.0 then
      Temp := Temp + 1.0;
    end if;
    return Temp;
  end Next;

end U_Rand;

