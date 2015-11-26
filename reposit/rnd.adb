with System;
with Long_Longs, C_Types;
package body Rnd is

  function C_Gettimeofday (Tv : System.Address; Tz : System.Address)
  return Integer;
  pragma Import (C, C_Gettimeofday, "gettimeofday");

  -- Return Long_Long equal or below X
  function To_Long_Long (X : Float) return Long_Longs.Ll_Integer is
    Max : constant Float := Float(Long_Long_Integer'Last);
    Min : constant Float := Float(Long_Long_Integer'First);
    Int : Long_Long_Integer;
  begin
    if X > Max or else X < Min then
      raise Constraint_Error;
    end if;
    Int := Long_Long_Integer(X);
    -- Adjust at +/- 1
    -- Error is always by excess (ex: 3.56 -> 4 want 3, or -3.44 -> -3 want -4)
    if Float(Int) > X then
      Int := Int - 1;
    end if;
    return Int;
  exception
    when others =>
      raise Constraint_Error;
  end To_Long_Long;

  -- Initialisation of sequence
  procedure Randomize (Gen : in out Generator; Init : in Float := 1.0) is
    -- The result of mutex allocation is always true, because infinite waiting
    Dummy_Ok : Boolean;
    F : Float;
    I : U_Rand.Seed_Range_1;

    -- Gives a "random" number
    function Init_Aleat return Float is
      Tv : C_Types.Timeval_T;
      Dummy : Integer;
    begin
      Dummy := C_Gettimeofday (Tv'Address, System.Null_Address);
      return Float(Tv.Tv_Usec) / 1.0E6;
    end Init_Aleat;

  begin
    -- 0 <= init <= 1 : Ok, otherwise random
    F := (if 0.0 <= Init and then Init < 1.0 then Init else Init_Aleat);
    I := U_Rand.Seed_Range_1 (F * Float(U_Rand.Seed_Range_1'Last - 1) + 1.0);

    Dummy_Ok := Gen.Lock.Get (-1.0);
    U_Rand.Start (Gen.Ugen, New_I => I);
    Gen.Randomized := True;
    Gen.Lock.Release;
  end Randomize;

  -- A Generator is initially not radomized
  function Is_Randomized (Gen : in out Generator) return Boolean is
    Dummy_Ok : Boolean;
  begin
    Dummy_Ok := Gen.Lock.Get (-1.0);
    return Result : Boolean do
      Result := Gen.Randomized;
      Gen.Lock.Release;
    end return;
  end Is_Randomized;

  -- Next element in sequence
  function Random (Gen : in out Generator;
                   Mini : in Float := 0.0; Maxi : in Float := 1.0)
                  return Float is
    -- Returned value
    Val : Float;
    Dummy_Ok : Boolean;
  begin
    Dummy_Ok := Gen.Lock.Get (-1.0);
    U_Rand.Next (Gen.Ugen, Val);
    Gen.Lock.Release;
    -- Here 0 <= Val < 1
    return (if Mini >= Maxi then Val else Mini + (Val * (Maxi - Mini) ));
  end Random;

  function Discr_Random (Gen : in out Generator;
                         Mini : in Num := Num'First; Maxi : in Num := Num'Last)
           return Num is
  begin
    return
      Num'Val (
        To_Long_Long (
          Random (Gen, Float (Num'Pos (Mini)), Float (Num'Pos (Maxi)) + 1.0)
        )
      );
  end Discr_Random;

  function Int_Random (Gen : in out Generator;
                       Mini : Integer := 0; Maxi : Integer := 1)
           return Integer is
  begin
    return
      Integer (
       To_Long_Long (Random (Gen, Float (Mini), Float (Maxi) + 1.0) )
      );
  end Int_Random;

  function Mod_Random (Gen : in out Generator;
                       Mini : in Modulus := Modulus'First;
                       Maxi : in Modulus := Modulus'Last) return Modulus is
    X : Float;
    M : Modulus;
  begin
    X := Random (Gen, Float (Mini), Float (Maxi) + 1.0);
    M := Modulus (X);
    if Float(M) > X then
      -- Round to modulus lead to > X
      M := M - 1;
    end if;
    return M;
  exception
    when Constraint_Error =>
      -- Round to modulus lead to overflow
      return Maxi;
  end Mod_Random;

  function Float_Random (Gen : in out Generator;
                         Mini : in Float := 0.0; Maxi : in Float := 1.0)
           return Float is
  begin
    return
      Random (Gen, Mini, Maxi);
  end Float_Random;

  function Dur_Random (Gen : in out Generator;
                       Mini : in Duration := 0.0; Maxi : in Duration := 1.0)
           return Duration is
  begin
    return
      Duration (Random (Gen, Float(Mini), Float(Maxi) ) );
  end Dur_Random;

end Rnd;

