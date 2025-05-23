with System;
with Long_Longs, C_Types;
package body Rnd is

  function C_Gettimeofday (Tv : System.Address; Tz : System.Address)
  return Integer
    with Import => True, Convention => C, External_Name => "gettimeofday";

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
  procedure Randomize (Agen : in out Generator;
                       Init : in Float := Randomly;
                       Only_First_Time : in Boolean := True) is
    -- The result of mutex allocation is always true, because infinite waiting
    F : Float;

    -- Gives a "random" number
    function Init_Rnd return Float is
      Tv : C_Types.Timeval_T;
      Dummy : Integer;
    begin
      Dummy := C_Gettimeofday (Tv'Address, System.Null_Address);
      return Float(Tv.Tv_Usec) / 1.0E6;
    end Init_Rnd;

    use type K_Rand.Natural_Val;

  begin
    -- 0 <= init < 1 : Ok, otherwise random
    if 0.0 <= Init and then Init < Randomly then
      F := Init;
    elsif not (Agen.Randomized and then Only_First_Time) then
      -- By default, only the first time
      F := Init_Rnd;
    else
      return;
    end if;

    Agen.Lock.Get;
    case Agen.Kind is
      when Simple =>
        Agen.Sgen.Start (New_W =>
          S_Rand.Natural_Val (F * Float(S_Rand.Natural_Val'Last - 1) + 1.0));
      when Universal =>
        Agen.Ugen.Start (New_I =>
          U_Rand.Seed_Range_1 (F * Float(U_Rand.Seed_Range_1'Last - 1) + 1.0));
      when Kiss =>
        Agen.Kgen.Start (New_W =>
          K_Rand.Natural_Val (F * Float(K_Rand.Natural_Val'Last - 1) + 1.0));
    end case;
    Agen.Randomized := True;
    Agen.Lock.Release;
  exception
    when others =>
      Agen.Lock.Release;
      raise;
  end Randomize;

  -- A Generator is initially not randomized
  function Is_Randomized (Agen : in out Generator) return Boolean is
  begin
    Agen.Lock.Get;
    return Result : Boolean do
      Result := Gen.Randomized;
      Agen.Lock.Release;
    end return;
  exception
    when others =>
      Agen.Lock.Release;
      raise;
  end Is_Randomized;

  -- Run the generator a random amount of times, from 0 to Max
  procedure Run (Agen : in out Generator; Max : Positive) is
    Dummy :  Integer;
  begin
    for I in 1 .. Rnd.Gen.Int_Random(1, Max) loop
      Dummy :=  Int_Random (Agen);
    end loop;
  end Run;

  -- Next element in sequence
  function Random (Agen : in out Generator;
                   Mini : in Float := 0.0; Maxi : in Float := 1.0)
                  return Float is

    Val : Float;
  begin
    Agen.Lock.Get;
    case Agen.Kind is
      when Simple =>
        Agen.Sgen.Next (Val);
      when Universal =>
        Agen.Ugen.Next (Val);
      when Kiss =>
        Agen.Kgen.Next (Val);
    end case;
    Agen.Lock.Release;
    -- Here 0 <= Val < 1
    return (if Mini >= Maxi then Val else Mini + Val * (Maxi - Mini));
  exception
    when others =>
      Agen.Lock.Release;
      raise;
  end Random;

  function Discr_Random (Agen : in out Generator;
                         Mini : in Num := Num'First; Maxi : in Num := Num'Last)
           return Num is
    (Num'Val (
       To_Long_Long (
         Random (Agen, Float (Num'Pos (Mini)), Float (Num'Pos (Maxi)) + 1.0)
       )
    ));

  function Int_Random (Agen : in out Generator;
                       Mini : Integer := 0; Maxi : Integer := 1)
           return Integer is
    (Integer (
      To_Long_Long (Random (Agen, Float (Mini), Float (Maxi) + 1.0) )
    ));

  function Mod_Random (Agen : in out Generator;
                       Mini : in Modulus := Modulus'First;
                       Maxi : in Modulus := Modulus'Last) return Modulus is
    X : Float;
    M : Modulus;
  begin
    X := Random (Agen, Float (Mini), Float (Maxi) + 1.0);
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

  function Float_Random (Agen : in out Generator;
                         Mini : in Float := 0.0; Maxi : in Float := 1.0)
           return Float is
    (Random (Agen, Mini, Maxi));

  function Dur_Random (Agen : in out Generator;
                       Mini : in Duration := 0.0; Maxi : in Duration := 1.0)
           return Duration is
    (Duration (Random (Agen, Float(Mini), Float(Maxi) ) ));

end Rnd;

