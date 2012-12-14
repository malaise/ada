with System;
with Ada.Unchecked_Deallocation;
with C_Types;
package body Rnd is

  procedure Free is new Ada.Unchecked_Deallocation (Gen_Rec, Gen_Access);


  function C_Gettimeofday (Tv : System.Address; Tz : System.Address)
  return Integer;
  pragma Import (C, C_Gettimeofday, "gettimeofday");

  -- Return integer equal or below X
  function To_Int (X : Float) return Long_Long_Integer is
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
  end To_Int;

  -- Initialisation of sequence
  procedure Randomize (Gen : in Generator; Init : in Float := 1.0) is
    -- The result of mutex allocation is always true, because infinite waiting
    Ok : Boolean;
    pragma Unreferenced (Ok);
    F : Float;
    I : U_Rand.Seed_Range_1;

    -- Gives a "random" number
    function Init_Aleat return Float is
      Tv : C_Types.Timeval_T;
      Dummy : Integer;
      pragma Unreferenced (Dummy);
    begin
      Dummy := C_Gettimeofday (Tv'Address, System.Null_Address);
      return Float(Tv.Tv_Usec) / 1.0E6;
    end Init_Aleat;

  begin
    -- 0 <= init <= 1 : Ok, otherwise random
    if 0.0 <= Init and then Init < 1.0 then
      F := Init;
    else
      F := Init_Aleat;
    end if;
    I := U_Rand.Seed_Range_1 (F * Float(U_Rand.Seed_Range_1'Last - 1) + 1.0);

    Ok := Mutex_Manager.Get (Gen.Acc.Lock, -1.0);
    U_Rand.Start (Gen.Acc.Ugen, New_I => I);
    Mutex_Manager.Release (Gen.Acc.Lock);
  end Randomize;


  -- Next element in sequence
  function Random (Gen : Generator; Mini : Float := 0.0; Maxi : Float := 1.0)
                  return Float is
    -- Returned value
    Val : Float;
    Ok : Boolean;
    pragma Unreferenced (Ok);
  begin
    Ok := Mutex_Manager.Get (Gen.Acc.Lock, -1.0);
    U_Rand.Next (Gen.Acc.Ugen, Val);
    Mutex_Manager.Release (Gen.Acc.Lock);
    -- Here 0 <= Val < 1
    if Mini >= Maxi then
      return Val;
    else
      return Mini + (Val * (Maxi - Mini) );
    end if;
  end Random;

  function Discr_Random (Gen : Generator;
                         Mini : Num := Num'First; Maxi : Num := Num'Last)
           return Num is
  begin
    return
      Num'Val (
        Integer (
          To_Int (
            Random (Gen, Float (Num'Pos (Mini)), Float (Num'Pos (Maxi)) + 1.0)
          )
        )
      );
  end Discr_Random;

  function Int_Random (Gen : Generator;
                       Mini : Integer := 0; Maxi : Integer := 1)
           return Integer is
  begin
    return
      Integer (
       To_Int (Random (Gen, Float (Mini), Float (Maxi) + 1.0) )
      );
  end Int_Random;

  function Float_Random (Gen : Generator;
                         Mini : Float := 0.0; Maxi : Float := 1.0)
           return Float is
  begin
    return
      Random (Gen, Mini, Maxi);
  end Float_Random;

  function Dur_Random (Gen : Generator;
                       Mini : Duration := 0.0; Maxi : Duration := 1.0)
           return Duration is
  begin
    return
      Duration (Random (Gen, Float(Mini), Float(Maxi) ) );
  end Dur_Random;

  overriding procedure Finalize (Gen : in out Generator) is
  begin
    Free (Gen.Acc);
  end Finalize;

end Rnd;

