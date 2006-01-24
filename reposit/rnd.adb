with System, Mutex_Manager;
with U_Rand;
package body Rnd is

  type C_Struct_Timeval is record
    Sec : Integer;
    Usec : Integer;
  end record;
  function C_Gettimeofday (Tv : System.Address; Tz : System.Address)
  return Integer;
  pragma Import (C, C_Gettimeofday, "gettimeofday");

  -- Protection of critical sections
  Lock : Mutex_Manager.Mutex;

  -- Trunc a real, for randomize and random
  function Long_Trunc (X : Float) return Long_Long_Integer is
    Max : constant Float := Float (Long_Long_Integer'Last);
    Min : constant Float := Float (Long_Long_Integer'First);
    Int : Long_Long_Integer;
  begin
    if (X > Max) or else (X < Min) then raise Constraint_Error; end if;
    Int := Long_Long_Integer (X);
    -- Adjust at +/- 1
    if X > 0.0 then
      -- If x > 0 error by excess
      if Float (Int) > X then Int := Int - 1; end if;
      return Int;
    else
      -- If x < 0 error by default
      if Float (Int) < X then Int := Int + 1; end if;
      return Int;
    end if;
  exception
    when others => raise Constraint_Error;
  end Long_Trunc;

  -- Initialisation of sequence
  procedure Randomize (Init : in Float := 1.0) is
    -- The result of mutex allocation is always true, because infinite waiting
    Ok : Boolean;
    F : Float;
    I : U_Rand.Seed_Range_1;

    -- Gives a "random" number
    function Init_Aleat return Float is
      Tv : C_Struct_Timeval;
      Dummy : Integer;
    begin
      Dummy := C_Gettimeofday (Tv'Address, System.Null_Address);
      return Float(Tv.Usec) / 1.0E6;
    end Init_Aleat;

  begin
    -- 0 <= init <= 1 : Ok, otherwise random
    if 0.0 <= Init and then Init < 1.0 then
      F := Init;
    else
      F := Init_Aleat;
    end if;
    I := U_Rand.Seed_Range_1 (F * Float(U_Rand.Seed_Range_1'Last - 1) + 1.0);

    Ok := Mutex_Manager.Get_Mutex (Lock, -1.0);
    U_Rand.Start (New_I => I);
    Mutex_Manager.Release_Mutex (Lock);
  end Randomize;


  -- Next element in sequence
  function Random (Mini : Float := 0.0; Maxi : Float := 1.0)
    return Float is
    -- Returned value
    Val : Float;
    Ok : Boolean;
  begin
    Ok := Mutex_Manager.Get_Mutex (Lock, -1.0);
    Val := U_Rand.Next;
    Mutex_Manager.Release_Mutex (Lock);
    -- Here 0 <= Val < 1
    if Mini >= Maxi then
      return Val;
    else
      return Mini + (Val * (Maxi - Mini) );
    end if;
  end Random;

  function Discr_Random (Mini : Num := Num'First; Maxi : Num := Num'Last)
    return Num is
  begin
    return
      Num'Val (
        Integer (
          Long_Trunc (
            Random (Float (Num'Pos (Mini)), Float (Num'Pos (Maxi)) + 1.0)
          )
        )
      );
  end Discr_Random;

  function Int_Random (Mini : Integer := 0; Maxi : Integer := 1)
    return Integer is
  begin
    return
      Integer (
       Long_Trunc (Random (Float (Mini), Float (Maxi) + 1.0) )
      );
  end Int_Random;

  function Float_Random (Mini : Float := 0.0; Maxi : Float := 1.0)
    return Float is
  begin
    return
      Random (Mini, Maxi);
  end Float_Random;

  function Dur_Random (Mini : Duration := 0.0; Maxi : Duration := 1.0)
    return Duration is
  begin
    return
      Duration (Random (Float(Mini), Float(Maxi) ) );
  end Dur_Random;

end Rnd;

