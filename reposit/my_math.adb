with Ada.Text_Io, Ada.Numerics.Generic_Elementary_Functions;
package body My_Math is

  package Real_Math is new Ada.Numerics.Generic_Elementary_Functions (Real);
  package Inte_Io is new Ada.Text_Io.Integer_Io (Inte);
  package Real_Io is new Ada.Text_Io.Float_Io (Real);

  --------------------------
  -- Constants for computing
  --------------------------
  -- One cycle in degrees
  Cycle_Deg : constant Real := 360.0;

  -- Get an Inte, get a Real from a string
  -- raise Data_Error if From is not valid
  function Get (From : String) return Inte is
    I : Inte;
    Last : Positive;
  begin
    if From(From'First) <= ' ' then
      raise Data_Error;
    end if;
    Inte_Io.Get (From, I, Last);
    if Last /= From'Last then
      raise Data_Error;
    end if;
    return I;
  exception
    when Ada.Text_Io.Data_Error =>
      raise Data_Error;
  end Get;

  function Get (From : String) return Real is
    R : Real;
    Last : Positive;
  begin
    if From(From'First) <= ' ' then
      raise Data_Error;
    end if;
    Real_Io.Get (From, R, Last);
    if Last /= From'Last then
      raise Data_Error;
    end if;
    return R;
  exception
    when Ada.Text_Io.Data_Error =>
      raise Data_Error;
  end Get;

  -- Integer part of a real
  function Int (X : Real) return Real is

    Neg   : Boolean := False;
    Dig   : constant Positive := Real'Digits;  -- digits of real
    Exp   : constant Positive := 4;            -- +123
    -- Put will use <fore> "." Aft "E" Exp, ajusting <fore> to fit Total
    -- We want: Sd.<14digits>E+123, S being an extra space
    Total : constant Positive := 2 + Dig + 1 + Exp;
    subtype Typ_Index_Str is Positive range 1 .. Total;
    Str_Aux   : String(Typ_Index_Str);
    Str_Exp   : String(1 .. Exp);
    Result    : Real;
    Exponent  : Integer;
    Index_Str : Integer;
  begin
    if X < 0.0 then
      Neg := True;
    end if;

    -- Store  x in a string
    Real_Io.Put(Str_Aux, abs X, Dig - 1, Exp);

    -- Compute exponent
    Str_Exp(1 .. Exp) := Str_Aux(Total - Exp + 1 .. Total);
    Exponent := Integer'Value(Str_Exp);
    if Exponent < 0 then
      -- No integer part
      Result := 0.0;
    elsif Exponent < Dig then

      -- Reset fraction digits to 0
      for Index in 4 + Exponent .. Total - Exp - 1 loop
        Str_Aux(Index) := '0';
      end loop;

      -- Convert result to real
      Real_Io.Get(Str_Aux, Result, Index_Str);
    else

      -- No fraction part (number too big)
      Result := X;
    end if;

    return (if Neg then -Result else Result);
  end Int;

  function Frac (X : Real) return Real is (X - Int(X));

  -- Real to inte : round or trunc
  function Round (X : Real) return Inte is
  begin
    return (if X > 0.0 then Trunc(X + 0.5) else Trunc(X - 0.5));
  exception
    when others =>
      raise Math_Error;
  end Round;

  function Trunc (X : Real) return Inte is
    Epsilon : constant Real := Real'Model_Epsilon;
    Int : Inte;
    Rea : Real;
  begin
    if      X > Real (Inte'Last)
    or else X < Real (Inte'First) then
      raise Math_Error;
    end if;

    -- Specific case
    if X = 0.0 then
      return 0;
    end if;

    -- Round
    Int := Inte (X);
    Rea := Real (Int);

    -- If round leads to a delta of less than Eps, then it is correct
    if abs (Rea - X) / 10.0 ** Lg (abs X) < Epsilon then
      return Int;
    end if;

    -- Adjust +/- 1 due to conversion to Inte
    if X > 0.0 then
      -- if x > 0 error by exceed
      if Rea > X then
        Int := Int - 1;
      end if;
    elsif X < 0.0 then
      -- if x < 0 error by default
      if Rea < X then
        Int := Int + 1;
      end if;
    else
      Int := 0;
    end if;

    return Int;
  exception
    when others =>
      raise Math_Error;
  end Trunc;

  -- Rounded result of division
  function Roundiv (A, B : Inte) return Inte is
    Q, R : Inte;
    Abs_R : Inte;
  begin
    Div (A, B, Q, R);
    Abs_R := abs R;
    if abs B - Abs_R > Abs_R then
      -- R < B/2 => trunc, Q is correct
      return Q;
    end if;
    -- R >= B/2 => round, adjust Q++ or Q--
    if Q = 0 then
      if (A >= 0) = (B >= 0) then
        return Q + 1;
      else
        return Q - 1;
      end if;
    elsif Q >= 0 then
      return Q + 1;
    else
      return Q - 1;
    end if;
  end Roundiv;

  -- Round R at N digits.
  -- If N is positive then it applies to the int part
  -- else it applies to the frac part.
  function Round_At (X : Real; N : Inte) return Real is
    P, M, T, I, F : Real;
    Rounded_Frac : Inte;
  begin
    P := 10.0 ** Real(N);
    -- If N < 0, then work with Frac part of R, saving Int part in M
    -- Else work on R itself
    if N < 0 then
      M := Int (X);
      T := Frac (X);
    else
      M := 0.0;
      T := X;
    end if;
    -- Move the "." at position N
    T := T / P;
    -- Separate Int and Frac
    I := Int (T);
    F := Frac (T);
    -- Round the Frac part, (this leads to -1, 0 or 1)
    Rounded_Frac := Round (F);
    -- Add the result to the Int part
    I := I + Real(Rounded_Frac);
    -- Restore the original "." position
    T := I * P;
    -- Done
    return M + T;
  end Round_At;

  -- Integer division
  procedure Div (A, B : in Inte; Q, R : out Inte) is
  begin
    Q := A / B;
    R := A rem B;
  end Div;

  procedure Div (A : in Real; B : in Inte; Q : out Inte; R : out Real) is
    Rb : constant Real := Real (B);
    D : Real;
  begin
    D := Int (A / Rb);
    Q := Trunc (D);
    R := A - D * Rb;
  end Div;

  -- Power
  function "**" (Number, Exponent : Real) return Real is
  begin
   return Real_Math."**" (Number, Exponent);
  exception
    when others =>
      raise Math_Error;
  end "**";

  -- Square root
  function Sqrt (X : Real) return Real is
  begin
    return Real_Math.Sqrt (X);
  exception
    when others =>
      raise Math_Error;
  end Sqrt;

  -- Based 10 log
  function Lg (X : Real) return Real is
  begin
    return Real_Math.Log (X, 10.0);
  exception
    when others =>
      raise Math_Error;
  end Lg;

  function Exp (X : Real := 1.0) return Real is (E ** X);

  -- Ln
  function Ln (X : Real) return Real is
  begin
    return Real_Math.Log (X);
  exception
    when others =>
      raise Math_Error;
  end Ln;

  -- Trigo
  function Sin (X    : Real;
                Mode : Angle_Unit := Radian) return Real is
  begin
    if Mode = Radian then
      return Real_Math.Sin (X);
    else
      return Real_Math.Sin (X, Cycle_Deg);
    end if;
  exception
    when others =>
      raise Math_Error;
  end Sin;

  function Cos (X    : Real;
                Mode : Angle_Unit := Radian) return Real is
  begin
    if Mode = Radian then
      return Real_Math.Cos (X);
    else
      return Real_Math.Cos (X, Cycle_Deg);
    end if;
  exception
    when others =>
      raise Math_Error;
  end Cos;

  function Tan (X    : Real;
                Mode : Angle_Unit := Radian) return Real is
  begin
    if Mode = Radian then
      return Real_Math.Tan (X);
    else
      return Real_Math.Tan (X, Cycle_Deg);
    end if;
  exception
    when others =>
      raise Math_Error;
  end Tan;

  function Arc_Sin (X    : Real;
                    Mode : Angle_Unit := Radian) return Real is
  begin
    if Mode = Radian then
      return Real_Math.Arcsin (X);
    else
      return Real_Math.Arcsin (X, Cycle_Deg);
    end if;
  exception
    when others =>
      raise Math_Error;
  end Arc_Sin;

  function Arc_Cos (X    : Real;
                    Mode : Angle_Unit := Radian) return Real is
  begin
    if Mode = Radian then
      return Real_Math.Arccos (X);
    else
      return Real_Math.Arccos (X, Cycle_Deg);
    end if;
  exception
    when others =>
      raise Math_Error;
  end Arc_Cos;

  function Arc_Tan (X    : Real;
                    Mode : Angle_Unit := Radian) return Real is
  begin
    if Mode = Radian then
      return Real_Math.Arctan (X);
    else
      return Real_Math.Arctan (X, Cycle => Cycle_Deg);
    end if;
  exception
    when others =>
      raise Math_Error;
  end Arc_Tan;

  function Arc_Tan2 (Y, X : Real;
                    Mode : Angle_Unit := Radian) return Real is
  begin
    if Mode = Radian then
      return Real_Math.Arctan (Y, X);
    else
      return Real_Math.Arctan (Y, X, Cycle_Deg);
    end if;
   exception
    when others =>
      raise Math_Error;
  end Arc_Tan2;

end My_Math;

