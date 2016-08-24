with Ada.Text_Io;
with C_Types;
package body My_Math is

  package Inte_Io is new Ada.Text_Io.Integer_Io (Inte);
  package Real_Io is new Ada.Text_Io.Float_Io (Real);

  function Cpow (X, Y : C_Types.Double) return C_Types.Double;
  pragma Import (C, Cpow, "pow");

  function Csqrt (X : C_Types.Double) return C_Types.Double;
  pragma Import (C, Csqrt, "sqrt");

  function Clog10 (X : C_Types.Double) return C_Types.Double;
  pragma Import (C, Clog10, "log10");

  function Clog (X : C_Types.Double) return C_Types.Double;
  pragma Import (C, Clog, "log");

  function Csin (X : C_Types.Double) return C_Types.Double;
  pragma Import (C, Csin, "sin");

  function Ccos (X : C_Types.Double) return C_Types.Double;
  pragma Import (C, Ccos, "cos");

  function Ctan (X : C_Types.Double) return C_Types.Double;
  pragma Import (C, Ctan, "tan");

  function Casin (X : C_Types.Double) return C_Types.Double;
  pragma Import (C, Casin, "asin");

  function Cacos (X : C_Types.Double) return C_Types.Double;
  pragma Import (C, Cacos, "acos");

  function Catan (X : C_Types.Double) return C_Types.Double;
  pragma Import (C, Catan, "atan");

  function Catan2 (Y, X : C_Types.Double) return C_Types.Double;
  pragma Import (C, Catan2, "atan2");

  --------------------------
  -- Constants for computing
  --------------------------
  -- Multiples and sub-multiples of pi
  Pi_Hundred_Heighty   : constant := Pi/180.0;

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
    Dig   : constant Positive := 15;  -- digits of real
    Exp   : constant Positive := 4;  -- +123
    Total : constant Positive := 2 + Dig + 1 + Exp + 1;
    -- b-1.<14digits>E+123; (b = 1 extra space)
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

    -- store  x in a string
    Real_Io.Put(Str_Aux, abs X , Dig - 1, Exp);

    -- compute exponent
    Str_Exp(1 .. Exp) := Str_Aux(Total - Exp + 1 .. Total);
    Exponent := Integer'Value(Str_Exp);
    if Exponent < 0 then

      -- no integer part
      Result := 0.0;
    elsif Exponent < Dig - 1 then

      -- reset fraction digits to 0
      for Index in Total - Exp - Dig + 1 + Exponent .. Total - Exp - 1 loop
        Str_Aux(Index) := '0';
      end loop;

      -- convert result to real
      Real_Io.Get(Str_Aux, Result, Index_Str);
    else

      -- no fraction part (number to big)
      Result := X;
    end if;

    return (if Neg then -Result else Result);
  end Int;

  function Frac (X : Real) return Real is
  begin
    return X - Int(X);
  end Frac;

  -- Real to inte : round or trunc
  function Round (X : Real) return Inte is
  begin
    return (if X > 0.0 then Trunc(X + 0.5) else Trunc(X - 0.5));
  exception
    when others =>
      raise Math_Error;
  end Round;

  function Trunc (X : Real) return Inte is
    Int : Inte;
  begin
    if      X > Real(Inte'Last)
    or else X < Real(Inte'First) then
      raise Math_Error;
    end if;

    Int := Inte(X);

    -- adjust +/- 1
    if X > 0.0 then
      -- if x > 0 error by exceed
      if Real(Int) > X then
        Int := Int - 1;
      end if;
    elsif X < 0.0 then
      -- if x < 0 error by default
      if Real(Int) < X then
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
  begin
    return My_Math.Round (My_Math.Real(A) / My_Math.Real(B));
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

  -- Power
  function "**" (Number, Exponent : Real) return Real is
  begin
   return Real(Cpow (C_Types.Double(Number), C_Types.Double(Exponent)));
  exception
    when others =>
      raise Math_Error;
  end "**";

  -- Square root
  function Sqrt (X : Real) return Real is
  begin
    if X < 0.0 then
      raise Math_Error;
    end if;
    return Real(Csqrt (C_Types.Double(X)));
  exception
    when others =>
      raise Math_Error;
  end Sqrt;

  -- Based 10 log
  function Lg (X : Real) return Real is
  begin
    if X < 0.0 then
      raise Math_Error;
    end if;
    return Real(Clog10 (C_Types.Double(X)));
  exception
    when others =>
      raise Math_Error;
  end Lg;

  function Exp (X : Real := 1.0) return Real is
  begin
    return E ** X;
  end Exp;

  -- Ln
  function Ln (X : Real) return Real is
  begin
    if X < 0.0 then
      raise Math_Error;
    end if;
    return Real(Clog (C_Types.Double(X)));
  exception
    when others =>
      raise Math_Error;
  end Ln;

  -- Trigo
  function Sin (X    : Real;
                Mode : Angle_Unit := Radian) return Real is
    Y      : Real;
  begin
    Y := (if Mode = Radian then X else X * Pi_Hundred_Heighty);
    return Real(Csin (C_Types.Double(Y)));
  exception
    when others =>
      raise Math_Error;
  end Sin;

  function Cos (X    : Real;
                Mode : Angle_Unit := Radian) return Real is
    Y      : Real;
  begin
    Y := (if Mode = Radian then X else X * Pi_Hundred_Heighty);
    return Real(Ccos (C_Types.Double(Y)));
  exception
    when others =>
      raise Math_Error;
  end Cos;

  function Tan (X    : Real;
                Mode : Angle_Unit := Radian) return Real is
    Y       : Real;
  begin
    Y := (if Mode = Radian then X else X * Pi_Hundred_Heighty);
    return Real(Ctan (C_Types.Double(Y)));
  exception
    when others =>
      raise Math_Error;
  end Tan;

  function Arc_Sin (X    : Real;
                    Mode : Angle_Unit := Radian) return Real is
    Y : Real;
  begin
    if abs X  > 1.0 then
      raise Math_Error;
    end if;
    Y := Real(Casin (C_Types.Double(X)));
    if Mode = Degree then
      Y := Y / Pi_Hundred_Heighty;
    end if;
    return Y;
  exception
    when others =>
      raise Math_Error;
  end Arc_Sin;

  function Arc_Cos (X    : Real;
                    Mode : Angle_Unit := Radian) return Real is
    Y : Real;
  begin
    if abs X > 1.0 then
      raise Math_Error;
    end if;
    Y := Real(Cacos (C_Types.Double(X)));
    if Mode = Degree then
      Y := Y / Pi_Hundred_Heighty;
    end if;
    return Y;
  exception
    when others =>
      raise Math_Error;
  end Arc_Cos;

  function Arc_Tan (X    : Real;
                    Mode : Angle_Unit := Radian) return Real is
    Y : Real;
  begin
    Y := Real(Catan (C_Types.Double(X)));
    if Mode = Degree then
      Y := Y / Pi_Hundred_Heighty;
    end if;
    return Y;
  exception
    when others =>
      raise Math_Error;
  end Arc_Tan;

  function Arc_Tan2 (Y, X : Real;
                    Mode : Angle_Unit := Radian) return Real is
    Z : real;
  begin
    Z := Real(Catan2 (C_Types.Double(Y), C_Types.Double(X)));
    if Mode = Degree then
      Z := Z / Pi_Hundred_Heighty;
    end if;
    return Z;
   exception
    when others =>
      raise Math_Error;
  end Arc_Tan2;

end My_Math;

