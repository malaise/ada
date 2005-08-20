with Ada.Text_Io;
package body My_Math is

  -- Interface to C math library
  type  Double is new Long_Float;

  function Cpow (X, Y : Double) return Double;
  pragma Interface (C, Cpow);
  pragma Interface_Name (Cpow, "pow");

  function Csqrt (X : Double) return Double;
  pragma Interface (C, Csqrt);
  pragma Interface_Name (Csqrt, "sqrt");

  function Clog10 (X : Double) return Double;
  pragma Interface (C, Clog10);
  pragma Interface_Name (Clog10, "log10");

  function Clog (X : Double) return Double;
  pragma Interface (C, Clog);
  pragma Interface_Name (Clog, "log");

  function Csin (X : Double) return Double;
  pragma Interface (C, Csin);
  pragma Interface_Name (Csin, "sin");

  function Ccos (X : Double) return Double;
  pragma Interface (C, Ccos);
  pragma Interface_Name (Ccos, "cos");

  function Ctan (X : Double) return Double;
  pragma Interface (C, Ctan);
  pragma Interface_Name (Ctan, "tan");

  function Casin (X : Double) return Double;
  pragma Interface (C, Casin);
  pragma Interface_Name (Casin, "asin");

  function Cacos (X : Double) return Double;
  pragma Interface (C, Cacos);
  pragma Interface_Name (Cacos, "acos");

  function Catan (X : Double) return Double;
  pragma Interface (C, Catan);
  pragma Interface_Name (Catan, "atan");

  --------------------------
  -- Constants for computing
  --------------------------
  -- Multiples et sub multiples of pi
  Two_Pi               : constant := 2.0*Pi;
  Pi_Two               : constant := Pi/2.0;
  Pi_Four              : constant := Pi/4.0;
  Pi_Hundred_Heighty   : constant := Pi/180.0;


  -- Integer part of a real
  function Int (X : Real) return Real is

    package Real_Text_Io is new Ada.Text_Io.Float_Io(Real);

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
    Real_Text_Io.Put(Str_Aux, abs(X), Dig - 1, Exp);

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
      Real_Text_Io.Get(Str_Aux, Result, Index_Str);
    else

      -- no fraction part (number to big)
      Result := X;
    end if;

    if Neg then
      return -Result;
    else
      return Result;
    end if;
  end Int;

  function Frac (X : Real) return Real is
  begin
    return X - Int(X);
  end Frac;

  -- Real to inte : round or trunc
  function Round (X : Real) return Inte is
    Resultat : Inte;
  begin
    if X > 0.0 then
      Resultat := Trunc(X + 0.5);
    else
      Resultat := Trunc(X - 0.5);
    end if;
    return Resultat;
  exception
    when others =>
      raise Math_Error;
  end Round;

  function Trunc (X : Real) return Inte is
    Int : Inte;
  begin
    if X > Real(Integer'Last) then
      raise Math_Error;
    end if;

    Int := Inte(X);

    -- adjust +- 1
    if X > 0.0 then

      -- if x > 0 error by exceed
      if Real(Int) > X then
        Int := Int - 1;
      end if;

    else

      -- if x < 0 error by default
      if Real(Int) < X then
        Int := Int + 1;
      end if;

    end if;

    return Int;
  exception
    when others =>
      raise Math_Error;
  end Trunc;

  -- power
  function "**" (Number, Exponent : Real) return Real is
  begin
   return Real(Cpow (Double(Number), Double(Exponent)));
  exception
    when others =>
      raise Math_Error;
  end "**";

  -- square root
  function Sqrt (X : Real) return Real is
  begin
    if X < 0.0 then
      raise Math_Error;
    end if;
    return Real(Csqrt (Double(X)));
  exception
    when others =>
      raise Math_Error;
  end Sqrt;

  -- Based 10 log
  function Log_10 (X : Real) return Real is
  begin
    if X < 0.0 then
      raise Math_Error;
    end if;
    return Real(Clog10 (Double(X)));
  exception
    when others =>
      raise Math_Error;
  end Log_10;

  function Exp (X : Real := 1.0) return Real is
  begin
    return E ** X;
  end Exp;

  function Ln (X : Real) return Real is
  begin
    if X < 0.0 then
      raise Math_Error;
    end if;
    return Real(Clog (Double(X)));
  exception
    when others =>
      raise Math_Error;
  end Ln;

  function Sin (X    : Real;
                Mode : Angle_Unit := Radian) return Real is
    Y      : Real;
  begin
    if Mode = Radian then
      Y := X;
    else
      Y := X * Pi_Hundred_Heighty;
    end if;
    return Real(Csin (Double(Y)));
  exception
    when others =>
      raise Math_Error;
  end Sin;

  function Cos (X    : Real;
                Mode : Angle_Unit := Radian) return Real is
    Y      : Real;
  begin
    if Mode = Radian then
      Y := X;
    else
      Y := X * Pi_Hundred_Heighty;
    end if;
    return Real(Ccos (Double(Y)));
  exception
    when others =>
      raise Math_Error;
  end Cos;

  function Tg (X    : Real;
               Mode : Angle_Unit := Radian) return Real is
    Y       : Real;
  begin
    if Mode = Radian then
      Y := X;
    else
      Y := X * Pi_Hundred_Heighty;
    end if;
    return Real(Ctan (Double(Y)));
  exception
    when others =>
      raise Math_Error;
  end Tg;

  function Arc_Sin (X    : Real;
                    Mode : Angle_Unit := Radian) return Real is
    Y : Real;
  begin
    if abs (X) > 1.0 then
      raise Math_Error;
    end if;
    Y := Real(Casin (Double(X)));
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
    Y   : Real;
  begin
    if abs (X) > 1.0 then
      raise Math_Error;
    end if;
    Y := Real(Cacos (Double(X)));
    if Mode = Degree then
      Y := Y / Pi_Hundred_Heighty;
    end if;
    return Y;
  exception
    when others =>
      raise Math_Error;
  end Arc_Cos;

  function Arc_Tg (X    : Real;
                   Mode : Angle_Unit := Radian) return Real is
    Y       : Real;
  begin
    Y := Real(Catan (Double(X)));
    if Mode = Degree then
      Y := Y / Pi_Hundred_Heighty;
    end if;
    return Y;
  exception
    when others =>
      raise Math_Error;
  end Arc_Tg;

end My_Math;

