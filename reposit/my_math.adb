with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_Io;
package body My_Math is


  package Math is new Ada.Numerics.Generic_Elementary_Functions (Real);



  -- Constants for computing
  --------------------------
  -- greatest number that can be exponented
  Ln_Max               : Real;
  -- ln (10)
  Ln_10                : Real;
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
   return Math."**" (Number, Exponent);
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
    return Math.Sqrt (X);
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
    return Ln(X)/Ln_10;
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
    return Math.Log (X);
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
    return Math.Sin (Y);
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
    return Math.Cos (Y);
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
    return Math.Tan (Y);
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
    Y := Math.Arcsin (X);
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
    Y := Math.Arccos (X);
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
    Y := Math.Arctan (X);
    if Mode = Degree then
      Y := Y / Pi_Hundred_Heighty;
    end if;
    return Y;
  exception
    when others =>
      raise Math_Error;
  end Arc_Tg;

begin
  Ln_Max := Ln(Real'Large);
  Ln_10 := Ln(10.0);
end My_Math;
