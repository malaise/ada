with Ada.Numerics;
package My_Math is

  subtype Inte is Long_Long_Integer;
  -- This should generate the overflow checks
  --  (was subtype Real is Long_Float;)
  type Real is digits 15 range -1.5E308 .. 1.5E308;

  -- Types and constants for trigonometry
  -- pi constant
  Pi : constant := Ada.Numerics.Pi;
  -- e constant
  E : constant := Ada.Numerics.E;
  -- possible units for trigonometric constants 
  type Angle_Unit is (Radian, Degree);

  -- Integer part and fractional part of a real
  -- R := Int(R) + Frac(R)
  function Int  (X : Real) return Real;
  function Frac (X : Real) return Real;

  -- From real to int : round or trunc
  function Round (X : Real) return Inte;
  function Trunc (X : Real) return Inte;

  -- Power and sqrt
  function "**" (Number, Exponent : Real) return Real;
  function Sqrt (X : Real) return Real;

  -- 10 based Log
  function Log_10 (X : Real) return Real;

  -- Logarithmic functions based on E
  function Exp (X : Real := 1.0) return Real;
  function Ln  (X : Real) return Real;

  -- Trigonometric functions
  function Sin (X : Real; Mode : Angle_Unit := Radian) return Real;
  function Cos (X : Real; Mode : Angle_Unit := Radian) return Real;
  function Tg  (X : Real; Mode : Angle_Unit := Radian) return Real;

  -- Invers trigonometric functions
  -- arc sinus returned on -pi/2 .. pi/2
  function Arc_Sin (X : Real; Mode : Angle_Unit := Radian) return Real;
  -- arc cosinus returned on 0 .. pi
  function Arc_Cos (X : Real; Mode : Angle_Unit := Radian) return Real;
  -- arc tangente returned on -pi/2 .. pi/2
  function Arc_Tg  (X : Real; Mode : Angle_Unit := Radian) return Real;

  -- exception raised in case of error
  Math_Error : exception;

end My_Math;
