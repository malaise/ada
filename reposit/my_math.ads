with Ada.Numerics;
with Long_Longs;
-- Mathematical types, constants and operations
package My_Math is

  subtype Inte is Long_Longs.Ll_Integer;
  -- This should generate the overflow checks
  --  (was subtype Real is Long_Float;)
  type Real is digits 15 range -1.79E308 .. 1.79E308;

  -- Types and constants for trigonometry
  -- pi constant
  Pi : constant := Ada.Numerics.Pi;
  -- e constant
  E : constant := Ada.Numerics.E;
  -- Possible units for trigonometric constants
  type Angle_Unit is (Radian, Degree);

  -- Get an Inte, get a Real from a string
  -- No leading or trailing separator allowed
  -- raise Data_Error if From is not valid
  Data_Error : exception;
  function Get (From : String) return Inte;
  function Get (From : String) return Real;

  -- Integer part and fractional part of a real
  -- R := Int(R) + Frac(R)
  function Int  (X : Real) return Real;
  function Frac (X : Real) return Real;

  -- From real to int : round or trunc
  function Round (X : Real) return Inte;
  function Trunc (X : Real) return Inte;

  -- Divide A by B and return the rounded result
  function Roundiv (A, B : Inte) return Inte;

  -- Round R at N digits.
  -- If N is positive then it applies to the int part
  -- else it applies to the frac part.
  -- Ex R=990.2 N=2 -> 1000
  --    R=5.118 N=-1 -> 5.1
  --    R=5.118 N=-2 -> 5.12
  function Round_At (X : Real; N : Inte) return Real;

  -- Power and sqrt
  function "**" (Number, Exponent : Real) return Real;
  function Sqrt (X : Real) return Real;

  -- 10 based Log
  function Lg (X : Real) return Real;

  -- Logarithmic functions based on E
  function Exp (X : Real := 1.0) return Real;
  function Ln  (X : Real) return Real;

  -- Trigonometric functions
  function Sin (X : Real; Mode : Angle_Unit := Radian) return Real;
  function Cos (X : Real; Mode : Angle_Unit := Radian) return Real;
  function Tan  (X : Real; Mode : Angle_Unit := Radian) return Real;

  -- Invert trigonometric functions
  -- arc sinus returned on -pi/2 .. pi/2
  function Arc_Sin (X : Real; Mode : Angle_Unit := Radian) return Real;
  -- arc cosinus returned on 0 .. pi
  function Arc_Cos (X : Real; Mode : Angle_Unit := Radian) return Real;
  -- arc tangent returned on -pi/2 .. pi/2
  function Arc_Tan  (X : Real; Mode : Angle_Unit := Radian) return Real;
  -- arc tangent function of two variables on -pi .. pi
  -- Principal value of Arc_Tan (Y/X), using the signs of Y and X to
  --  determine the quadrant of the result
  function Arc_Tan2 (Y, X : Real; Mode : Angle_Unit := Radian) return Real;

  -- Exception raised in case of invalid argument or numeric error
  Math_Error : exception;

end My_Math;

