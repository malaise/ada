with ADA.NUMERICS;
package MATH is

  subtype INTE is LONG_LONG_INTEGER;
  subtype REAL is LONG_FLOAT;

  -- Types and constants for trigonometry
  -- pi constant
  PI : constant := ADA.NUMERICS.PI;
  -- e constant
  E : constant := ADA.NUMERICS.E;
  -- possible units for trigonometric constants 
  type ANGLE_UNIT is (RADIAN, DEGREE);

  -- Integer part and fractional part of a real
  function INT  (X : REAL) return REAL;
  function FRAC (X : REAL) return REAL;

  -- From real to int : round or trunc
  function ROUND (X : REAL) return INTE;
  function TRUNC (X : REAL) return INTE;

  -- Power and sqrt
  function "**" (NUMBER, EXPONENT : REAL) return REAL;
  function SQRT (X : REAL) return REAL;

  -- 10 based Log
  function LOG_10 (X : REAL) return REAL;

  -- Logarithmic functions based on E
  function EXP (X : REAL := 1.0) return REAL;
  function LN  (X : REAL) return REAL;

  -- Trigonometric functions
  function SIN (X : REAL; MODE : ANGLE_UNIT := RADIAN) return REAL;
  function COS (X : REAL; MODE : ANGLE_UNIT := RADIAN) return REAL;
  function TG  (X : REAL; MODE : ANGLE_UNIT := RADIAN) return REAL;

  -- Invers trigonometric functions
  -- arc sinus returned on -pi/2 .. pi/2
  function ARC_SIN (X : REAL; MODE : ANGLE_UNIT := RADIAN) return REAL;
  -- arc cosinus returned on 0 .. pi
  function ARC_COS (X : REAL; MODE : ANGLE_UNIT := RADIAN) return REAL;
  -- arc tangente returned on -pi/2 .. pi/2
  function ARC_TG  (X : REAL; MODE : ANGLE_UNIT := RADIAN) return REAL;

  -- exception raised in case of error
  MATH_ERROR : exception;

end MATH;
