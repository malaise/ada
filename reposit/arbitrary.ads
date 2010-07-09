-- Arbitrary precision numbers
with As.U; use As.U;
package Arbitrary is
  type Number is private;

  -- Constructor
  function Set (V : String) return Number;
  function Set (V : Integer) return Number;
  function Set (V : Long_Integer) return Number;
  function Set (V : Long_Long_Integer) return Number;
  function Is_Set (V : Number) return Boolean;

  -- Basic "constants"
  function Zero return Number;
  function One  return Number;
  function Two  return Number;

  -- Image: +xxx or -xxx
  function Image (V : Number) return String;
  function Length (V : Number) return Natural;

  -- Is a Number positive (True for 0)
  function Is_Positive (V : Number) return Boolean;

  -- Basic unitary operations
  function "abs" (A : Number) return Number;
  function "-" (A : Number) return Number;

  -- Basic comparisons
  function "=" (A, B : Number) return Boolean;
  function "<" (A, B : Number) return Boolean;
  function "<=" (A, B : Number) return Boolean;
  function ">" (A, B : Number) return Boolean;
  function ">=" (A, B : Number) return Boolean;

  -- Basic operations
  function "+" (A, B : Number) return Number;
  function "-" (A, B : Number) return Number;
  function "*" (A, B : Number) return Number;
  function "/" (A, B : Number) return Number;
  function "rem" (A, B : Number) return Number;
  function "mod" (A, B : Number) return Number;

  -- Less basic operations
  procedure Div (A, B : in Number; Q, R : out Number);
  function "**" (A, B : Number) return Number;
  procedure Sqrt (A : in Number; S, R : out Number);
  function Sqrt (A : Number) return Number;

  -- Digit extraction. Sign is not taken into account
  subtype Digit is Natural range 0 .. 9;
  function Nb_Digits (A : Number) return Positive;
  function Nth_Digit (A : Number; N : Positive) return Digit;
  function Last_Digit (A : Number) return Digit;

private
  type Number is new Asu_Us;
end Arbitrary;

