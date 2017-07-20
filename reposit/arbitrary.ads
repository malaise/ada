-- Arbitrary precision numbers
with Long_Longs;
private with As.U;
package Arbitrary is
  type Number is tagged private;

  -- Constructor
  function Set (V : String) return Number;
  procedure Set (N: out Number; V : in String);
  function Set (V : Integer) return Number;
  procedure Set (N: out Number; V : in Integer);
  function Set (V : Long_Integer) return Number;
  procedure Set (N: out Number; V : in Long_Integer);
  function Set (V : Long_Longs.Ll_Integer) return Number;
  procedure Set (N: out Number; V : in Long_Longs.Ll_Integer);

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
  overriding function "=" (A, B : Number) return Boolean;
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
  function Roundiv (A, B : Number) return Number;
  function "**" (A, B : Number) return Number;
  procedure Sqrt (A : in Number; S, R : out Number);
  function Sqrt (A : Number) return Number;

  -- Digit extraction. Sign is not taken into account
  subtype Digit is Natural range 0 .. 9;
  function Nb_Digits (A : Number) return Positive;
  function Nth_Digit (A : Number; N : Positive) return Digit;
  function Last_Digit (A : Number) return Digit;

private
  type Number is new As.U.Asu_Us with null record;
end Arbitrary;

