-- Arbitrary precision numbers
with Ada.Strings.Unbounded;
package Arbitrary is
  type Number is private;

  -- Constructor
  function Set (V : String) return Number;
  function Set (V : Integer) return Number;
  function Set (V : Long_Integer) return Number;
  function Set (V : Long_Long_Integer) return Number;

  -- Basic "constants"
  function Zero return Number;
  function One  return Number;

  -- Image
  function Image (V : Number) return String;
  function Length (V : Number) return Natural;

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
  function Sqrt (A : Number) return Number;

private
  type Number is new Ada.Strings.Unbounded.Unbounded_String;
end Arbitrary;

