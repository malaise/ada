with Ada.Strings.Unbounded;
package Arbitrary is
  type Number is private;

  -- Constructor
  function Set (V : String) return Number;
  function Set (V : Integer) return Number;
  function Set (V : Long_Integer) return Number;
  function Set (V : Long_Long_Integer) return Number;

  -- Image
  function Image (V : Number) return String;

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
  Len_Error : exception;
  function "+" (A, B : Number) return Number;
  function "-" (A, B : Number) return Number;

private
  type Number is new Ada.Strings.Unbounded.Unbounded_String;
end Arbitrary;

