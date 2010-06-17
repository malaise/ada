package Arbitrary.Fractions is

  type Fraction is private;

  -- Constructor
  function Set (Numerator, Denominator : Number) return Fraction;

  -- Image: "Numerator:Denominator"
  function Image (F : Fraction) return String;

  -- Basic "constants"
  function Zero return Fraction;
  function One  return Fraction;
  function Two  return Fraction;

  -- Extractors
  function Numerator (F : Fraction) return Number;
  function Denominator (F : Fraction) return Number;

  -- Basic unitary operations
  function "abs" (A : Fraction) return Fraction;
  function "-" (A : Fraction) return Fraction;

  -- Basic comparisons
  function "=" (A, B : Fraction) return Boolean;
  function "<" (A, B : Fraction) return Boolean;
  function "<=" (A, B : Fraction) return Boolean;
  function ">" (A, B : Fraction) return Boolean;
  function ">=" (A, B : Fraction) return Boolean;

  -- Basic operations
  function "+" (A, B : Fraction) return Fraction;
  function "-" (A, B : Fraction) return Fraction;
  function "*" (A, B : Fraction) return Fraction;
  function "/" (A, B : Fraction) return Fraction;
  function "**" (A : Fraction; B : Number) return Fraction;

private

  -- Always reduced
  -- Sign is always on numerator
  type Fraction is record
    Numerator   : Number;
    Denominator : Number;
  end record;

end Arbitrary.Fractions;

