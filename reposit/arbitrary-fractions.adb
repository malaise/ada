with Prime_List, Dynamic_List;
with Arbitrary.Factors;
package body Arbitrary.Fractions is

  package Nb_List_Mng renames Arbitrary.Factors.Nb_List_Mng;

  -- Reduce the fraction
  procedure Reduce (F : in out Fraction) is
    N, D, T, Hcd : Number;
    Ln, Ld, Lc : Nb_List_Mng.List_Type;
  begin
    -- Sanity check and conventional Zero.
    if F.Denominator = Zero then
      raise Constraint_Error;
    end if;
    if F.Numerator = Zero then
      F := Zero;
      return;
    end if;

    -- Fix sign of numerator if denominator < 0
    if F.Denominator < Zero then
      F.Numerator := - F.Numerator;
      F.Denominator := - F.Denominator;
    end if;

    -- Extract abs values and decompose them in prime factors
    N := abs F.Numerator;
    Arbitrary.Factors.Decompose (N, Ln);
    D := F.Denominator;
    Arbitrary.Factors.Decompose (D, Ld);

    -- Extract common factors and compute
    --  highest common denominator
    Arbitrary.Factors.Extract_Common (Ln, Ld, Lc);
    Hcd := Arbitrary.Factors.Multiply (Lc);

    -- Divide numerator and denominator by Hcd
    if Hcd /= One then
      F.Numerator := F.Numerator / Hcd;
      F.Denominator := F.Denominator / Hcd;
    end if;

  end Reduce;

  -- Constructor
  function Set (Numerator, Denominator : Number) return Fraction is
    F : Fraction := (Numerator, Denominator);
  begin
    Reduce (F);
    return F;
  end Set;

  -- Image
  function Image (F : Fraction) return String is
   Str : constant String := Image (F.Denominator);
  begin
    return Image (F.Numerator) & ":" & Str (2 .. Str'Last);
  end Image;

  -- Basic "constants"
  function Zero return Fraction is
  begin
    return (Zero, One);
  end Zero;

  function One  return Fraction is
  begin
    return (One, One);
  end One;


  -- Extractors
  function Numerator (F : Fraction) return Number is
  begin
    return F.Numerator;
  end Numerator;

  function Denominator (F : Fraction) return Number is
  begin
    return F.Denominator;
  end Denominator;


  -- Basic unitary operations
  function "abs" (A : Fraction) return Fraction is
  begin
    return (abs A.Numerator, A.Denominator);
  end "abs";

  function "-" (A : Fraction) return Fraction is
  begin
    return (- A.Numerator, A.Denominator);
  end "-";


  -- Basic comparisons
  function "=" (A, B : Fraction) return Boolean is
  begin
    return A.Numerator = B.Numerator
    and then A.Denominator = B.Denominator;
  end "=";

  function "<" (A, B : Fraction) return Boolean is
    La, Lb, Lc : Nb_List_Mng.List_Type;
    Rda, Rdb : Number;
  begin
    -- Decompose denominators in prime factors
    --  and compute reduced denominators
    Arbitrary.Factors.Decompose (A.Denominator, La);
    Arbitrary.Factors.Decompose (B.Denominator, Lb);
    Arbitrary.Factors.Extract_Common (La, Lb, Lc);
    Rda := Arbitrary.Factors.Multiply (La);
    Rdb := Arbitrary.Factors.Multiply (Lb);
    return A.Numerator * Rdb < B.Numerator * Rda;
  end "<";

  function "<=" (A, B : Fraction) return Boolean is
  begin
    return A = B or else A < B;
  end "<=";

  function ">" (A, B : Fraction) return Boolean is
    La, Lb, Lc : Nb_List_Mng.List_Type;
    Rda, Rdb : Number;
  begin
    -- Decompose denominators in prime factors
    --  and compute reduced denominators
    Arbitrary.Factors.Decompose (A.Denominator, La);
    Arbitrary.Factors.Decompose (B.Denominator, Lb);
    Arbitrary.Factors.Extract_Common (La, Lb, Lc);
    Rda := Arbitrary.Factors.Multiply (La);
    Rdb := Arbitrary.Factors.Multiply (Lb);
    return A.Numerator * Rdb > B.Numerator * Rda;
  end ">";

  function ">=" (A, B : Fraction) return Boolean is
  begin
    return A = B or else A > B;
  end ">=";


  -- Basic operations
  function "+" (A, B : Fraction) return Fraction is
    La, Lb, Lc : Nb_List_Mng.List_Type;
    Rda, Rdb : Number;
    R : Fraction;
  begin
    -- Decompose denominators in prime factors
    --  and compute reduced denominators
    Arbitrary.Factors.Decompose (A.Denominator, La);
    Arbitrary.Factors.Decompose (B.Denominator, Lb);
    Arbitrary.Factors.Extract_Common (La, Lb, Lc);
    Rda := Arbitrary.Factors.Multiply (La);
    Rdb := Arbitrary.Factors.Multiply (Lb);
    -- R.Den := Lowest common multiple
    R.Denominator := Rda * Rdb * Arbitrary.Factors.Multiply (Lc);
    -- R.Num := A.Num * B.Den + B.Num * A.Den
    R.Numerator := A.Numerator * Rdb + B.Numerator * Rda;
    -- Reduce
    Reduce (R);
    return R;
  end "+";

  function "-" (A, B : Fraction) return Fraction is
  begin
    return A + (-B);
  end "-";

  function "*" (A, B : Fraction) return Fraction is
    F1, F2, R : Fraction;
  begin
    -- Build A.Num/B.Denom and B.Num/A.denom and reduce them
    --  and multiply them
    F1 := (A.Numerator, B.Denominator);
    F2 := (B.Numerator, A.Denominator);
    R := (F1.Numerator * F2.Numerator,
          F1.Denominator * F2.Denominator);
    Reduce (R);
    return R;
  end "*";

  function Inverse (F : Fraction) return Fraction is
  begin
    if F.Numerator >= Zero then
      return  (F.Denominator, F.Numerator);
    else
      -- Sign is always on numerator
      return (-F.Denominator, -F.Numerator);
    end if;
  end Inverse;

  function "/" (A, B : Fraction) return Fraction is
  begin
    return A * Inverse (B);
  end "/";

  function "**" (A : Fraction; B : Number) return Fraction is
    R : Fraction;
  begin
    if B = Zero then
      return One;
    elsif B < Zero then
      return (Inverse (A) ** (-B));
    else
      R := (A.Numerator ** B,
            A.Denominator ** B);
      Reduce (R);
      return R;
    end if;
  end "**";

end Arbitrary.Fractions;

