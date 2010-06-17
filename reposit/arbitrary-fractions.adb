package body Arbitrary.Fractions is

  -- Extract the highest common denominator of two (positive) numbers
  function Hcd (A, B : Number) return Number is
    N1, N2, N3 : Number;
  begin
    N1 := A;
    N2 := B;
    -- Divide N1 by N2 as long as N3 /= N2 and shift
    loop
      N3 := N1 rem N2;
      exit when N3 = Zero;
      N1 := N2;
      N2 := N3;
    end loop;
    return N2;
  end Hcd;

  -- Reduce the fraction
  procedure Reduce (F : in out Fraction) is
    H : Number;
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

    -- Compute Hcd
    H := Hcd (abs F.Numerator, F.Denominator);

    -- Divide numerator and denominator by Hcd
    if H /= One then
      F.Numerator := F.Numerator / H;
      F.Denominator := F.Denominator / H;
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

  function Two  return Fraction is
  begin
    return (Two, One);
  end Two;


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
    H, Rda, Rdb : Number;
  begin
    -- Compute Hcd of denominators and reduce them
    H := Hcd (A.Denominator, B.Denominator);
    Rda := A.Denominator / H;
    Rdb := B.Denominator / H;
    -- Compare
    return A.Numerator * Rdb < B.Numerator * Rda;
  end "<";

  function "<=" (A, B : Fraction) return Boolean is
  begin
    return A = B or else A < B;
  end "<=";

  function ">" (A, B : Fraction) return Boolean is
    H, Rda, Rdb : Number;
  begin
    -- Compute Hcd of denominators and reduce them
    H := Hcd (A.Denominator, B.Denominator);
    Rda := A.Denominator / H;
    Rdb := B.Denominator / H;
    -- Compare
    return A.Numerator * Rdb > B.Numerator * Rda;
  end ">";

  function ">=" (A, B : Fraction) return Boolean is
  begin
    return A = B or else A > B;
  end ">=";


  -- Basic operations
  function "+" (A, B : Fraction) return Fraction is
    H, Rda, Rdb : Number;
    R : Fraction;
  begin
    -- Compute Hcd of denominators and reduce them
    H := Hcd (A.Denominator, B.Denominator);
    Rda := A.Denominator / H;
    Rdb := B.Denominator / H;
    -- R.Den := Lowest common multiple
    R.Denominator := A.Denominator / H * B.Denominator;
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

