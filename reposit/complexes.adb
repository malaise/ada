with Normalization;
package body Complexes is

  use My_Math;

  ------------------
  -- COMPLEX TYPE --
  ------------------
  function Create_Complex (Real_Part, Imag_Part : Real) return Complex is
  begin
    return (Part_Real => Real_Part, Part_Imag => Imag_Part);
  end Create_Complex;

  function Create_Complex (Real_Part : Real) return Complex is
  begin
    return (Part_Real => Real_Part, Part_Imag => 0.0);
  end Create_Complex;

  function Part_Real (C : Complex) return Real is
  begin
    return C.Part_Real;
  end Part_Real;

  function Part_Imag (C : Complex) return Real is
  begin
    return C.Part_Imag;
  end Part_Imag;


  ---------------------
  -- TYPES FOR POLAR --
  ---------------------
  function Reduct (A : Radian) return Reducted_Radian is
    N : Inte;
    Two_Pi : constant Radian := Reducted_Radian'Last;
    R : Radian;
  begin
    N := Trunc (Real(A / (Two_Pi)));
    R := A - Radian(N) * Two_Pi;
    if R < 0.0 then R := R + Two_Pi; end if;
    return R;
  end Reduct;

  function Reduct (A : Degree)  return Reducted_Degree is
    N : Inte;
    One_Circle : constant Degree := Reducted_Degree'Last;
    R : Degree;
  begin
    N := Trunc (Real(A / One_Circle));
    R := A - Degree(N) * One_Circle;
    if R < 0.0 then R := R + One_Circle; end if;
    return R;
  end Reduct;

  Rounding : constant := -6;
  function To_Degree (A : Radian) return Reducted_Degree is
    R : Real;
  begin
    R := Real(Reduct(A)/Reducted_Radian'Last) * Real(Reducted_Degree'Last);
    R := Round_At (R, Rounding);
    return Reducted_Degree (R);
  end To_Degree;

  function To_Radian (A : Degree) return Reducted_Radian is
    R : Real;
  begin
    R := Real(Reduct(A)/Reducted_Degree'Last) * Real(Reducted_Radian'Last);
    return Reducted_Radian (R);
  end To_Radian;


  --------------------------
  -- POLAR REPRESENTATION --
  --------------------------
  function Create_Polar (M : Typ_Module; A : Radian) return Polar is
  begin
    return (Module => M, Argument => Reduct(A));
  end Create_Polar;

  function Create_Polar (M : Typ_Module; A : Degree) return Polar is
  begin
    return (Module => M, Argument => To_Radian(A));
  end Create_Polar;

  function Module (P : Polar) return Typ_Module is
  begin
    return P.Module;
  end Module;

  function Angle_Radian (P : Polar) return Reducted_Radian is
  begin
    return P.Argument;
  end Angle_Radian;

  function Angle_Degree (P : Polar) return Reducted_Degree is
  begin
    return To_Degree (P.Argument);
  end Angle_Degree;


  ----------------
  -- CONVERSION --
  ----------------
  function To_Polar (C : Complex) return Polar is
    X_Pol : Polar;
  begin
    X_Pol.Module := Sqrt
     (C.Part_Real * C.Part_Real + C.Part_Imag * C.Part_Imag);
    X_Pol.Argument := (if C.Part_Real /= 0.0 then
                         Reduct (Radian(Arc_Tan (C.Part_Imag / C.Part_Real)))
                       else Pi / 2.0);
    if C.Part_Real < 0.0 then
      X_Pol.Argument := X_Pol.Argument + Pi;
    end if;
    return (X_Pol);
  end To_Polar;

  function To_Complex (P : Polar)  return Complex is
    X_Cart : Complex;
  begin
    X_Cart.Part_Real := Real(P.Module) * Cos (Real(P.Argument));
    X_Cart.Part_Imag := Real(P.Module) * Sin (Real(P.Argument));
    return (X_Cart);
  end To_Complex;


  ----------------
  -- OPERATIONS --
  ----------------
  function "+" (X, Y : Complex) return Complex is
  begin
    return (Part_Real => X.Part_Real + Y.Part_Real,
            Part_Imag => X.Part_Imag + Y.Part_Imag);
  end "+";

  function "+" (X : Real; Y : Complex) return Complex is
  begin
    return (Part_Real => X   + Y.Part_Real,
            Part_Imag =>       Y.Part_Imag);
  end "+";

  function "+" (X : Complex; Y : Real) return Complex is
  begin
    return (Part_Real => X.Part_Real + Y,
            Part_Imag => X.Part_Imag);
  end "+";

  function "+" (X : Real; Y : Real) return Complex is
  begin
    return (Part_Real => X + Y,
            Part_Imag => 0.0);
  end "+";

  -----------------------------------------------------------------------------

  function "-" (X, Y : Complex) return Complex is
  begin
    return (Part_Real => X.Part_Real - Y.Part_Real,
            Part_Imag => X.Part_Imag - Y.Part_Imag);
  end "-";

  function "-" (X : Real; Y : Complex) return Complex is
  begin
    return (Part_Real => X   - Y.Part_Real,
            Part_Imag =>     - Y.Part_Imag);
  end "-";

  function "-" (X : Complex; Y : Real) return Complex is
  begin
    return (Part_Real => X.Part_Real - Y,
            Part_Imag => X.Part_Imag);
  end "-";

  function "-" (X : Real; Y : Real) return Complex is
  begin
    return (Part_Real => X - Y,
            Part_Imag => 0.0);
  end "-";

  -----------------------------------------------------------------------------

  function "*" (X, Y : Complex) return Complex is
  begin
    return (Part_Real => X.Part_Real * Y.Part_Real
                       - X.Part_Imag * Y.Part_Imag,
            Part_Imag => X.Part_Real * Y.Part_Imag
                       + X.Part_Imag * Y.Part_Real);
  end "*";

  function "*" (X : Real; Y : Complex) return Complex is
  begin
    return (Part_Real => X * Y.Part_Real,
            Part_Imag => X * Y.Part_Imag);
  end "*";

  function "*" (X : Complex; Y : Real) return Complex is
  begin
    return (Part_Real => X.Part_Real * Y,
            Part_Imag => X.Part_Imag * Y);
  end "*";

  function "*" (X : Real; Y : Real) return Complex is
  begin
    return (Part_Real => X * Y,
            Part_Imag => 0.0);
  end "*";

  -----------------------------------------------------------------------------

  function "/" (X, Y : Complex) return Complex is
    Module_Y : constant Real := Y.Part_Real * Y.Part_Real
                              + Y.Part_Imag * Y.Part_Imag;
  begin
    return (Part_Real =>
                ( (X.Part_Real * Y.Part_Real) + (X.Part_Imag * Y.Part_Imag) )
                / Module_Y ,
      Part_Imag =>
                ( (X.Part_Imag * Y.Part_Real) - (X.Part_Real * Y.Part_Imag) )
                / Module_Y );
  end "/";

  function "/" (X : Real; Y : Complex) return Complex is
    Module_Y : constant Real := Y.Part_Real * Y.Part_Real
                              + Y.Part_Imag * Y.Part_Imag;
  begin
    return (Part_Real => (  X * Y.Part_Real / Module_Y),
            Part_Imag => (- X * Y.Part_Imag / Module_Y) );
  end "/";

  function "/" (X : Complex; Y : Real) return Complex is
  begin
    return (Part_Real => X.Part_Real / Y,
            Part_Imag => X.Part_Imag / Y);
  end "/";

  function "/" (X : Real; Y : Real) return Complex is
  begin
    return (Part_Real => X / Y,
            Part_Imag => 0.0);
  end "/";

  -----------------------------------------------------------------------------

  function "**" (X : Complex; Y : Real) return Complex is
    X_Polar : Polar;
    Res_Complex : Complex;
  begin
    X_Polar := To_Polar (X);
    X_Polar := (Module   => X_Polar.Module ** Y,
                Argument => Reduct (X_Polar.Argument * Radian(Y)) );
    Res_Complex := To_Complex (X_Polar);
    return Res_Complex;
  end "**";

  -------------------------------------------------------------------------------

  -- Width of Real
  Str_Width : constant Positive := My_Math.Real'Width;
  Exp : constant := 2;
  subtype Image_Str is String (1 .. Str_Width);

  function Put (C : Complex) return String is
    Imag_Part : constant Real := Part_Imag (C);
    Real_Str, Imag_Str : Image_Str;
    Sign : Character;
  begin
    Real_Str := Normalization.Normal_Digits (Part_Real (C), Str_Width, Exp);
    Sign := (if Imag_Part >= 0.0 then '+' else '-');
    Imag_Str := Normalization.Normal_Digits (abs (Imag_Part), Str_Width, Exp);
    return Real_Str & ' ' & Sign & " i *" & Imag_Str;
  end Put;

  function Get (X, Y : String) return Complex is
    Real_Part, Imag_Part : Real;
    C : Complex;
  begin
    Real_Part := My_Math.Get (X);
    Imag_Part := My_Math.Get (Y);
    C := Create_Complex (Real_Part, Imag_Part);
    return C;
  end Get;

end Complexes;
