package body C_Nbres is

  use My_Math;

  -- ANGLES en RADIAN
  package Polar is

    type Polar is record
      Module   : Real;
      Argument : Real;
    end record;

    function To_Polar   (X : Complex) return Polar;
    function To_Complex (X : Polar)   return Complex;

  end Polar;

  package body Polar is

    function To_Polar (X : Complex) return Polar is
      X_Pol : Polar;
    begin
      X_Pol.Module := Sqrt
       (X.Part_Real * X.Part_Real + X.Part_Imag * X.Part_Imag);
      if (X.Part_Real /= 0.0) then
        X_Pol.Argument := Arc_Tg (X.Part_Imag / X.Part_Real);
      else
        X_Pol.Argument := Pi / 2.0;
      end if;
      if X.Part_Real < 0.0 then
        X_Pol.Argument := X_Pol.Argument + Pi;
      end if;
      return (X_Pol);
    end To_Polar;

    function To_Complex (X : Polar)  return Complex is
      X_Cart : Complex;
    begin
      X_Cart.Part_Real := X.Module * Cos (X.Argument);
      X_Cart.Part_Imag := X.Module * Sin (X.Argument);
      return (X_Cart);
    end To_Complex;

  end Polar;


  function Reduct (A : Radian) return Reducted_Radian is
    N : Inte;
    Two_Pi : constant Radian := Reducted_Radian'Last;
    R : Radian;
  begin
    N := Trunc (Real(A / (Two_Pi)));
    R := A - Radian(N) * Radian(Two_Pi);
    if R < 0.0 then R := R + Two_Pi; end if;
    return R;
  end Reduct;

  function Reduct (A : Degree)  return Reducted_Degree is
    N : Inte;
    One_Circle : constant Degree := Reducted_Degree'Last;
    R : Degree;
  begin
    N := Trunc (Real(A / (One_Circle)));
    R := A - Degree(N) * Degree(One_Circle);
    if R < 0.0 then R := R + One_Circle; end if;
    return R;
  end Reduct;

  function To_Degree (A : Radian) return Reducted_Degree is
    R : Real;
  begin
    R := Real(Reduct(A)/Reducted_Radian'Last) * Real(Reducted_Degree'Last);
    return Reducted_Degree (R);
  end To_Degree;

  function To_Radian (A : Degree) return Reducted_Radian is
    R : Real;
  begin
    R := Real(Reduct(A)/Reducted_Degree'Last) * Real(Reducted_Radian'Last);
    return Reducted_Radian (R);
  end To_Radian;


  function Create_Complex (M : Typ_Module; A : Radian) return Complex is
  begin
    return Polar.To_Complex (
     (Module => Real(M), Argument => Real(Reduct(A))));
  end Create_Complex;

  function Create_Complex (M : Typ_Module; A : Degree) return Complex is
  begin
    return Polar.To_Complex (
     (Module => Real(M), Argument => Real(To_Radian(A))) );
  end Create_Complex;

  function Module (C : Complex) return Typ_Module is
    M : Real := Polar.To_Polar (C).Module;
  begin
    return Typ_Module (abs(M));
  end Module;

  function Angle_Radian (C : Complex) return Reducted_Radian is
  begin
    return Reduct(Radian(Polar.To_Polar(C).Argument));
  end Angle_Radian;

  function Angle_Degree (C : Complex) return Reducted_Degree is
  begin
    return Reduct (To_Degree(Angle_Radian(C)));
  end Angle_Degree;



  function Part_Real (C : Complex) return Real is
  begin
    return C.Part_Real;
  end Part_Real;

  function Part_Imag (C : Complex) return Real is
  begin
    return C.Part_Imag;
  end Part_Imag;

  function Create_Complex (Real_Part, Imag_Part : Real) return Complex is
  begin
    return (Part_Real => Real_Part, Part_Imag => Imag_Part);
  end Create_Complex;

  function Create_Complex (Real_Part : Real) return Complex is
  begin
    return (Part_Real => Real_Part, Part_Imag => 0.0);
  end Create_Complex;

  -----------------------------------------------------------------------------

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
    Module_Y : Real := Y.Part_Real * Y.Part_Real
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
    Module_Y : Real := Y.Part_Real * Y.Part_Real
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
    X_Polar : Polar.Polar;
    Res_Complex : Complex;
  begin
    X_Polar := Polar.To_Polar (X);
    X_Polar := (Module   => X_Polar.Module ** Y,
                Argument => X_Polar.Argument *  Y);
    Res_Complex := Polar.To_Complex (X_Polar);
    return Res_Complex;
  end "**";

  -------------------------------------------------------------------------------

  use My_Real_Io;

  procedure Put (C : in Complex) is
    Imag_Part : Real := Part_Imag (C);
  begin
    Put (Part_Real (C) );
    if (Imag_Part >= 0.0) then
      Text_Io.Put (" +");
    else
      Text_Io.Put (" -");
    end if;
    Put ( abs (Imag_Part) );
    Text_Io.Put (" * i ");
  end Put;

  procedure Get (C : out Complex) is
    Real_Part, Imag_Part : Real;
  begin
    Text_Io.Put ("Reel? ");
    Get (Real_Part);
    Text_Io.Put ("Imag? ");
    Get (Imag_Part);
    C := Create_Complex (Real_Part, Imag_Part);
  end Get;


end C_Nbres;
